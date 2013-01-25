(in-package :wookie)

(define-condition response-error (wookie-error)
  ((response :initarg :response :reader response-error-response :initform nil))
  (:report (lambda (c s) (format s "Response error: ~a" (response-error-response c))))
  (:documentation "Describes a response error"))

(define-condition response-already-sent (response-error) ()
  (:report (lambda (c s) (format s "Response already sent: ~a" (response-error-response c))))
  (:documentation "Triggered when a response is attempted more than once."))

(defclass request ()
  ((socket :accessor request-socket :initarg :socket :initform nil)
   (method :accessor request-method :initarg :method :initform :get)
   (resource :accessor request-resource :initarg :resource :initform "/")
   (headers :accessor request-headers :initarg :headers :initform nil)
   (uri :accessor request-uri :initarg :url :initform nil)
   (plugin-data :accessor request-plugin-data :initarg :plugin-data :initform nil)
   (body-callback :accessor request-body-callback :initarg :body-callback :initform nil)
   (http :accessor request-http :initarg :http :initform nil)
   (error-handlers :accessor request-error-handlers :initarg :error-handlers :initform nil)
   (error-precedence :accessor request-error-precedence :initarg :error-precedence :initform nil))
  (:documentation "A class describing a request, passed to every route."))

(defclass response ()
  ((headers :accessor response-headers :initarg :headers :initform nil)
   (request :accessor response-request :initarg :request :initform nil)
   (finishedp :accessor response-finished-p :initarg :finishedp :initform nil))
  (:documentation "A class holding information about a response to the client."))

(defmacro with-chunking (request (chunk-data last-chunk-p) &body body)
  "Set up a listener for chunked data in a chunk-enabled router. This macro
   takes a request object, the names of the chunk-data/finishedp arguments
   for the body, and the body form.

   Chunk-data is a byte-array of data received as decoded chunked data comes in
   from the client, and last-chunk-p is a boolean indicating whether the last
   chunk from the request is being sent in."
  `(setf (request-body-callback ,request) (lambda (,chunk-data ,last-chunk-p)
                                            ,@body)))

(defun add-default-headers (headers)
  "Add a number of default headers to a headers plist. If one of the default
   headers is already present, do NOT overwrite it. This allows the app to set
   its own headers that can override the defaults."
  (flet ((prepend-header-if-not-exists (key val)
           (unless (getf headers key)
             (setf headers (append (list key val) headers)))))
    (prepend-header-if-not-exists :date
                                  (local-time:format-timestring
                                    nil
                                    (local-time:now)
                                    :format local-time:+rfc-1123-format+))
    (prepend-header-if-not-exists :server
                                  (if *hide-version*
                                      "Wookie"
                                      (format nil "Wookie (~a)"
                                              (asdf:component-version
                                                (asdf:find-system :wookie))))))
  headers)

(defun send-response (response &key (status 200) headers body close)
  "Send a response to an incoming request. Takes :status, :headers, and :body
   keyword arguments, which together form an entire response.

   If :close is T, close the client connection after the response has been
   sent fully."
  ;; make sure we haven't already responded to this request
  (when (response-finished-p response)
    (error (make-instance 'response-already-sent :response response)))
  
  ;; run the response hooks
  (run-hooks :response-started response (response-request response) status headers body)
  (let* ((headers (append (response-headers response) headers))
         (body-enc (when body (babel:string-to-octets body :encoding :utf-8)))
         (headers (if body
                      (append headers (list :content-length (length body-enc)))
                      headers))
         (request (response-request response))
         (socket (request-socket request))
         (status-text (lookup-status-text status)))
    ;; make writing a single HTTP line a bit less painful
    (flet ((write-http-line (format-str &rest format-args)
             (as:write-socket-data
               socket
               (apply #'format
                      (append (list nil
                                    (concatenate 'string format-str "~c~c"))
                              (append format-args (list #\return #\newline)))))))
      ;; write the status line
      (write-http-line "HTTP/1.1 ~a ~a" status status-text)
      (setf headers (add-default-headers headers))
      ;; write all the headers
      (map-plist headers
                 (lambda (header value)
                   (let ((header-name (camel-case header)))
                     (if (listp value)
                         (dolist (val value)
                           (write-http-line "~a: ~a" header-name val))
                         (write-http-line "~a: ~a" header-name value)))))
      ;; finalize headers (closing \r\n)
      (write-http-line "")
      ;; send body if specified
      (when body
        (as:write-socket-data socket body-enc)))
    ;; if we speficied we want to close, do it now
    (if close
        ;; close the socket once it's done writing
        (as:write-socket-data socket nil
          :write-cb (lambda (socket)
                      (as:close-socket socket)))
        ;; we sent a response, but aren't closing. reset the parser so that if
        ;; another request comes in on the same socket, WE'LL BE READY!!!!11one
        (setup-parser socket))
    (setf (response-finished-p response) t)))

(defun start-response (response &key (status 200) headers)
  "Start a response to the client, but do not specify body content (or close the
   connection). Return a chunked (chunga) stream that can be used to send the
   body content bit by bit until finished by calling finish-response."
  ;; we need to add in our own transfer header, so remove all others
  (dolist (head-list (list headers (response-headers response)))
    (remf head-list :content-length)
    (remf head-list :transfer-encoding))
  (send-response response
                 :status status
                 :headers (append headers
                                  (list :transfer-encoding "chunked")))
  (let* ((request (response-request response))
         (async-stream (make-instance 'as:async-io-stream :socket (request-socket request)))
         (chunked-stream (chunga:make-chunked-stream async-stream)))
    (setf (chunga:chunked-stream-output-chunking-p chunked-stream) t)
    chunked-stream))

(defun finish-response (chunked-stream &key close)
  "Given the stream passed back from start-response, finalize the response (send
   empty chunk) and close the connection, if specified."
  (force-output chunked-stream)
  (let* ((async-stream (chunga:chunked-stream-stream chunked-stream))
         (socket (as:stream-socket async-stream)))
    ;; write empty chunk
    (as:write-socket-data socket #(48 13 10 13 10)  ; "0\r\n\r\n"
      :write-cb (lambda (socket)
                  (when close
                    (as:close-socket socket))))))

(defgeneric add-request-error-handler (request error-type handler)
  (:documentation
    "Adds the given handler to the request's erro handling table. The handler
     will only be fired if the type of the error being triggered is a subtype of
     the given error-type symbol.
     
     An error handler is in the format
       (lambda (event) ...)"))

(defmethod add-request-error-handler ((request request) (error-type symbol) (handler function))
  ;; setup an error table if none exists
  (unless (hash-table-p (request-error-handlers request))
    (setf (request-error-handlers request) (make-hash-table :test #'eq)))
  (let ((precedence (add-error-handler error-type handler :error-table (request-error-handlers request))))
    (setf (request-error-precedence request) precedence)))

