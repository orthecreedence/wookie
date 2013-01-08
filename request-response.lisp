(in-package :wookie)

(defclass request ()
  ((method :accessor request-method :initarg :method :initform :get)
   (resource :accessor request-resource :initarg :resource :initform "/")
   (plugin-data :accessor request-plugin-data :initarg :plugin-data :initform nil)
   (body-callback :accessor request-body-callback :initarg :body-callback :initform nil)
   (http :accessor request-http :initarg :http :initform nil))
  (:documentation "A class describing a request, passed to every route."))

(defclass response ()
  ((socket :accessor response-socket :initarg :socket :initform nil)
   (headers :accessor response-headers :initarg :headers :initform nil))
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

(defun send-response (response &key (status 200) headers body close)
  "Send a response to an incoming request. Takes :status, :headers, and :body
   keyword arguments, which together form an entire response.

   If :close is T, close the client connection after the response has been
   sent fully.

   At the moment, does *not* support streaming chunked content."
  (run-hooks :response response status headers body)
  (let* ((headers (append (response-headers response) headers))
         (body-enc (when body (babel:string-to-octets body :encoding :utf-8)))
         (headers (if body
                      (append headers (list :content-length (length body-enc)))
                      headers))
         (socket (response-socket response))
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
      (unless (getf headers :server)
        (setf headers (append headers
                              (list :server (if *hide-version*
                                                "Wookie"
                                                (format nil "Wookie (~a)"
                                                        (asdf:component-version (asdf:find-system :wookie))))))))
      ;; write all the headers
      (map-plist headers
                 (lambda (header value)
                   (write-http-line "~a: ~a" (camel-case header) value)))
      ;; finalize headers (closing \r\n)
      (write-http-line "")
      ;; send body if specified
      (when body
        (as:write-socket-data socket body-enc)))
    ;; if we speficied we want to close, do it now
    (when close
      (as:write-socket-data socket nil
        :write-cb (lambda (socket)
                    (as:close-socket socket))))))

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
  (let* ((async-stream (make-instance 'as:async-io-stream :socket (response-socket response)))
         (chunked-stream (chunga:make-chunked-stream async-stream)))
    (setf (chunga:chunked-stream-output-chunking-p chunked-stream) t)
    chunked-stream))

(defun finish-response (chunked-stream &key close)
  "Given the stream passed back from start-response, finalize the response (send
   empty chunk) and close the connection, if specified."
  (force-output chunked-stream)
  (let* ((async-stream (chunga:chunked-stream-stream chunked-stream))
         (socket (as:stream-socket async-stream)))
    (as:write-socket-data socket #(48 13 10 13 10)
      :write-cb (lambda (socket)
                  (when close
                    (as:close-socket socket))))))

