(in-package :wookie)

(define-condition response-error (wookie-error)
  ((response :initarg :response :reader response-error-response :initform nil))
  (:report (lambda (c s) (format s "Response error: ~a" (response-error-response c))))
  (:documentation "Describes a response error"))

(define-condition response-already-sent (response-error) ()
  (:report (lambda (c s) (format s "Response already sent: ~a" (response-error-response c))))
  (:documentation "Triggered when a response is attempted more than once."))

(defparameter *wookie-version* (asdf:component-version (asdf:find-system :wookie))
  "Holds Wookie's current version.")

(defclass request ()
  ((socket :accessor request-socket :initarg :socket :initform nil)
   (method :accessor request-method :initarg :method :initform :get)
   (resource :accessor request-resource :initarg :resource :initform "/")
   (headers :accessor request-headers :initarg :headers :initform nil)
   (uri :accessor request-uri :initarg :url :initform nil)
   (data :accessor request-data :initarg :data :initform nil)
   (store-body :accessor request-store-body :initarg :store-body :initform t)
   (body :accessor request-body :initform nil)
   (plugin-data :accessor request-plugin-data :initarg :plugin-data :initform nil)
   (body-callback :accessor request-body-callback :initarg :body-callback :initform nil)
   (body-callback-setcb :accessor request-body-callback-setcb :initarg :body-callback-setcb :initform nil)
   (http :accessor request-http :initarg :http :initform nil))
  (:documentation "A class describing a request, passed to every route."))

(defclass response ()
  ((headers :accessor response-headers :initarg :headers :initform nil)
   (request :accessor response-request :initarg :request :initform nil)
   (finishedp :accessor response-finished-p :initarg :finishedp :initform nil)
   (chunk-stream :accessor response-chunk-stream :initarg :chunk-stream :initform nil))
  (:documentation "A class holding information about a response to the client."))

(defgeneric get-socket (request/response)
  (:documentation
    "Grabs the current socket for the request/response given."))

(defmethod get-socket ((request request))
  (request-socket request))

(defmethod get-socket ((response response))
  (get-socket (response-request response)))

(defmacro with-chunking (request (chunk-data last-chunk-p) &body body)
  "Set up a listener for chunked data in a chunk-enabled router. This macro
   takes a request object, the names of the chunk-data/finishedp arguments
   for the body, and the body form.

   Chunk-data is a byte-array of data received as decoded chunked data comes in
   from the client, and last-chunk-p is a boolean indicating whether the last
   chunk from the request is being sent in."
  (let ((request-var (gensym "request")))
    `(progn
       (let ((,request-var ,request))
         (setf (request-body-callback ,request-var)
               (lambda (,chunk-data ,last-chunk-p)
                 (vom:debug2 "(chunk) Got chunk (~a) ~a bytes"
                            ,last-chunk-p
                            (length ,chunk-data))
                 ,@body))
         (when (request-body-callback-setcb ,request-var)
           (funcall (request-body-callback-setcb ,request-var) (request-body-callback ,request-var))
           (setf (request-body-callback-setcb ,request-var) nil))))))

(defun add-default-headers (headers)
  "Add a number of default headers to a headers plist. If one of the default
   headers is already present, do NOT overwrite it. This allows the app to set
   its own headers that can override the defaults."
  ;(setf (getf headers :date) (local-time:format-timestring
  ;                             nil
  ;                             (local-time:now)
  ;                             :format local-time:+rfc-1123-format+))
  (unless *hide-version*
    (set-header headers :server (format nil "Wookie (~a)" *wookie-version*)))
  headers)

(defun get-log-uri (uri)
  "Given a quri object, return a string of the printable version for logging."
  (let* ((str (quri:uri-path uri))
         (params (quri:uri-query-params uri))
         (enable-qs nil)
         (str (if (and enable-qs params)
                  (concatenate 'string
                               str
                               "?"
                               (reduce (lambda (acc x)
                                         (concatenate 'string
                                                      acc
                                                      (when (< 0 (length acc))
                                                        "&")
                                                      x))
                                       (mapcar (lambda (x) (concatenate 'string (car x) "=" (cdr x)))
                                               params)
                                       :initial-value ""))
                  str)))
    str))

(defun send-response (response &key (status 200) headers body (close nil close-specified-p))
  "Send a response to an incoming request. Takes :status, :headers, and :body
   keyword arguments, which together form an entire response.

   If :close is T, close the client connection after the response has been
   sent fully. However, send-response does its best to read the request headers
   and determine whether or not the connection should be closed. Unless you have
   a reason to specify :close, it may be best to leave it blank."
  (vom:debug "(response) ~a ~a" response (response-request response))
  (let ((req (response-request response)))
    (vom:info "\"~a ~a\" ~a ~a"
              (request-method req) (get-log-uri (request-uri req))
              status (length body)))
  ;; make sure we haven't already responded to this request
  (when (response-finished-p response)
    (error (make-instance 'response-already-sent :response response)))

  (let* ((request (response-request response))
         (socket (request-socket request)))
    (when (as:socket-closed-p socket)
      (error 'as:socket-closed
             :code -1
             :msg "Trying to operate on a closed socket"
             :socket socket))
  
    ;; run the response hooks
    (do-run-hooks (socket) (run-hooks :response-started response request status headers body)
      (let* ((response-headers (response-headers response))
             (headers (append (if (hash-table-p response-headers)
                                  (alexandria:hash-table-plist response-headers)
                                  response-headers)
                              (if (hash-table-p headers)
                                  (alexandria:hash-table-plist headers)
                                  headers)))
             (body-enc (cond ((stringp body)
                              (babel:string-to-octets body :encoding :utf-8))
                             ((typep body 'cl-async-util:octet-vector)
                              body)
                             ((null body)
                              nil)
                             (t
                              (error "Unsupported body type (need string or octet vector): ~a~%" (type-of body)))))
             (status-text (lookup-status-text status)))
        (when (and body (not (get-header headers :content-length)))
          (set-header headers :content-length (length body-enc)))
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

        ;; auto-select the best close method, but only if close wasn't specified
        (unless close-specified-p
          (let ((request-headers (request-headers request)))
            (cond
              ;; we're chunking, so don't close yet
              ((string= (get-header request-headers "transfer-encoding") "chunked")
               (setf close nil))
              ;; we got Connection: keep-alive. so, keep-alive...
              ((string= (get-header request-headers "connection") "keep-alive")
               (setf close nil))
              ;; we got a Connection: close and we're not chunking. close.
              ((string= (get-header request-headers "connection") "close")
               (setf close t)))))

        ;; if we specified we want to close, do it now
        (if close
            ;; close the socket once it's done writing
            (as:write-socket-data socket (as:bytes nil)
              :write-cb (lambda (socket)
                          (vom:debug1 "(response) Close socket ~a" response)
                          (setf (as:socket-data socket) nil)
                          (as:close-socket socket)))
            ;; we sent a response, but aren't closing. reset the parser so that if
            ;; another request comes in on the same socket, WE'LL BE READY!!!!11one
            (progn
              (vom:debug1 "(response) Reset parser: ~a" response)
              (setup-parser socket)))

        ;; mark the response as having been sent
        (setf (response-finished-p response) t)
        response))))

(defun start-response (response &key (status 200) headers)
  "Start a response to the client, but do not specify body content (or close the
   connection). Return a chunked (chunga) stream that can be used to send the
   body content bit by bit until finished by calling finish-response."
  (vom:debug "(response) chunked ~a ~a" response (response-request response))
  ;; we need to add in our own transfer header, so remove all others
  (dolist (head-list (list headers (response-headers response)))
    (remf head-list :content-length)
    (remf head-list :transfer-encoding))
  (send-response response
                 :status status
                 :headers (append headers
                                  (list :transfer-encoding "chunked"))
                 :close nil)
  (let* ((request (response-request response))
         (async-stream (make-instance 'as:async-io-stream :socket (request-socket request)))
         (chunked-stream (chunga:make-chunked-stream async-stream)))
    (setf (chunga:chunked-stream-output-chunking-p chunked-stream) t
          (response-chunk-stream response) chunked-stream)
    chunked-stream))

(defun finish-response (response &key (close nil close-specified-p))
  "Given the stream passed back from start-response, finalize the response (send
   empty chunk) and close the connection, if specified."
  (vom:debug "(response) finish ~a ~a" response (response-request response))
  (let ((req (response-request response)))
    (vom:info "\"~a ~a\" ~a ~a"
              (request-method req) (get-log-uri (request-uri req))
              status "(chunked)"))
  (let* ((chunked-stream (response-chunk-stream response))
         (request (response-request response))
         (socket (request-socket request)))
    ;; make sure the stream writes its final data
    (force-output chunked-stream)
    ;; auto-select the best close method, but only if close wasn't specified
    (unless close-specified-p
      (let ((request-headers (request-headers request)))
        (cond
          ;; we got Connection: keep-alive. so, keep-alive...
          ((string= (get-header request-headers "connection") "keep-alive")
           (setf close nil))
          ;; we got a Connection: close so let's oblige the client
          ((string= (get-header request-headers "connection") "close")
           (setf close t)))))

    ;; write empty chunk
    (as:write-socket-data socket (as:bytes #(48 13 10 13 10))   ; "0\r\n\r\n"
      :write-cb (lambda (socket)
                  (when close
                    (vom:debug1 "(response) Finish, close socket")
                    (setf (as:socket-data socket) nil)
                    (as:close-socket socket)))))
  response)

(defun send-100-continue (response)
  "Send a 100 Continue header on the given response object."
  (let ((sock (request-socket (response-request response))))
    (as:write-socket-data sock (format nil "HTTP/1.1 100 Continue~c~c~c~c"
                                       #\return #\newline #\return #\newline))))

