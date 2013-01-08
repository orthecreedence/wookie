(in-package :wookie)

(defclass acceptor ()
  ((bind :accessor acceptor-bind :initarg :bind :initform nil)
   (port :accessor acceptor-port :initarg :port :initform 80)
   (ssl :accessor acceptor-ssl :initarg :ssl :initform nil)
   (ssl-cert :accessor acceptor-ssl-cert :initarg :ssl-cert :initform nil)
   (ssl-key :accessor acceptor-ssl-key :initarg :ssl-key :initform nil)))

(defun route-not-found (response)
  "Centralized function for handling the case of a missing router."
  (send-response response :status 404 :body "Page not found =["))

(defun handle-connection (sock)
  ;; TODO pass client address info into :connect hook
  (run-hooks :connect)
  (let* ((http (make-instance 'http-parse:http-request))
         (route nil)  ; holds the current route, filled in below once we get headers
         (route-dispatched nil)
         (request (make-instance 'request :http http))
         (response (make-instance 'response :socket sock)))
    (labels ((dispatch-route ()
               ;; dispatch the current route, but only if we haven't already done so
               (when route-dispatched
                 (return-from dispatch-route))
               (setf route-dispatched t)
               (run-hooks :pre-route request response)
               (if route
                   (let ((route-fn (getf route :curried-route)))
                     (funcall route-fn request response))
                   (route-not-found response))
               (run-hooks :post-route request response))
             (header-callback (headers)
               ;; if we got the headers, it means we can find the route we're
               ;; destined to use. if the route accepts chunks and the body is
               ;; chunked, run the router now so it can set up chunk listening.
               ;; otherwise, save the route for later and let the rest of the
               ;; request come in.
               (let* ((method (http-parse:http-method http))
                      (resource (http-parse:http-resource http))
                      (found-route (find-route method resource)))
                 (setf route found-route
                       (request-method request) method
                       (request-resource request) resource)
                 (when (string= (getf headers :expect) "100-continue")
                   (if found-route
                       (as:write-socket-data sock (format nil "HTTP/1.1 100 Continue~c~c~c~c"
                                                          #\return #\newline #\return #\newline))
                       (route-not-found response)))
                 (when (and found-route
                            (string= (getf headers :transfer-encoding) "chunked")
                            (getf found-route :allow-chunking))
                   (dispatch-route))))
             (body-callback (chunk finishedp)
               ;; forward the chunk to the callback provided in the chunk-enabled
               ;; router
               (let ((request-body-cb (request-body-callback request)))
                 (when request-body-cb
                   (funcall request-body-cb chunk finishedp))))
             (finish-callback ()
               ;; make sure we always dispatch at the end.
               (dispatch-route)))
      (let ((parser (http-parse:make-parser
                      http
                      :header-callback #'header-callback
                      :body-callback #'body-callback
                      ; TODO multipart handling (tmp files, probably)
                      :finish-callback #'finish-callback
                      :store-body t)))
        ;; attach parser to socket-data so we can deref it in the
        ;; read-cb
        (setf (as:socket-data sock) parser)))))

(defgeneric start-server (acceptor)
  (:documentation
    "Start wookie with the given acceptor."))

(defmethod start-server ((acceptor acceptor))
  ;; start the async server
  (as:tcp-server (acceptor-bind acceptor) (acceptor-port acceptor)
    (lambda (sock data)
      ;; grab the parser stored in the socket and pipe the data into it
      (let ((parser (as:socket-data sock)))
        (funcall parser data)))
    ;; handle socket events
    (lambda (ev)
      (format t "ev: ~a~%" ev))
    ;; when a new client connects, attach an HTTP parser to the connection so
    ;; when new data comes in on that socket, we can parse it
    :connect-cb #'handle-connection))

