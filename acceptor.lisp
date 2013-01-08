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

(defun event-handler (ev)
  "Handle socket events/conditions that crop up during processing."
  (format t "(ev) ~a~%" ev))

(defun handle-connection (sock)
  "Handles a new connection. Creates a bunch of closures that are passed into an
   http-parse parser which decide amongst themselves, during different points in
   the parsing, when to dispatch to the found router, when to send chunked
   content to the route, etc."
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
                      (parsed-uri (puri:parse-uri resource))
                      (path (puri:uri-path parsed-uri))
                      (found-route (find-route method path)))
                 ;; save the parsed uri for plugins/later code
                 (setf (request-uri request) parsed-uri)
                 (run-hooks :parsed-headers request)
                 ;; save any GET data into the request object
                 ;; TODO: investigate making this, POST, multipart, and cookies
                 ;; all plugins (multipart being the trickiest since it would have
                 ;; to intercept chunked data...)
                 (map-querystring (puri:uri-query parsed-uri)
                   (lambda (key value)
                     (setf (gethash key (request-get-data request)) value)))
                 ;; set up some tracking/state values now that we have headers
                 (setf route found-route
                       (request-method request) method
                       (request-resource request) resource)
                 ;; handle "Expect: 100-continue" properly
                 (when (string= (getf headers :expect) "100-continue")
                   (if found-route
                       (as:write-socket-data sock (format nil "HTTP/1.1 100 Continue~c~c~c~c"
                                                          #\return #\newline #\return #\newline))
                       (route-not-found response)))
                 ;; if we found a route, the route allows chunking, and we have
                 ;; chunked data, call the route now so it can set up its chunk
                 ;; handler before we start streaming the body chunks to it
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
    "Start Wookie with the given acceptor."))

(defmethod start-server ((acceptor acceptor))
  ;; start the async server
  (as:tcp-server (acceptor-bind acceptor) (acceptor-port acceptor)
    (lambda (sock data)
      ;; grab the parser stored in the socket and pipe the data into it
      (let ((parser (as:socket-data sock)))
        (funcall parser data)))
    #'event-handler
    :connect-cb #'handle-connection))

