(in-package :wookie)

(defclass listener ()
  ((bind :accessor listener-bind :initarg :bind :initform nil)
   (port :accessor listener-port :initarg :port :initform 80)
   (backlog :accessor listener-backlog :initarg :backlog :initform -1))
  (:documentation "Describes an HTTP listener."))

(defclass ssl-listener (listener)
  ((certificate :accessor listener-certificate :initarg :certificate :initform nil)
   (key :accessor listener-key :initarg :key :initform nil)
   (password :accessor listener-password :initarg :password :initform nil))
  (:documentation "Describes an HTTPS listener."))

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
         (response (make-instance 'response :socket sock :request request)))
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
                 (setf (request-uri request) parsed-uri
                       (request-headers request) headers)
                 (run-hooks :parsed-headers request)
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
               (run-hooks :body-chunk request chunk finishedp)
               (let ((request-body-cb (request-body-callback request)))
                 (when request-body-cb
                   (funcall request-body-cb chunk finishedp))))
             (finish-callback ()
               ;; make sure we always dispatch at the end.
               (run-hooks :body-complete request)
               (dispatch-route)))
      ;; make an HTTP parser. will be attached to the socket and will be
      ;; responsible for running all of the above callbacks directly as data
      ;; filters in from the read callback.
      (let ((parser (http-parse:make-parser
                      http
                      :header-callback #'header-callback
                      :body-callback #'body-callback
                      :finish-callback #'finish-callback
                      :store-body t)))
        ;; attach parser to socket-data so we can deref it in the read callback
        (setf (as:socket-data sock) parser)))))

(defun read-data (sock data)
  "A simple read-cb handler that passed data to the HTTP parser attached to the
   socket the data is coming in on. The parser runs all necessary callbacks
   directly, so this function just blindly feeds the data in."
  ;; grab the parser stored in the socket and pipe the data into it
  (let ((parser (as:socket-data sock)))
    (funcall parser data)))

(defgeneric start-server (listener)
  (:documentation
    "Start Wookie with the given listener."))

(defmethod start-server ((listener listener))
  ;; start the async server
  (as:tcp-server (listener-bind listener) (listener-port listener)
    #'read-data
    #'event-handler
    :connect-cb #'handle-connection
    :backlog (listener-backlog listener)))

(defmethod start-server ((listener ssl-listener))
  ;; start the async SSL server
  (as-ssl:tcp-ssl-server (listener-bind listener) (listener-port listener)
    #'read-data
    #'event-handler
    :connect-cb #'handle-connection
    :certificate (listener-certificate listener)
    :key (listener-key listener)
    :password (listener-password listener)
    :backlog (listener-backlog listener)))

