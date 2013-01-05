(in-package :wookie)

(defclass acceptor ()
  ((bind :accessor acceptor-bind :initarg :bind :initform nil)
   (port :accessor acceptor-port :initarg :port :initform 80)
   (ssl :accessor acceptor-ssl :initarg :ssl :initform nil)
   (ssl-cert :accessor acceptor-ssl-cert :initarg :ssl-cert :initform nil)
   (ssl-key :accessor acceptor-ssl-key :initarg :ssl-key :initform nil)))

(defgeneric start-server (acceptor)
  (:documentation
    "Start wookie with the given acceptor."))

(defmethod start-server ((acceptor acceptor))
  ;; start the async server
  (as:tcp-server (acceptor-bind acceptor) (acceptor-port acceptor)
    (lambda (sock data)
      ;; pull the parser out of the socket's data and see if we have a full
      ;; request
      (let ((parser (as:socket-data sock)))
        (multiple-value-bind (http headers-parsed-p body-parsed-p)
            (funcall parser data)
          (when (and headers-parsed-p body-parsed-p) 
            ;; we got a full request, parsed and ready to go, find a matching
            ;; route
            (let ((route-fn (find-route (http-parse:http-method http)
                                        (http-parse:http-resource http)))
                  (request (make-instance 'request
                                          :method (http-parse:http-method http)
                                          :resource (http-parse:http-resource http)
                                          :http http))
                  (response (make-instance 'response :socket sock)))
              ;; run our pre-route hooks
              (run-hooks :pre-route request response)
              ;; call matching route or signal error
              (if route-fn
                  (funcall route-fn request response)
                  ;; TODO replace with error hook
                  (send-response response :status 404 :body "Page not found =["))
              ;; run post-route hooks
              (run-hooks :post-route request response))))))
    ;; handle socket events
    (lambda (ev)
      (format t "ev: ~a~%" ev))
    ;; when a new client connects, attach an HTTP parser to the connection so
    ;; when new data comes in on that socket, we can parse it
    :connect-cb (lambda (sock)
                  ;; TODO pass client address info into :connect hook
                  (run-hooks :connect)
                  (let* ((http (make-instance 'http-parse:http-request))
                         (parser (http-parse:make-parser http :store-body t)))
                    ;; attach parser to socket-data so we can deref it in the
                    ;; read-cb
                    (setf (as:socket-data sock) parser)))))

