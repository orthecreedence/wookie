(in-package :wookie)

(defclass acceptor ()
  ((bind :accessor acceptor-bind :initarg :bind :initform nil)
   (port :accessor acceptor-port :initarg :port :initform 80)
   (ssl :accessor acceptor-ssl :initarg :ssl :initform nil)
   (ssl-cert :accessor acceptor-ssl-cert :initarg :ssl-cert :initform nil)
   (ssl-key :accessor acceptor-ssl-key :initarg :ssl-key :initform nil)))

(defclass reply ()
  ((socket :accessor reply-socket :initarg :socket :initform nil)
   (request :accessor reply-request :initarg :request :initform nil)
   (headers :accessor reply-headers :initarg :headers :initform nil)))

(defun send-reply (reply &key (status 200) headers body)
  "Send a reply to an incoming request. Takes :status, :headers, and :body
   keyword arguments, which together form an entire response.

   This is meant as more of a lower-level function.

   At the moment, does *not* support streaming chunked content."
  (let* ((headers (append (reply-headers reply) headers))
         (body-enc (when body (babel:string-to-octets body :encoding :utf-8)))
         (headers (if body
                      (append headers (list :content-length (length body-enc)))
                      headers))
         (socket (reply-socket reply))
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
      ;; write all the headers
      (map-plist headers
                 (lambda (header value)
                   (write-http-line "~a: ~a" (camel-case header) value)))
      ;; finalize headers (closing \r\n)
      (write-http-line "")
      ;; send body if specified
      (when body
        (as:write-socket-data socket body-enc)))))

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
            ;; TODO remove this killswitch
            (when (eq (http-parse:http-method http) :delete)
              (as:close-socket sock)
              (as:exit-event-loop))
            ;; we got a full request, parsed and ready to go, find a matching
            ;; route
            (let ((router (find-route (http-parse:http-method http)
                                      (http-parse:http-resource http)))
                  (reply (make-instance 'reply :socket sock :request http))) 
              ;; call matching route or signal error
              (if router
                  (funcall router reply)
                  ;; TODO replace with error hook
                  (send-reply reply :status 404 :body "Page not found =[")))))))
    ;; handle socket events
    (lambda (ev)
      (format t "ev: ~a~%" ev))
    ;; when a new client connects, attach an HTTP parser to the connection so
    ;; when new data comes in on that socket, we can parse it
    :connect-cb (lambda (sock)
                  (let* ((http (make-instance 'http-parse:http-request))
                         (parser (http-parse:make-parser http :store-body t)))
                    ;; attach parser to socket-data so we can deref it in the
                    ;; read-cb
                    (setf (as:socket-data sock) parser)))))

