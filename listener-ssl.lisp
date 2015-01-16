(in-package :wookie)

(defclass ssl-listener (listener)
  ((certificate :accessor listener-certificate :initarg :certificate :initform nil)
   (key :accessor listener-key :initarg :key :initform nil)
   (password :accessor listener-password :initarg :password :initform nil))
  (:documentation "Describes an HTTPS listener."))

(defmethod start-server ((listener ssl-listener))
  ;; start the async SSL server
  (vom:info "(start) Starting Wookie (SSL)  ~a:~a"
            (if (listener-bind listener)
                (listener-bind listener)
                "0.0.0.0")
            (listener-port listener))
  (as-ssl:tcp-ssl-server (listener-bind listener) (listener-port listener)
    'read-data
    :event-cb 'listener-event-handler
    :connect-cb 'handle-connection
    :certificate (listener-certificate listener)
    :key (listener-key listener)
    :password (listener-password listener)
    :backlog (listener-backlog listener)))

