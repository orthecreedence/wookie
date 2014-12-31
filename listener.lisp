(in-package :wookie)

(defclass listener ()
  ((bind :accessor listener-bind :initarg :bind :initform nil)
   (port :accessor listener-port :initarg :port :initform 80)
   (backlog :accessor listener-backlog :initarg :backlog :initform -1))
  (:documentation "Describes an HTTP listener."))

(defgeneric start-server (listener)
  (:documentation
    "Start Wookie with the given listener."))

(defmethod start-server ((listener listener))
  ;; start the async server
  (vom:info "(start) Starting Wookie  ~a:~a"
            (if (listener-bind listener)
                (listener-bind listener)
                "0.0.0.0")
            (listener-port listener))
  (as:tcp-server (listener-bind listener) (listener-port listener)
    'read-data
    'listener-event-handler
    :connect-cb 'handle-connection
    :backlog (listener-backlog listener)))

