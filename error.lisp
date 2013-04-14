(in-package :wookie)

(define-condition wookie-error ()
  ((msg :initarg :msg :reader wookie-error-msg :initform nil)
   (socket :initarg :socket :reader wookie-error-socket :initform nil))
  (:report (lambda (c s) (format s "Wookie error: ~a" (wookie-error-msg c))))
  (:documentation "Describes a basic error while processing. Meant to be extended."))

