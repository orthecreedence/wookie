(uiop:define-package #:wookie/wookie-plugin/session
  (:documentation "A session plugin for Wookie")
  (:import-from #:wookie)
  (:import-from #:wookie/wookie-plugin/cookie)
  (:use #:cl #:wookie-util #:wookie))
(in-package #:wookie/wookie-plugin/session)

(defun load-session-data (request response)
  (declare (ignore response))
  (setf (plugin-request-data :session request) "build me!"))

(defun init-session ()
  (add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (remove-hook :pre-route :session-main-hook))

(register-plugin :session 'init-session 'unload-session)

