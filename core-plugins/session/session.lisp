(defpackage :wookie-plugin-core-session
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-session)

(defun load-session-data (request response)
  (declare (ignore response))
  (setf (plugin-request-data :session request) "build me!"))

(defun init-session ()
  (add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (remove-hook :pre-route :session-main-hook))

(register-plugin :session 'init-session 'unload-session)

