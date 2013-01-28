(defpackage :wookie-plugin-core-session
  (:use :cl))
(in-package :wookie-plugin-core-session)

(defun load-session-data (request response)
  (declare (ignore response))
  (wookie-plugin:set-plugin-request-data :session request "build me!")) 

(defun init-session ()
  (wookie:add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (wookie:remove-hook :pre-route :session-main-hook))

(wookie-plugin:register-plugin :session 'init-session 'unload-session)

