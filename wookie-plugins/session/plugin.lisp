(defpackage :wookie-core-session-handler
  (:use :cl))
(in-package :wookie-core-session-handler)

(defun load-session-data (request response)
  (declare (ignore response))
  (wookie-plugin:set-plugin-request-data :session request "omg")) 

(defun init-session ()
  (wookie:add-hook :pre-route 'load-session-data :session-main-hook))

(defun unload-session ()
  (wookie:remove-hook :pre-route :session-main-hook))

(wookie-plugin:register-plugin
  :session
  '(:name "Wookie session handler"
    :author "Andrew Lyon"
    :version "0.1.0"
    :depends-on (:cookie))
  'init-session
  'unload-session)

