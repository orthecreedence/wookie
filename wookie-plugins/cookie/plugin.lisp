;; TODO be sure to provide set-cookie function (takes response object) as well
;; as cookie-var function
(defpackage :wookie-core-cookie-vars
  (:use :cl :wookie :wookie-util :wookie-plugin))
(in-package :wookie-core-cookie-vars)

(defun parse-cookie-vars (request)
  "Grab Cookie data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-cookie-vars (make-hash-table :test #'equal)))
    (map-querystring (puri:uri-query (request-uri request))
      (lambda (key val)
        (setf (gethash key hash-cookie-vars) val)))
    (wookie-plugin:set-plugin-request-data :cookie request hash-cookie-vars)))

(defplugfun cookie-var (request key)
  "Get a value from the Cookie data by key."
  (let ((hash-cookie-vars (wookie-plugin:get-plugin-request-data :cookie request)))
    (gethash key hash-cookie-vars)))

(defplugfun set-cookie (response key val)
  ""
  (declare (ignore response key val)))

(defun init-cookie-vars ()
  (wookie:add-hook :parsed-headers 'parse-cookie-vars :cookie-core-parse-vars))

(defun unload-cookie-vars ()
  (wookie:remove-hook :parsed-headers :cookie-core-parse-vars))

(wookie-plugin:register-plugin
  :cookie
  '(:name "Wookie core Cookie plugin"
    :author "Andrew Lyon"
    :version "0.1.0")
  'init-cookie-vars
  'unload-cookie-vars)

