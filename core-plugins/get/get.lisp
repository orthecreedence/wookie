(defpackage :wookie-plugin-core-get
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-get)

(defun parse-get-vars (request)
  "Grab GET data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-get-vars (make-hash-table :test #'equal)))
    (map-querystring (puri:uri-query (request-uri request))
      (lambda (key val)
        (setf (gethash key hash-get-vars) val)))
    (setf (plugin-request-data :get request) hash-get-vars)))

(defplugfun get-var (request key)
  "Get a value from the GET data by key."
  (let ((hash-get-vars (plugin-request-data :get request)))
    (gethash key hash-get-vars)))

(defun init-get-vars ()
  (add-hook :parsed-headers 'parse-get-vars :get-core-parse-vars))

(defun unload-get-vars ()
  (remove-hook :parsed-headers :get-core-parse-vars))

(register-plugin :get 'init-get-vars 'unload-get-vars)

