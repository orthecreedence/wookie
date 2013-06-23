(defpackage :wookie-plugin-core-get
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-get)

(defun parse-get-vars (request)
  "Grab GET data from parsed URI querystring and set into a hash table stored
   with the request."
   (setf (plugin-request-data :get request) (querystring-to-hash
                                              (puri:uri-query
                                                (request-uri request)))))

(defplugfun get-var (request key)
  "Get a value from the GET data by key."
  (let ((hash-get-vars (plugin-request-data :get request)))
    (when (and hash-get-vars (hash-table-p hash-get-vars))
      (gethash key hash-get-vars))))

(defun init-get-vars ()
  (add-hook :parsed-headers 'parse-get-vars :get-core-parse-vars))

(defun unload-get-vars ()
  (remove-hook :parsed-headers :get-core-parse-vars))

(register-plugin :get 'init-get-vars 'unload-get-vars)

