(defpackage :wookie-core-cookie-vars
  (:use :cl :wookie :wookie-util :wookie-plugin))
(in-package :wookie-core-cookie-vars)

(defparameter *scanner-cookie-split*
  (cl-ppcre:create-scanner ";[ \\s\\t]+")
  "Scanner for splitting up cookies.")

(defun parse-cookie-vars (request)
  "Grab Cookie data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-cookie-vars (make-hash-table :test #'equal))
        (cookies (getf (request-headers request) :cookie)))
    (when cookies
      (dolist (cookie (cl-ppcre:split *scanner-cookie-split* cookies))
        (let* ((search-eq (position #\= cookie))
               (key (subseq cookie 0 search-eq))
               (val (subseq cookie (1+ search-eq))))
          (setf (gethash key hash-cookie-vars) val)))
      (wookie-plugin:set-plugin-request-data :cookie request hash-cookie-vars))))

(defplugfun cookie-var (request key)
  "Get a value from the Cookie data by key."
  (let ((hash-cookie-vars (wookie-plugin:get-plugin-request-data :cookie request)))
    (gethash key hash-cookie-vars)))

(defplugfun set-cookie (response key val &key expires)
  "Update the headers for a response to set a cookie."
  (when (stringp (getf (response-headers response) :set-cookie))
    (setf (getf (response-headers response) :set-cookie)
          (list (getf (response-headers response) :set-cookie))))
  (let* ((header (concatenate 'string key "=" val))
         (header (if expires
                     (let ((date ""))
                       (concatenate 'string header "; Expires=" date))
                     header)))
    (push header (getf (response-headers response) :set-cookie))))

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

