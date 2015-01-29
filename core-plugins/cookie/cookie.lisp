(defpackage :wookie-plugin-core-cookie
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-cookie)

(defparameter *scanner-cookie-split*
  (cl-ppcre:create-scanner ";[ \\s\\t]+")
  "Scanner for splitting up cookies.")

(defun parse-cookie-vars (request)
  "Grab Cookie data from parsed URI querystring and set into a hash table stored
   with the request."
  (let ((hash-cookie-vars (make-hash-table :test #'equal))
        (cookies (gethash "cookie" (request-headers request))))
    (when cookies
      (dolist (cookie (cl-ppcre:split *scanner-cookie-split* cookies))
        (let* ((search-eq (position #\= cookie))
               (key (subseq cookie 0 search-eq))
               (val (subseq cookie (1+ search-eq))))
          (setf (gethash key hash-cookie-vars) val)))
      (setf (plugin-request-data :cookie request) hash-cookie-vars))))

(defplugfun cookie-var (request key)
  "Get a value from the Cookie data by key."
  (let ((hash-cookie-vars (plugin-request-data :cookie request)))
    (when (and hash-cookie-vars (hash-table-p hash-cookie-vars))
      (gethash key hash-cookie-vars))))

(defplugfun set-cookie (response key val &key expires max-age path domain http-only secure)
  "Update the headers for a response to set a cookie."
  (when (stringp (gethash "set-cookie" (response-headers response)))
    (setf (gethash "set-cookie" (response-headers response))
          (list (gethash "set-cookie" (response-headers response)))))
  (let* ((attributes (list :expires expires
                           :max-age max-age
                           :path path
                           :domain domain
                           :http-only http-only
                           :secure secure))
         (attributes (map-plist attributes
                                (lambda (k v)
                                  (when v
                                    (let ((val (if (or (eq k :http-only)
                                                       (eq k :secure))
                                                   ""
                                                   (concatenate 'string "="
                                                                (if (stringp v)
                                                                    v
                                                                    (write-to-string v))))))
                                    (concatenate 'string
                                                 (camel-case k) val))))))
         (header (concatenate 'string key "=" val))
         (header (reduce (lambda (a b)
                           (unless (string= b "")
                             (concatenate 'string a "; " b)))
                         attributes
                         :initial-value header)))
    (push header (gethash "set-cookie" (response-headers response)))))

(defun init-cookie-vars ()
  (add-hook :parsed-headers 'parse-cookie-vars :cookie-core-parse-vars))

(defun unload-cookie-vars ()
  (remove-hook :parsed-headers :cookie-core-parse-vars))

(register-plugin :cookie 'init-cookie-vars 'unload-cookie-vars)

