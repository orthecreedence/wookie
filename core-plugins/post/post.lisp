(defpackage :wookie-plugin-core-post
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-post)

(defun check-if-post (request)
  "Check if this request contains POST data, and mark the plugin data as such so
   once we have body data we know whether or not to try and parse it."
  (let ((headers (request-headers request)))
    (when (search "application/x-www-form-urlencoded" (getf headers :content-type))
      ;; make sure we store the body so we can access it once it uploads
      (setf (http-parse:http-store-body (request-http request)) t)
      ;; setting T here lets the `parse-post-vars` fn know that we're dealing
      ;; with POST vars
      (setf (plugin-request-data :post request) t))))

(defun parse-post-vars (request)
  "Grab POST data from parsed URI querystring and set into a hash table stored
   with the request."
  (when (plugin-request-data :post request)
    ;; convert the body to a string via the Content-Type header
    (let* ((headers (request-headers request))
           (body (body-to-string (http-parse:http-body (request-http request))
                                 (getf headers :content-type))))
      (setf (plugin-request-data :post request) (querystring-to-hash body)))))

(defplugfun post-var (request key)
  "Get a value from the POST data by key."
  (let ((hash-post-vars (plugin-request-data :post request)))
    (gethash key hash-post-vars)))

(defun init-post-vars ()
  (add-hook :parsed-headers 'check-if-post :post-core-check-post)
  (add-hook :body-complete 'parse-post-vars :post-core-parse-post))

(defun unload-post-vars ()
  (remove-hook :parsed-headers :post-core-plugin)
  (remove-hook :body-complete :post-core-parse-post))

(register-plugin :post 'init-post-vars 'unload-post-vars)

