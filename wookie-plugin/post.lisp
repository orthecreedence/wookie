(uiop:define-package #:wookie/wookie-plugin/post
  (:documentation "A POST plugin for Wookie")
  (:import-from #:wookie)
  (:import-from #:fast-io)
  (:import-from #:yason)
  (:use #:cl #:wookie-util #:wookie))
(in-package #:wookie/wookie-plugin/post)

(defun check-if-post (request)
  "Check if this request contains POST data, and mark the plugin data as such so
   once we have body data we know whether or not to try and parse it."
  (let ((headers (request-headers request))
        (method (request-method request)))
    (when (and (eq method :post)
               (not (not (string= (string-downcase (gethash "transfer-encoding" headers)) "chunked"))))
      ;; let the parser know we have a post
      (setf (plugin-request-data :post request) t))))

(defun parse-post-vars (request)
  "Grab POST data from parsed URI querystring and set into a hash table stored
   with the request."
  (when (and (request-store-body request)
             (arrayp (request-body request)))
    ;; convert the body to a string via the Content-Type header
    (let* ((body-bytes (request-body request))
           (first-byte (aref body-bytes 0)))
      (if (find first-byte (list (char-code #\{)
                                 (char-code #\")
                                 (char-code #\[)))
          ;; TODO: fix hardcoded UTF8
          (let ((body-obj (ignore-errors (yason:parse (babel:octets-to-string body-bytes :encoding :utf-8)))))
            (when body-obj
              (setf (plugin-request-data :post-vars request) body-obj)))
          (let* ((headers (request-headers request))
                 (body (when (and body-bytes
                                  (search "application/x-www-form-urlencoded"
                                          (gethash "content-type" headers)))
                         (body-to-string body-bytes (gethash "content-type" headers))))
                 (body-qs (when body (querystring-to-hash body))))
            (when body
              (setf (plugin-request-data :post-vars request) body-qs)))))))

(defplugfun post-var (request key)
  "Get a value from the POST data by key."
  (let ((hash-post-vars (plugin-request-data :post-vars request)))
    (when (and hash-post-vars (hash-table-p hash-post-vars))
      (gethash key hash-post-vars))))

(defun init-post-vars ()
  (add-hook :parsed-headers 'check-if-post :post-core-check-post)
  (add-hook :body-complete 'parse-post-vars :post-core-parse-post))

(defun unload-post-vars ()
  (remove-hook :parsed-headers :post-core-plugin)
  (remove-hook :body-complete :post-core-parse-post))

(register-plugin :post 'init-post-vars 'unload-post-vars)

