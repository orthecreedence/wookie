(defpackage :wookie-plugin-core-post
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-post)

(defun check-if-post (request)
  "Check if this request contains POST data, and mark the plugin data as such so
   once we have body data we know whether or not to try and parse it."
  (let ((headers (request-headers request))
        (method (request-method request))
        ;; default 2mb post body size
        (max-post-size (or (getf (plugin-config :post) :max-post-size)
                           (* 1024 1024 2))))
    (when (and (eq method :post)
               (not (not (string= (string-downcase (gethash "transfer-encoding" headers)) "chunked"))))
      ;; let save-body know that we should be storing the body
      (setf (plugin-request-data :post request) #(max-post-size (fast-io:make-output-buffer))))))

(defun save-body (request chunk start end lastp)
  "Save the body as it comes in into our request data."
  (declare (ignore lastp))
  (let* ((plugin-data (or (plugin-request-data :post request) #(nil nil)))
         (max-size (aref plugin-data 0))
         (buffer (aref plugin-data 1)))
    (cond ((and buffer
                (< max-size (fast-io:buffer-position buffer)))
           ;; we hit our limit, wipe out the body and update the plugin data
           (setf (plugin-request-data :post-data request) :body-too-large)
           (setf (plugin-request-data :post-vars request) :body-too-large)
           (setf (aref plugin-data 1) nil)
           (unless (getf (plugin-config :post) :suppress-body-size-error)
             (let* ((sock (request-socket request))
                    (res (getf (as:socket-data sock) :response)))
               (send-response res :status 413 :body "body too large" :close t)
               (let ((future (asf:make-future)))
                 (asf:signal-error future "body too large")
                 (return-from save-body future)))))
          (buffer
           (fast-io:fast-write-sequence chunk buffer start end)))))

(defun parse-post-vars (request)
  "Grab POST data from parsed URI querystring and set into a hash table stored
   with the request."
  (let* ((plugin-data (or (plugin-request-data :post request) #(nil nil)))
         (buffer (aref plugin-data 1)))
    (when buffer
      ;; convert the body to a string via the Content-Type header
      (let* ((body-bytes (fast-io:finish-output-buffer buffer))
             (headers (request-headers request))
             (body (when (search "application/x-www-form-urlencoded"
                                 (gethash "content-type" headers))
                     (body-to-string body-bytes (gethash "content-type" headers))))
             (body-qs (when body
                        (querystring-to-hash body))))
        (setf (plugin-request-data :post-data request) body-bytes)
        (when body
          (setf (plugin-request-data :post-vars request) body-qs))))))

(defplugfun post-var (request key)
  "Get a value from the POST data by key."
  (let ((hash-post-vars (plugin-request-data :post-vars request)))
    (when (and hash-post-vars (hash-table-p hash-post-vars))
      (gethash key hash-post-vars))))

(defun init-post-vars ()
  (add-hook :parsed-headers 'check-if-post :post-core-check-post)
  (add-hook :body-chunk 'save-body :post-core-save-body)
  (add-hook :body-complete 'parse-post-vars :post-core-parse-post))

(defun unload-post-vars ()
  (remove-hook :parsed-headers :post-core-plugin)
  (remove-hook :body-chunk :post-core-save-body)
  (remove-hook :body-complete :post-core-parse-post))

(register-plugin :post 'init-post-vars 'unload-post-vars)

