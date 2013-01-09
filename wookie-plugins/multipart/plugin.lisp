(defpackage :wookie-core-multipart-vars
  (:use :cl :wookie :wookie-util :wookie-plugin))
(in-package :wookie-core-multipart-vars)

(defun check-if-multipart (request)
  "Check if this request contains multipart data, and mark the plugin data as
   such so once we have body data we know whether or not to try and parse it."
  (let ((headers (request-headers request)))
    (when (search "multipart/form-data;" (getf headers :content-type))
      (let* ((hash-form-vars (make-hash-table :test #'equal))
             (hash-file-data (make-hash-table :test #'equal))
             (multi-cb (lambda (field-name field-headers field-meta body-bytes body-complete-p)
                         (flet ((save-form-data ()
                                  ;; make accessing the form data hash value less painful
                                  (symbol-macrolet ((hash (gethash field-name hash-form-vars)))
                                    ;; if we don't have an array under the hash value yet, init one
                                    (unless hash
                                      (setf hash (make-array 0 :element-type '(unsigned-byte 8))))
                                    ;; append the data
                                    (setf hash (cl-async-util::append-array
                                                 hash
                                                 body-bytes))
                                    ;; once this field is complete, convert the body to a string
                                    (when body-complete-p
                                      (setf hash (body-to-string hash (getf field-headers :content-type))))))
                                (save-file-data ()
                                  ))
                           (let* ((filep (getf field-meta :filename)))
                             (if filep
                                 (save-file-data)
                                 (save-form-data))))))
             (parser (http-parse:make-multipart-parser headers multi-cb)))
        (when parser
          (wookie-plugin:set-plugin-request-data
            :multipart
            request
            (list :hash-form hash-form-vars
                  :hash-file hash-file-data
                  :parser parser)))))))

(defun parse-multipart-vars (request chunk finishedp)
  "Grab multipart data from parsed URI querystring and set into a hash table
   stored with the request."
  (declare (ignore finishedp))
  (let* ((plugin-data (wookie-plugin:get-plugin-request-data :multipart request))
         (parser (getf plugin-data :parser)))
    ;; if we have a parser, feed the chunk data into it. our hashes will be
    ;; populated as the data is decoded
    (when parser
      (funcall parser chunk))))

(defplugfun form-var (request key)
  "Get a value from the multipart data by key."
  (let* ((plugin-data (wookie-plugin:get-plugin-request-data :multipart request))
         (hash-form-vars (getf plugin-data :hash-form)))
    (gethash key hash-form-vars)))

(defun init-multipart-vars ()
  (wookie:add-hook :parsed-headers #'check-if-multipart :multipart-core-check-multipart)
  (wookie:add-hook :body-chunk #'parse-multipart-vars :multipart-core-parse-multipart))

(defun unload-multipart-vars ()
  (wookie:remove-hook :parsed-headers :multipart-core-plugin)
  (wookie:remove-hook :body-chunk :multipart-core-parse-multipart))

(wookie-plugin:register-plugin
  :multipart
  '(:name "Wookie core multipart plugin"
    :author "Andrew Lyon"
    :version "0.1.0")
  #'init-multipart-vars
  #'unload-multipart-vars)

