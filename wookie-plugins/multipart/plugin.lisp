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
             (cur-file nil)
             (multi-cb (lambda (field-name field-headers field-meta body-bytes body-complete-p)
                         (flet ((save-form-data ()
                                  ;; make accessing the form data hash value less painful
                                  (symbol-macrolet ((hash-set (gethash field-name hash-form-vars)))
                                    (let ((hash-val hash-set))
                                      ;; if we don't have an array under the hash value yet, init one
                                      (unless hash-val
                                        (setf hash-set (make-array 0 :element-type '(unsigned-byte 8))))
                                      ;; append the data
                                      (setf hash-set (cl-async-util::append-array
                                                       hash-val
                                                       body-bytes))
                                      ;; once this field is complete, convert the body to a string
                                      (when body-complete-p
                                        (setf hash-set (body-to-string hash-val (getf field-headers :content-type)))))))
                                (save-file-data ()
                                  ;; open a tmp file for writing if we don't already have a handle
                                  (unless cur-file
                                    (let ((tmp-filename (generate-tmp-file-name)))
                                      ;; save all the needed info about our file: filename,
                                      ;; mime-type, the path to the file the data is stored
                                      ;; in and the handle/stream
                                      (setf cur-file (list :filename (getf field-meta :filename)
                                                           :mime-type (getf field-headers :content-type)
                                                           :tmp-file tmp-filename
                                                           :handle (open tmp-filename
                                                                         :if-exists :supersede
                                                                         :if-does-not-exist :create
                                                                         :direction :output
                                                                         :element-type '(unsigned-byte 8))))))
                                  ;; TODO investigate an option besides writing to a temp
                                  ;; file since this can potentially block the event loop.
                                  (let ((handle (getf cur-file :handle)))
                                    ;; write our sequence directly to the file handle
                                    (write-sequence body-bytes handle)
                                    (force-output handle)
                                    ;; if this is that last chunk, close the file, remove
                                    ;; the handle from the file data, and set the file data
                                    ;; into the file-dat hash.
                                    (when body-complete-p
                                      (close handle)
                                      (remf cur-file :handle)
                                      (setf (gethash field-name hash-file-data) cur-file)
                                      ;; be sure to clear out cur-file so if we get another
                                      ;; upload it won't get confused with this one.
                                      (setf cur-file nil)))))
                           ;; different handler depending on if we have a file upload or a
                           ;; boring old form field
                           (if (getf field-meta :filename)
                               (save-file-data)
                               (save-form-data)))))
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

(defun remove-tmp-files (response request status headers body)
  "Loop over all tmp files uploaded in a request and delete them."
  (declare (ignore response status headers body))
  (let ((files (getf (wookie-plugin:get-plugin-request-data :multipart request)
                     :hash-file)))
    (loop for file-entry being the hash-values of files do
      (let ((tmp-filename (getf file-entry :tmp-file)))
        ;(ignore-errors (delete-file tmp-filename))
        ))))

(defplugfun form-var (request field-name)
  "Get a value from the multipart data by its field name (string)."
  (let* ((plugin-data (wookie-plugin:get-plugin-request-data :multipart request))
         (hash-form-vars (getf plugin-data :hash-form)))
    (gethash field-name hash-form-vars)))

(defplugfun file-upload (request field-name)
  "Get a file entry from the request by the field name (string). The entry is a
   plist with the keys :filename, :tmp-file, and :mime-type."
  (let* ((plugin-data (wookie-plugin:get-plugin-request-data :multipart request))
         (hash-form-vars (getf plugin-data :hash-file)))
    (gethash field-name hash-form-vars)))

(defun init-multipart-vars ()
  (wookie:add-hook :parsed-headers #'check-if-multipart :multipart-core-check-multipart)
  (wookie:add-hook :body-chunk #'parse-multipart-vars :multipart-core-parse-multipart)
  (wookie:add-hook :response-started #'remove-tmp-files :multipart-core-remove-tmp))

(defun unload-multipart-vars ()
  (wookie:remove-hook :parsed-headers :multipart-core-plugin)
  (wookie:remove-hook :body-chunk :multipart-core-parse-multipart)
  (wookie:add-hook :response-started :multipart-core-remove-tmp))

(wookie-plugin:register-plugin
  :multipart
  '(:name "Wookie core multipart plugin"
    :author "Andrew Lyon"
    :version "0.1.0")
  #'init-multipart-vars
  #'unload-multipart-vars)

