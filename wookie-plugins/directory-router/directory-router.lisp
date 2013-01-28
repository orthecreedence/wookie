(defpackage :wookie-plugin-core-directory-router
  (:use :cl :wookie :wookie-util :wookie-plugin))
(in-package :wookie-plugin-core-directory-router)

(defparameter *scanner-strip-trailing-slash*
  (cl-ppcre:create-scanner "/+$")
  "A scanner that removes the trailing slashes from a path.")

(defparameter *scanner-strip-leading-slash*
  (cl-ppcre:create-scanner "^/+")
  "A scanner that removes the leading slashes from a path.")

(defparameter *scanner-basename*
  (cl-ppcre:create-scanner "^.*/([^/]+/?)$")
  "Grabs a path's basename.")

(defun directory-listing (file-path route-path local-path request response)
  "Send a directory listing."
  (declare (ignore request))
  (let ((files (directory (concatenate 'string local-path "/" file-path "/*.*")))
        (stream (start-response response :headers '(:content-type "text/html"))))
    (flet ((write-line (string)
             (write-sequence (babel:string-to-octets (concatenate 'string string #(#\return #\newline))
                                                     :encoding :utf-8)
                             stream)))
      (write-line "<html>")
      (write-line (format nil "<head><title>Index of ~a/</title></head>" file-path))
      (write-line "<body>")
      (write-line (format nil "<h1>Index of ~a/</h1>" file-path))
      (write-line "<ul>")
      (unless (string= (namestring file-path) "")
        (write-line (format nil "<li><a href=\"/~a/..\">..</a></li>"
                            file-path)))
      (dolist (file files)
        (let* ((filename (namestring file))
               (basename (cl-ppcre:regex-replace *scanner-basename*
                                                 filename
                                                 "\\1")))
          (write-line (format nil "<li><a href=\"~a~a/~a\">~a</a></li>"
                              route-path
                              (if (or (string= file-path "")
                                      (eq (aref file-path 0) #\/))
                                  file-path
                                  (concatenate 'string "/" file-path))
                              basename
                              basename))))
      (write-line "</ul>")
      (write-line "</body>")
      (write-line "</html>")
      (finish-response response))))

(defun send-file (file-path route-path local-path request response)
  (declare (ignore request route-path))
  (let ((path (concatenate 'string local-path "/" file-path))
        (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (stream (start-response response :headers '(:content-type "text/plain"))))
    (with-open-file (fstream path :element-type '(unsigned-byte 8))
      (loop for n = (read-sequence buffer fstream)
            while (< 0 n) do
        (write-sequence (subseq buffer 0 n) stream)
        (force-output stream)))
    (finish-response response)))

(defplugfun def-directory-route (route-path local-path)
  "Define a route that handles directory listings and file serving. If a file or
   directory doesn't exist, run the next route."
  (flet ((remove-trailing-slashes (path)
           (cl-ppcre:regex-replace *scanner-strip-trailing-slash* path "")))
    (let* ((route-path (namestring route-path))
           (route-path (remove-trailing-slashes route-path))
           (resource (concatenate 'string route-path "/(.*)$")))
      (clear-route :get resource)
      (defroute (:get resource) (req res args)
        (let* ((file-path (remove-trailing-slashes (car args)))
               (local-file (concatenate 'string
                                        (namestring local-path)
                                        "/" file-path)))
          (cond
            ((cl-fad:directory-exists-p local-file)
             (directory-listing file-path route-path local-path req res))
            ((cl-fad:file-exists-p local-file)
             (send-file file-path route-path local-path req res))
            (t 
             (next-route))))))))

;; guess we don't need these
(defun init-directory-router ())
(defun unload-directory-router ())

(wookie-plugin:register-plugin :directory-router 'init-directory-router 'unload-directory-router)

