(defpackage :wookie-plugin-core-directory-router
  (:use :cl :wookie-util :wookie))
(in-package :wookie-plugin-core-directory-router)

(defparameter *scanner-get-extension*
  (cl-ppcre:create-scanner "^.*\\.([a-z0-9]+)$" :case-insensitive-mode t)
  "Grab a file extension.")

(defparameter *scanner-strip-trailing-slash*
  (cl-ppcre:create-scanner "/+$")
  "A scanner that removes the trailing slashes from a path.")

(defparameter *scanner-strip-leading-slash*
  (cl-ppcre:create-scanner "^/+")
  "A scanner that removes the leading slashes from a path.")

(defparameter *scanner-basename*
  (cl-ppcre:create-scanner "^.*/([^/]+/?)$")
  "Grabs a path's basename.")

(defun get-mime (file)
  (let* ((extension (cl-ppcre:regex-replace *scanner-get-extension* file "\\1"))
         (ext-sym (intern (string-upcase extension)
                          :wookie-plugin-core-directory-router)))
    (case ext-sym
      ((html htm shtml) "text/html")
      ((css) "text/css")
      ((xml) "text/xml")
      ((gif) "image/gif")
      ((jpeg jpg) "image/jpeg")
      ((js) "application/x-javascript")
      ((atom) "application/atom+xml")
      ((rss) "application/rss+xml")

      ((mml) "text/mathml")
      ((txt) "text/plain")
      ((jad) "text/vnd.sun.j2me.app-descriptor")
      ((wml) "text/vnd.wap.wml")
      ((htc) "text/x-component")

      ((png) "image/png")
      ((tif tiff) "image/tiff")
      ((wbmp) "image/vnd.wap.wbmp")
      ((ico) "image/x-icon")
      ((jng) "image/x-jng")
      ((bmp) "image/x-ms-bmp")
      ((svg) "image/svg+xml")

      ((jar war ear) "application/java-archive")
      ((hqx) "application/mac-binhex40")
      ((doc) "application/msword")
      ((pdf) "application/pdf")
      ((ps eps ai) "application/postscript")
      ((rtf) "application/rtf")
      ((xls) "application/vnd.ms-excel")
      ((ppt) "application/vnd.ms-powerpoint")
      ((wmlc) "application/vnd.wap.wmlc")
      ((kml) "application/vnd.google-earth.kml+xml")
      ((kmz) "application/vnd.google-earth.kmz")
      ((7z) "application/x-7z-compressed")
      ((cco) "application/x-cocoa")
      ((jardiff) "application/x-java-archive-diff")
      ((jnlp) "application/x-java-jnlp-file")
      ((run) "application/x-makeself")
      ((pl pm) "application/x-perl")
      ((prc pdb) "application/x-pilot")
      ((rar) "application/x-rar-compressed")
      ((rpm) "application/x-redhat-package-manager")
      ((sea) "application/x-sea")
      ((swf) "application/x-shockwave-flash")
      ((sit) "application/x-stuffit")
      ((tcl tk) "application/x-tcl")
      ((der pem crt) "application/x-x509-ca-cert")
      ((xpi) "application/x-xpinstall")
      ((xhtml) "application/xhtml+xml")
      ((zip) "application/zip")

      ((bin exe dll) "application/octet-stream")
      ((deb) "application/octet-stream")
      ((dmg) "application/octet-stream")
      ((eot) "application/octet-stream")
      ((iso img) "application/octet-stream")
      ((msi msp msm) "application/octet-stream")

      ((mid midi kar) "audio/midi")
      ((mp3) "audio/mpeg")
      ((ogg) "audio/ogg")
      ((ra) "audio/x-realaudio")

      ((3gpp 3gp) "video/3gpp")
      ((mpeg mpg) "video/mpeg")
      ((mov) "video/quicktime")
      ((flv) "video/x-flv")
      ((mng) "video/x-mng")
      ((asx asf) "video/x-ms-asf")
      ((wmv) "video/x-ms-wmv")
      ((avi) "video/x-msvideo")
      (t "text/plain"))))
  
(defun directory-listing (file-path route-path local-path request response)
  "Send a directory listing."
  (declare (ignore request))
  (let ((files (cl-fad:list-directory (concatenate 'string local-path "/" file-path "/")))
        (stream (start-response response :headers '(:content-type "text/html")))
        (filtered-filepath (if (or (string= file-path "")
                                   (eq (aref file-path 0) #\/))
                               file-path
                               (concatenate 'string "/" file-path))))
    (flet ((write-rline (string)
             (write-sequence (babel:string-to-octets (concatenate 'string string #(#\return #\newline))
                                                     :encoding :utf-8)
                             stream)))
      (write-rline "<html>")
      (write-rline (format nil "<head><title>Index of ~a/</title></head>" file-path))
      (write-rline "<body>")
      (write-rline (format nil "<h1>Index of ~a/</h1>" file-path))
      (write-rline "<ul>")
      (unless (string= (namestring file-path) "")
        (write-rline (format nil "<li><a href=\"~a~a/..\">..</a></li>"
                            route-path
                            filtered-filepath)))
      (dolist (file files)
        (let* ((ext (pathname-type file))
               (basename (if (cl-fad:directory-exists-p file)
                             (concatenate 'string
                                          (car (last (pathname-directory file)))
                                          "/")
                             (concatenate 'string
                                          (pathname-name file)
                                          (when ext (format nil ".~a" ext))))))
          (write-rline (format nil "<li><a href=\"~a~a/~a\">~a</a></li>"
                               route-path
                               filtered-filepath
                               basename
                               basename))))
      (write-rline "</ul>")
      (write-rline "</body>")
      (write-rline "</html>")
      (finish-response response))))

(defun send-file (file-path route-path local-path request response)
  (declare (ignore request route-path))
  (let ((path (concatenate 'string local-path "/" file-path))
        (buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (stream (start-response response :headers (list :content-type
                                                        (get-mime file-path)))))
    (with-open-file (fstream path :element-type '(unsigned-byte 8))
      (loop for n = (read-sequence buffer fstream)
            while (< 0 n) do
        (write-sequence (subseq buffer 0 n) stream)
        (force-output stream)))
    (finish-response response)))

(defplugfun def-directory-route (route-path local-path &key disable-directory-listing)
  "Define a route that handles directory listings and file serving. If a file or
   directory doesn't exist, run the next route."
  (flet ((remove-trailing-slashes (path)
           (cl-ppcre:regex-replace *scanner-strip-trailing-slash* path ""))
         (remove-route-path (path)
           (let ((route-path-pos (search route-path path)))
             (if route-path-pos
                 (subseq path route-path-pos)
                 path))))
    (let* ((route-path (remove-trailing-slashes route-path))
           (resource (concatenate 'string route-path "(/?.*)$")))
      (clear-route :get resource)
      (defroute (:get resource) (req res args)
        (let* ((file-path (remove-trailing-slashes (car args)))
               (file-path (remove-route-path file-path))
               (local-file (concatenate 'string
                                        (namestring local-path)
                                        "/" file-path))
               (is-directory (cl-fad:directory-exists-p local-file)))
          (cond
            ((and (not disable-directory-listing)
                  is-directory)
             (directory-listing file-path route-path local-path req res))
            ((and (not is-directory)
                  (cl-fad:file-exists-p local-file))
             (send-file file-path route-path local-path req res))
            (t 
             (next-route))))))))

;; guess we don't need these LOL
(defun init-directory-router ())
(defun unload-directory-router ())

(register-plugin :directory-router 'init-directory-router 'unload-directory-router)

