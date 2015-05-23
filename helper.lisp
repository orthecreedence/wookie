(defpackage :wookie-helper
  (:use :cl :wookie :wookie-plugin-export)
  (:export start-static-server
           serve-html5-app))
(in-package :wookie-helper)

(defun start-static-server (&key (asset-dir "./") bind (port 8080))
  "This is a full-service convenience function that lets you quickly start up a
   server for serving static content (defaults to the current directory, but you
   can specify via `asset-dir`)."
  (load-plugins)
  (funcall (intern "DEF-DIRECTORY-ROUTE" :wookie-plugin-core-directory-router) "/" asset-dir)
  ;; catch-all handler
  (defroute (:* ".*") (req res)
    (send-response res :status 404 :body "Not found!"))
  (as:with-event-loop (:catch-app-errors t)
    (let ((listener (make-instance 'listener :bind bind :port port)))
      (start-server listener))
    (as:signal-handler 2
      (lambda (sig)
        (declare (ignore sig))
        (as:exit-event-loop)))))

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
   returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun serve-html5-app (&key bind port (index-file "index.html"))
  "This is a full-service convenience function for serving up an HTML5 app. This
   is a lot like start-static-server, but sends all non-existent routes to the
   load-index function, assuming your index.html file (or whatever you specify
   in :index-file) is capable of routing on the current URL path. This lets you
   run HTML apps directly from Wookie with one simple call.

   Note that this must be started in the directory of the app itself."
  (load-plugins)

  (flet ((load-index (res)
           (let ((body (file-contents index-file)))
             (send-response res :body body :headers '(:content-type "text/html")))))
    (defroute (:get "/") (req res)
      (load-index res))

    (funcall (intern "DEF-DIRECTORY-ROUTE" :wookie-plugin-core-directory-router) "/" "./")

    ;; send not-found files to index route
    (defroute (:get ".*") (req res)
      (load-index res))

    (as:with-event-loop (:catch-app-errors t)
      (vom:config :wookie :info)
      (start-server (make-instance 'listener :bind bind :port port))
      (as:signal-handler 2
        (lambda (sig)
          (declare (ignore sig))
          (as:exit-event-loop))))))

