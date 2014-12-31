(defpackage :wookie-helper
  (:use :cl :wookie :wookie-plugin-export)
  (:export start-static-server))
(in-package :wookie-helper)

(defun start-static-server (&key (asset-dir "./") bind (port 8080))
  "This is a full-service convenience function that lets you quickly start up a
   server for serving static content (defaults to the current directory, but you
   can specify via `asset-dir`)."
  (load-plugins)
  (let ((*error-handler* (lambda (err socket)
                           (unless (typep err 'as:tcp-info)
                             (when (and socket (typep socket 'as:socket))
                               (let* ((socket-data (as:socket-data socket))
                                    (response (getf socket-data :response)))
                                 ;; if we've got a response object and an error hasn't been sent yet, send
                                 ;; one. this will fix 99.99% of client hanging. the other 0.01% has yet to
                                 ;; be discovered.
                                 (when (and response
                                            (not (response-finished-p response))
                                            (not (typep err 'auth-failed)))
                                   (let ((body (format nil "There was an error processing your request~a"
                                                       (if *display-errors*
                                                           (format nil ": ~a" err)
                                                           "."))))
                                     (send-response response :status 500 :body body)))))
                             ;; let the guy looking at the logs see.
                             (vom:error "UNcaught error: ~a" err)))))
    (funcall (intern "DEF-DIRECTORY-ROUTE" :wookie-plugin-core-directory-router) "/" asset-dir)
    ;; catch-all handler
    (defroute (:* ".*") (req res)
      (send-response res :status 404 :body "Not found!"))
    (as:with-event-loop (:catch-app-errors t)
      (let ((listener (make-instance 'listener :bind bind :port port)))
        (start-server listener)))))

