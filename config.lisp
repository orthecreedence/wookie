(defpackage :wookie-config
  (:use :cl)
  (:export #:+log-levels+
           #:*log-level*
           #:*log-output*

           #:*error-handler*
           #:*hide-version*
           #:*enabled-plugins*
           #:*tmp-file-store*))
(in-package :wookie-config)

;; define our logging levels. follows syslog.
(defparameter +log-levels+ '(:emerg 0
                             :error 1
                             :warning 2
                             :notice 3
                             :info 4
                             :debug 5))

(defvar *log-level* :warning
  "Wookie's log level. Default is :warning. Acceptable values are
   '(:debug :info :notice :warning :error :emerg)")

(defvar *log-output* nil
  "Can hold a stream to send log messages to. If nil, sends to *standard-output*")

(defvar *error-handler* nil
  "Wookie installs its own error/event handler to the TCP server it operates on,
   but provides *error-handler* as a means to do any further error processing
   yourself.
   
   *error-handler* must either be nil, or a lambda/function that takes exactly
   one argument.
   
   If *error-handler* is not specified, Wookie re-triggers the error, otherwise
   the error is sent to the given handler and no further processing on the error
   is done by Wookie.")

(defvar *hide-version* nil
  "Boolean specifying whether or not to hide the Wookie version in the Server
   header.")

(defvar *enabled-plugins* '(:get :post :multipart :http-var :cookie :directory-router)
  "A list of (keyword) names of enabled plugins.")

(defvar *tmp-file-store* (asdf:system-relative-pathname :wookie #p"upload-tmp/")
  "Stores the path to where uploads/temporary files go.")

