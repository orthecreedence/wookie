(defpackage :wookie-config
  (:use :cl)
  (:export #:+log-levels+
           #:*log-level*
           #:*log-output*

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

(defvar *hide-version* nil
  "Boolean specifying whether or not to hide the Wookie version in the Server
   header.")

(defvar *enabled-plugins* '(:get :post :multipart :cookie :directory-router)
  "A list of (keyword) names of enabled plugins.")

(defvar *tmp-file-store* (asdf:system-relative-pathname :wookie #p"upload-tmp/")
  "Stores the path to where uploads/temporary files go.")

