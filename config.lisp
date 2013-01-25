(defpackage :wookie-config
  (:use :cl)
  (:export #:+log-emerg+
           #:+log-alert+
           #:+log-crit+
           #:+log-err+
           #:+log-warning+
           #:+log-notice+
           #:+log-info+
           #:+log-debug+
           #:*log-level*

           #:*hide-version*
           #:*tmp-file-store*))
(in-package :wookie-config)

;; define our logging constants. follows syslog.
(defconstant +log-emerg+ 0)
(defconstant +log-alert+ 1)
(defconstant +log-crit+ 2)
(defconstant +log-err+ 3)
(defconstant +log-warning+ 4)
(defconstant +log-notice+ 5)
(defconstant +log-info+ 6)
(defconstant +log-debug+ 7)

(defvar *log-level* +log-warning+
  "Wookie's log level. Default is +log-warning+.")

(defvar *hide-version* nil
  "Boolean specifying whether or not to hide the Wookie version in the Server
   header.")

(defvar *tmp-file-store* (asdf:system-relative-pathname :wookie #p"upload-tmp/")
  "Stores the path to where uploads/temporary files go.")

