(defpackage :wookie-config
  (:use :cl)
  (:export #:*debug-on-error*
           #:*max-body-size*
           #:*hide-version*
           #:*enabled-plugins*
           #:*tmp-file-store*))
(in-package :wookie-config)

(defvar *debug-on-error* nil
  "If T, will not catch errors but instead allow them to go to the debugger.")

(defvar *max-body-size* (* 1024 1024 2)
  "The max HTTP body size in bytes that Wookie will store for each request,
   assuming the request is set to store the body (see request-store-body).")

(defvar *hide-version* nil
  "Boolean specifying whether or not to hide the Wookie version in the Server
   header.")

(defvar *enabled-plugins* '(:get :post :multipart :http-var :cookie :directory-router)
  "A list of (keyword) names of enabled plugins.")

(defvar *tmp-file-store* (asdf:system-relative-pathname :wookie #p"upload-tmp/")
  "Stores the path to where uploads/temporary files go.")

