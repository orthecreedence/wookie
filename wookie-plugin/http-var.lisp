(uiop:define-package #:wookie/wookie-plugin/http-var
  (:documentation "A plugin that makes accessing GET/POST/multipart form vars easy.")
  (:import-from #:wookie)
  (:use #:cl #:wookie-util #:wookie #:wookie-plugin-export))
(in-package #:wookie/wookie-plugin/http-var)

(defplugfun http-var (request key &key (order '(:get :post :multipart)))
  "Get a data variable from an HTTP request. Checks GET/POST/Multipart, assuming
   those plugins are loaded."
  (flet ((check-var-type (fn)
           (when (ignore-errors (fboundp fn))
             (let ((val (funcall fn request key)))
               (when val
                 (return-from http-var val))))))
    (dolist (type order)
      (case type
        (:get (check-var-type 'wookie-plugin-export::get-var))
        (:post (check-var-type 'wookie-plugin-export::post-var))
        (:multipart (check-var-type 'wookie-plugin-export::form-var))))))

(defun init-http-var ())
(defun unload-http-var ())

(register-plugin :http-var 'init-http-var 'unload-http-var)

