(in-package :wookie)

(define-condition route-error (wookie-error)
  ((resource :initarg :resource :reader route-error-resource :initform nil))
  (:report (lambda (c s) (format s "Routing error: ~s" (route-error-resource c))))
  (:documentation "Describes a general routing error."))

(define-condition route-not-found (route-error) ()
  (:report (lambda (c s) (format s "Routing error: route not found for ~s" (route-error-resource c))))
  (:documentation "Describes a route not found error."))

(defvar *routes* (make-array 0 :adjustable t :fill-pointer t) 
  "Holds all the routes for the system.") 

(defun clear-routes ()
  "Clear out all routes."
  (setf *routes* (make-array 0 :adjustable t :fill-pointer t)))

(defun make-route (method resource fn &key regex case-sensitive allow-chunking)
  "Simple wrapper to make a route object from a set of args."
  (let ((scanner (if regex
                     (cl-ppcre:create-scanner
                       (concatenate 'string "^" resource "$")
                       :case-insensitive-mode (not case-sensitive))
                     resource)))
    (list :method method
          :resource scanner
          :fn fn
          :regex regex
          :allow-chunking allow-chunking
          :resource-str resource)))

(defun find-route (method resource)
  "Given a method and a resource, find the best matching route."
  (loop for route across *routes* do
    (when (eq (getf route :method) method)
      (multiple-value-bind (matchedp matches)
          (if (getf route :regex)
              (cl-ppcre:scan-to-strings (getf route :resource) resource)
              (string= (getf route :resource) resource))
        (when matchedp
          (let* ((fn (getf route :fn))
                 (curried-fn (lambda (request response)
                               (apply fn (append (list request response)
                                                 (coerce matches 'list))))))
            (setf (getf route :curried-route) curried-fn)
            (return-from find-route route)))))))

(defun upsert-route (new-route)
  "Add a new route to the table. If a route already exists with the same method
   and resource string, it is replaced with the new one in the same position the 
   old route existed in (as to preserve routing order)."
  (let ((route-found nil)
        (resource-str (getf new-route :resource-str) )
        (method (getf new-route :method)))
    (unless (zerop (length *routes*))
      (loop for i from 0
            for route across *routes* do
        (when (and (eq (getf route :method) method)
                   (string= (getf route :resource-str) resource-str))
          (setf (aref *routes* i) route
                route-found t)
          (return))))
    (unless route-found
      (vector-push-extend new-route *routes*))
    *routes*))

(defun clear-route (method resource-str)
  "Clear out a route in the routing table."
  (setf *routes* (delete-if (lambda (route)
                              (and (eq (getf route :method) method)
                                   (string= (getf route :resource-str) resource-str)))
                            *routes*)))

(defmacro defroute ((method resource &key (regex t) (case-sensitive t) chunk)
                    (bind-request bind-response &optional bind-args)
                    &body body)
  "Defines a wookie route and pushes it into the route list."
  (let* ((new-route (gensym))
         (ignore-bind-args nil)
         (bind-args (if bind-args
                        bind-args
                        (progn
                          (setf ignore-bind-args t)
                          (gensym)))))
    `(let ((,new-route (make-route ,method ,resource
                                   (lambda (,bind-request ,bind-response &rest ,bind-args)
                                     (declare (ignorable ,bind-request))
                                     ,(when ignore-bind-args
                                        `(declare (ignore ,bind-args)))
                                     ,@body)
                                   :regex ,regex
                                   :case-sensitive ,case-sensitive
                                   :allow-chunking ,chunk)))
       (upsert-route ,new-route))))

