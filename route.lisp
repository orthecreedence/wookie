(in-package :wookie)

(defvar *routes* nil
  "Holds all the routes for the system.") 

(defun make-route (method resource fn)
  "Simple wrapper to make a route object from a set of args."
  (list :method method :resource resource :fn fn))

(defmacro defroute (method resource (bind-http bind-reply) &body body)
  "Defines a wookie route and pushes it into the route list."
  `(push (make-route ,method ,resource
                     (lambda (,bind-http ,bind-reply)
                       ,@body))
         *routes*))

(defun find-route (method resource)
  "Given a method and a resource, find the best matching route."
  (let ((route (find-if (lambda (route)
                          (and (eq (getf route :method) method)
                               (string= (getf route :resource) resource)))
                        *routes*)))
    (when route
      (getf route :fn))))
