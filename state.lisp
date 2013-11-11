(in-package :wookie)

;;; This file defines and instantiates Wookie's global state handler. This is
;;; used throughout Wookie to store hook functions, routes, plugin state, etc.
;;; The having all state in one variable makes it a lot easier to thread Wookie
;;; without worrying about threads bumping into each other.

(defclass wookie-state ()
  ((hooks
     :accessor wookie-state-hooks
     :initarg :hooks
     :initform (make-hash-table :size 10 :test #'eq)
     :documentation "Holds the hook callbacks associated with this context.")
   (plugins
     :accessor wookie-state-plugins
     :initarg :plugins
     :initform (make-hash-table :test #'eq)
     :documentation "Holds the loaded plugins and their associated data for this context")
   (plugin-config
     :accessor wookie-state-plugin-config
     :initarg :plugin-config
     :initform nil
     :documentation "Holds all plugin configuration.")
   (routes
     :accessor wookie-state-routes
     :initarg :routes
     :initform (make-array 0 :adjustable t :fill-pointer t)
     :documentation "Holds the routes this context uses."))
  (:documentation
    "wookie-state holds all global data/state used by Wookie. It's purpose is to
     make threading Wookie easier by allowing the declaration of one
     thread-local variable instad of many."))

(defvar *state* (make-instance 'wookie-state)
  "Holds all global state/context for Wookie.")

