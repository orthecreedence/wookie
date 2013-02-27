(in-package :wookie)

(define-condition wookie-error ()
  ((msg :initarg :msg :reader wookie-error-msg :initform nil)
   (socket :initarg :socket :reader wookie-error-socket :initform nil))
  (:report (lambda (c s) (format s "Wookie error: ~a" (wookie-error-msg c))))
  (:documentation "Describes a basic error while processing. Meant to be extended"))

(defvar *wookie-error-handlers* (make-hash-table :test #'eq)
  "Hash table holding a mapping of error-type -> list-of-handlers.")

(defvar *wookie-error-handler-class-precedence* nil
  "Holds a list of classes, ordered most specific to most general in relation to
   each other.")

(defun update-class-precedence-list (error-table)
  "Update our class precedence list to reflect the error types present in the
   *wookie-error-handlers* hash table."
  (let ((hash-keys (loop for k being the hash-keys of error-table
                         collect k)))
    (if (<= (length hash-keys) 1)
        hash-keys
        (get-class-precedence-list hash-keys))))

(defun add-error-handler (error-type handler &key (error-table *wookie-error-handlers*))
  "Add an error handler to the Wookie system that watches for a given error type
   (route-not-found, tcp-eof, etc) and fires the added callbacks if encountered.

   error-type is a symbol describing a *condition*.
   
   The handler (for all types) is defined as:
     (lambda (event socket &optional request response) ...)
   
   Although the socket can be derived from the (request-socket) accessor, the
   request may not always be present, while the socket always is.
   
   Returns a list of the classes in the given error-table, ordered by their
   relative specificity."
  (wlog :debug "(error-handler) Add error handler ~s~%" error-type)
  ;; only bother recalculating the precedence list if we have a new key in the
  ;; hash...
  (unless (prog1 (nth-value 1 (gethash error-type error-table))
            (push handler (gethash error-type error-table)))
    (let ((precedence (update-class-precedence-list error-table)))
      ;; if we're adding a global handler, update the global precedence list
      (when (eq error-table *wookie-error-handlers*)
        (setf *wookie-error-handler-class-precedence* precedence))
      precedence)))

(defun get-class-precedence-list (classes)
  "Given a list of classes, order them by their relative specificity in relation
   to each other (most-specific first). If a tree of possible classes is given,
   they are ordered such that any common classes will be placed AFTER all
   extending have been listed.
   
   This function is mainly used for determining what order error handlers should
   be fired."
  (zsort:quicksort classes
    (lambda (a b)
      (cond ((subtypep a b)
             t)
            ((subtypep b a)
             nil)))))

(defun dispatch-event (event error-table error-table-precedence &rest handler-args)
  "Run the most appropriate error handler for the given event type. Returns true
   on a successful dispatch."
  (let ((event-type (type-of event)))
    (dolist (error-type error-table-precedence)
      ;; our event object is a match for one of the conditions we're handling!
      ;; dispatch the event and break the loop
      (when (subtypep event-type error-type)
        (dolist (handler (gethash error-type error-table))
          (apply handler handler-args))
        ;; stop at the first match, return T
        (return-from dispatch-event t)))))

