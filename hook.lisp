(in-package :wookie)

(defvar *hooks* (make-hash-table :size 10 :test #'eq))

(defun clear-hooks (&optional hook)
  "Clear all hooks (default) or optionally a specific hook type."
  (if hook
      (setf (gethash hook *hooks*) nil)
      (setf *hooks* (make-hash-table :size 10 :test #'eq))))

(defun run-hooks (hook &rest args)
  "Run all hooks of a specific type."
  (let ((callbacks (gethash hook *hooks*)))
    (dolist (cb callbacks)
      (apply cb args))))

(defun add-hook (hook function)
  "Add a hook into the wookie system."
  (push function (gethash hook *hooks*)))
