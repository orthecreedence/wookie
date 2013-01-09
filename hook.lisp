(in-package :wookie)

(defvar *hooks* (make-hash-table :size 10 :test #'eq))

(defun clear-hooks (&optional hook)
  "Clear all hooks (default) or optionally a specific hook type."
  (if hook
      (setf (gethash hook *hooks*) nil)
      (setf *hooks* (make-hash-table :size 10 :test #'eq))))

(defun run-hooks (hook &rest args)
  "Run all hooks of a specific type."
  (let ((hooks (gethash hook *hooks*)))
    (dolist (hook hooks)
      (apply (getf hook :function) args))))

(defun add-hook (hook function &optional hook-name)
  "Add a hook into the wookie system. Hooks will be run in the order they were
   added."
  ;; append instead of push since we want them to run in the order they were added
  (alexandria:appendf (gethash hook *hooks*)
                      (list :function function
                            :name hook-name)))

(defun remove-hook (hook function/hook-name)
  "Remove a hook from a set of hooks by its function reference OR by the hook's
   name given at add-hook."
  (when (and function/hook-name
             (gethash hook *hooks*))
    (setf (gethash hook *hooks*) (remove-if (lambda (hook)
                                              (let ((fn (getf hook :function))
                                                    (name (getf hook :name)))
                                                (or (eq fn function/hook-name)
                                                    (eq name function/hook-name))))
                                            (gethash hook *hooks*)))))

