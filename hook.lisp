(in-package :wookie)

(defvar *hooks* (make-hash-table :size 10 :test #'eq))

(defun clear-hooks (&optional hook)
  "Clear all hooks (default) or optionally a specific hook type."
  (wlog :debug "(hook) Clearing ~a~%" (if hook
                                               (format nil "hook ~s~%" hook)
                                               "all hooks"))
  (if hook
      (setf (gethash hook *hooks*) nil)
      (setf *hooks* (make-hash-table :size 10 :test #'eq))))

(defun run-hooks (hook &rest args)
  "Run all hooks of a specific type. Returns a future that is finished with no
   values when all hooks have successfully run. If a hook callback returns a
   future object, then run-hooks will wait for it to finish before finishing its
   own future. If multiple callbacks return futures, run-hooks waits for ALL of
   them to finish before finishing its future.
   
   This setup allows an application to add extra processing to hooks that may be
   asynchronous without blocking the event loop, and without the processing of
   the current request stampeding full steam ahead when it may need access to
   information the hook is grabbing async.
   
   For instance, let's say you want to check user auth on each request, you
   could set up a :pre-route hook that reads the request and checks the auth
   info against your database, finishing the future it returns only when the
   database has responded. Once the future is finished, then Wookie will
   continue processing the request."
  (wlog :debug "(hook) Run ~s (~a)~%" hook args)
  (let ((future (make-future))
        (hooks (gethash hook *hooks*))
        (collected-futures nil))  ; holds futures returned from hook functions
    (dolist (hook hooks)
      ;; see if a future was returned from the hook function. if so, save it.
      (let ((ret (apply (getf hook :function) args)))
        (when (futurep ret)
          (push ret collected-futures))))

    (if (null collected-futures)
        ;; no futures returned from our hook functions, so we can continue
        ;; processing our current request.
        (finish future)
        ;; we did collect futures from the hook functions, so let's wait for all
        ;; if them to finish before continuing with the current request.
        (let* ((num-futures-finished 0)
               ;; create a function that tracks how many futures have finished
               (finish-fn
                 (lambda ()
                   (incf num-futures-finished)
                   (when (<= (length collected-futures) num-futures-finished)
                     ;; all our watched futures are finished, continue the
                     ;; request!
                     (finish future)))))
          ;; watch each of the collected futures
          (future-handler-case
            (dolist (collected-future collected-futures)
              (attach collected-future finish-fn))
            ;; catch any errors while processing and forward them to the hook
            ;; runner
            ((or error simple-error) (e)
              (wlog :debug "(hook) Caught future error processing hook ~a (~a)~%" hook (type-of e))
              (signal-error future e)
              ;; clear out all callbacks/errbacks/values/etc. essentially, this
              ;; future and anything it references is gone forever.
              (reset-future future)))))
    ;; return the future that tracks when all hooks have successfully completed
    future))

(defun add-hook (hook function &optional hook-name)
  "Add a hook into the wookie system. Hooks will be run in the order they were
   added."
  (wlog :debug "(hook) Adding hook ~s ~a~%" hook (if hook-name
                                                          (format nil "(~s)" hook-name)
                                                          ""))
  ;; append instead of push since we want them to run in the order they were added
  (alexandria:appendf (gethash hook *hooks*)
                      (list (list :function function :name hook-name))))

(defun remove-hook (hook function/hook-name)
  "Remove a hook from a set of hooks by its function reference OR by the hook's
   name given at add-hook."
  (when (and function/hook-name
             (gethash hook *hooks*))
    (wlog :debug "(hook) Remove hook ~s~%" hook)
    (setf (gethash hook *hooks*) (remove-if (lambda (hook)
                                              (let ((fn (getf hook :function))
                                                    (name (getf hook :name)))
                                                (or (eq fn function/hook-name)
                                                    (eq name function/hook-name))))
                                            (gethash hook *hooks*)))))

