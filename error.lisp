(in-package :wookie)

(define-condition wookie-error ()
  ((msg :initarg :msg :reader wookie-error-msg :initform nil)
   (socket :initarg :socket :reader wookie-error-socket :initform nil))
  (:report (lambda (c s) (format s "Wookie error: ~a" (wookie-error-msg c))))
  (:documentation "Describes a basic error while processing. Meant to be extended."))

(defun main-event-handler (event socket)
  "Handle socket events/conditions that crop up during processing."
  (let* ((socket-data (when socket (as:socket-data socket)))
         (request (getf socket-data :request))
         (response (getf socket-data :response))
         (response-finished (when response (response-finished-p response)))
         (handled nil))
    ;; don't dispatch/log an EOF on a finished request/response (nobody cares)
    (when (and response
               (response-finished-p response)
               (subtypep (type-of event) 'as:tcp-eof))
      (return-from main-event-handler))

    ;; make sure tcp-info events get a lower log level. they suck.
    (if (typep event 'as:tcp-info)
        (log:debu1 "(event) ~a (~a)" event socket)
        (log:debug "(event) ~a (~a)" event socket))

    (unwind-protect
      (if (or (functionp *error-handler*)
              (ignore-errors (symbol-function *error-handler*)))
          ;; we have an error handler, call it with our error
          (funcall *error-handler* event socket)

          ;; no, no error handler, let's do some basic handling of our own
          (handler-case (error event)
            (route-not-found ()
              (when (and response (not response-finished))
                (send-response response :status 404 :body "Route for that resource not found =[.")))
            (wookie-error ()
              (when (and response (not response-finished))
                (send-response response
                               :status 500
                               :body (format nil "There was an error processing your request: ~a" event))))
            (as:tcp-eof ()
              ;; a simple "do nothing"
              nil)
            (t ()
              ;; unhandled, send it packing to the REPL
              (when (and response (not response-finished))
                (send-response response
                               :status 500
                               :body (format nil "There was an error processing your request: ~a" event)))
              (error event))))
      ;; no matter what, clear out the data for EOF sockets (poor man's garbage
      ;; collection)
      (when (and (typep event 'as:tcp-eof)
                 (typep (as:tcp-socket event) 'as:socket))
        (setf (as:socket-data (as:tcp-socket event)) nil)))))

(defun listener-event-handler (ev)
  "A wrapper around main-event-handler, useful for listeners to tie into."
  (let* ((event-type (type-of ev))
         (sock (cond ((subtypep event-type 'response-error)
                      (request-socket (response-request (response-error-response ev))))
                     ((subtypep event-type 'wookie-error)
                      (wookie-error-socket ev))
                     ((subtypep event-type 'as:tcp-info)
                      (as:tcp-socket ev)))))
    (funcall 'main-event-handler ev sock)))

