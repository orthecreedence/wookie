(in-package :wookie)

(defun get-overridden-method (request original-method)
  "Checks if there is a GET var called _method, and if so, uses it instead of
   the provided method."
  (let* ((hash (plugin-request-data :get request))
         (val (when (hash-table-p hash)
                (gethash "_method" hash))))
    (if (stringp val)
        (let ((new-method (intern (string-upcase val) :keyword)))
          ;; make sure it's copacetic
          (if (find new-method '(:get :post :delete :put :head :options :trace :connect))
              new-method
              original-method))
        original-method)))

(defun setup-parser (sock)
  "Setup a parser on a socket. This can be called multiple times on the same
   socket (usually at the end of a request) so that if another request comes in
   on the same connect, we'll have reset state =]"
  (let* ((http (make-instance 'http-parse:http-request))
         (route-path nil)
         (route nil)  ; holds the current route, filled in below once we get headers
         (route-dispatched nil)
         (request (make-instance 'request :socket sock :http http))
         (response (make-instance 'response :request request))
         (body-buffer nil)
         (body-finished-p nil))
    (setf (as:socket-data sock) (list :request request :response response))
    (labels ((dispatch-route ()
               ;; dispatch the current route, but only if we haven't already done so
               (when route-dispatched
                 (return-from dispatch-route))
               (setf route-dispatched t)
               (do-run-hooks (sock) (run-hooks :pre-route request response)
                 (flet ((run-route (route)
                          (if route
                              (let ((route-fn (getf route :curried-route)))
                                (wlog :debug "(route) Dispatch ~a: ~s~%" sock route)
                                (funcall route-fn request response)
                                ;; if route expects chunking and all body chunks
                                ;; have come in already, run the chunk callback
                                ;; with the body buffer (otherwise the route's
                                ;; body callback will never be called).
                                (let ((request-body-cb (request-body-callback request)))
                                  (when (and (getf route :allow-chunking)
                                             (getf route :buffer-body)
                                             body-buffer
                                             body-finished-p
                                             request-body-cb)
                                    (funcall request-body-cb (flex:get-output-stream-sequence body-buffer) t))))
                              (progn
                                (wlog :notice "(route) Missing route: ~s~%" route)
                                (funcall 'main-event-handler (make-instance 'route-not-found
                                                                            :resource route-path
                                                                            :socket sock) sock)
                                (return-from dispatch-route)))))
                   ;; load our route, but if we encounter a use-next-route condition,
                   ;; add the route to the exclude list and load the next route with
                   ;; the same matching criteria as before
                   (let ((route-exclude nil))
                     (block run-route
                       (loop
                         (handler-case
                           ;; run our route and break the loop if successful
                           (progn
                             (run-route route)
                             (return-from run-route))
                           ;; caught a use-next-route condition, push the current
                           ;; route onto the exclude list, load the next route, and
                           ;; try again
                           (use-next-route ()
                             (wlog :debug "(route) Next route~%")
                             (push route route-exclude)
                             (setf route (find-route (http-parse:http-method http)
                                                     route-path
                                                     :exclude route-exclude))))))))
                 (do-run-hooks (sock) (run-hooks :post-route request response)
                   nil)))
             (header-callback (headers)
               ;; if we got the headers, it means we can find the route we're
               ;; destined to use. if the route accepts chunks and the body is
               ;; chunked, run the router now so it can set up chunk listening.
               ;; otherwise, save the route for later and let the rest of the
               ;; request come in.
               (let* ((method (http-parse:http-method http))
                      (resource (http-parse:http-resource http))
                      (parsed-uri (puri:parse-uri resource))
                      (path (do-urlencode:urldecode (puri:uri-path parsed-uri) :lenientp t))
                      (host (getf headers :host)))
                 (wlog :info "(request)  ~a ~a ~s ~a ~a~%"
                       request
                       response
                       method
                       resource
                       (if host (concatenate 'string "(" host ")") ""))
                 (setf route-path path)
                 ;; save the parsed uri for plugins/later code
                 (setf (request-uri request) parsed-uri
                       (request-headers request) headers)
                 (do-run-hooks (sock) (run-hooks :parsed-headers request)
                   ;; set up some tracking/state values now that we have headers
                   ;; ALSO, check for _method var when routing.
                   (let* ((method (get-overridden-method request method))
                          (found-route (find-route method path :host host)))
                     (setf route found-route
                           (request-method request) method
                           (request-resource request) resource)
                     ;; handle "Expect: 100-continue" properly
                     (when (string= (getf headers :expect) "100-continue")
                       (if found-route
                           (unless (getf route :suppress-100)
                             (send-100-continue response))
                           (progn
                             (funcall 'main-event-handler (make-instance 'route-not-found :resource route-path :socket sock)
                                                          sock)
                             (return-from header-callback))))
                     ;; if we found a route and the route allows chunking, call the
                     ;; route now so it can set up its chunk handler before we start
                     ;; streaming the body chunks to it
                     ;;
                     ;; NOTE that we don't *need* to test if the data is actually
                     ;; chunked for a chunk-enabled route to be able to receive the
                     ;; data. if a chunk-enabled route gets called for data that
                     ;; isn't chunked, it will receive all the data for that request
                     ;; as one big chunk.
                     (when (and found-route
                                (getf found-route :allow-chunking))
                       (dispatch-route))))))
             (body-callback (chunk finishedp)
               ;; forward the chunk to the callback provided in the chunk-enabled
               ;; router
               (do-run-hooks (sock) (run-hooks :body-chunk request chunk finishedp)
                 (let ((request-body-cb (request-body-callback request)))
                   (setf body-finished-p (or body-finished-p finishedp))
                   (cond ((and request-body-cb
                               body-buffer)
                          ;; we have a body chunking callback and the body has
                          ;; been buffering. append the latest chunk to the
                          ;; buffer and send the entire buffer into the body cb.
                          ;; then, nil the buffer so we know we don't have to do
                          ;; body buffering anymore
                          (write-sequence chunk body-buffer)
                          (funcall request-body-cb (flex:get-output-stream-sequence body-buffer) body-finished-p)
                          (setf body-buffer nil))
                         (request-body-cb
                          ;; we have a chunking callback set up by the route, no
                          ;; need to do anything fancy. jsut send the chunk in.
                          (funcall request-body-cb chunk finishedp))
                         ((and (getf route :allow-chunking)
                               (getf route :buffer-body))
                          ;; we're allowing chunking through this route, we're
                          ;; allowing body buffering through this route, and the
                          ;; chunking callback hasn't been set up yet (possible
                          ;; if the client starts streaming the body before our
                          ;; :pre-route hook(s) finish their futures). create a
                          ;; body buffer if we don't have one and start saving
                          ;; our chunks to it (until our route has a chance to
                          ;; set up the chunking cb).
                          (unless body-buffer
                            (setf body-buffer (flex:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
                          (write-sequence chunk body-buffer))))))
             (finish-callback ()
               (setf body-finished-p t)
               ;; make sure we always dispatch at the end.
               (do-run-hooks (sock) (run-hooks :body-complete request)
                 (dispatch-route))))
      ;; make an HTTP parser. will be attached to the socket and will be
      ;; responsible for running all of the above callbacks directly as data
      ;; filters in from the read callback.
      (let ((parser (http-parse:make-parser
                      http
                      :header-callback #'header-callback
                      :body-callback #'body-callback
                      :finish-callback #'finish-callback)))
        ;; attach parser to socket-data so we can deref it in the read callback
        (setf (getf (as:socket-data sock) :parser) parser)))))

(defun handle-connection (sock)
  "Handles a new connection. Creates a bunch of closures that are passed into an
   http-parse parser which decide amongst themselves, during different points in
   the parsing, when to dispatch to the found router, when to send chunked
   content to the route, etc."
  (wlog :debug "(connect) ~a~%" sock)
  ;; TODO pass client address info into :connect hook
  (do-run-hooks (sock) (run-hooks :connect sock)
    (setup-parser sock)))

(defun read-data (sock data)
  "A simple read-cb handler that passed data to the HTTP parser attached to the
   socket the data is coming in on. The parser runs all necessary callbacks
   directly, so this function just blindly feeds the data in."
  (wlog :debug "(read) ~a: ~a bytes~%" sock (length data))
  ;; grab the parser stored in the socket and pipe the data into it
  (let ((parser (getf (as:socket-data sock) :parser)))
    (funcall parser data)))


