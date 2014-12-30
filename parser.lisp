;;; This file holds the parser. It's the piece that ties *everything* together,
;;; from dispatching routes, body chunking, running a lot of essential hooks,
;;; etc.
;;;
;;; The parser is the heart of Wookie and holds most of the core logic. Most of
;;; the other pieces are icing on the cake.

(in-package :wookie)

(defun get-overridden-method (request original-method)
  "Checks if there is a GET var called _method, and if so, uses it instead of
   the provided method."
  (let* ((hash (plugin-request-data :get request))
         (val (when (hash-table-p hash)
                (gethash "_method" hash))))
    ;; if we passed in _method *and* we're not doing an OPTIONS call, replace
    ;; the HTTP method with the override
    (if (and (stringp val)
             (not (eq original-method :options)))
        (let ((new-method (intern (string-upcase val) :keyword)))
          ;; make sure it's copacetic
          (if (find new-method '(:get :post :delete :put :head :options :trace :connect))
              new-method
              original-method))
        original-method)))

(defun setup-parser (sock)
  "This is the main parser function. It's responsible for listening to a socket,
   setting up an HTTP parser for it, handling the different events/callbacks the
   HTTP parser throws at it, dispatching routes, handling body chunking, etc. A
   lot of this is done via shared state which all lives under this function's
   top-level (let) form, and a set of local functions which read/modify this
   shared state.

   This function is at the core of Wookie's being.

   Note that setup-parser can be called multiple times on the same socket. The
   only reason to do this is if a request/response has come and gone on the
   socket and you wish to make the socket available for another request. Wookie
   handles all of this automatically."
  (let* ((http (fast-http:make-http-request))
         (route-path nil)
         (route nil)  ; holds the current route, filled in below once we get headers
         (route-dispatched nil)
         (error-occurred-p nil)
         (request (make-instance 'request :socket sock :http http))
         (response (make-instance 'response :request request))
         (body-buffer (fast-io:make-output-buffer))
         (body-finished-p nil))
    (setf (as:socket-data sock) (list :request request :response response))
    (labels ((dispatch-route ()
               "Dispatch the route under `route`. This not only handles calling
                the route's main function, but also handles use-next-route
                conditions and setting up intricate logic for resilient body
                chunking. It also runs the :pre-route and :post-route hooks."
               ;; dispatch the current route, but only if we haven't already done so
               (when route-dispatched
                 (return-from dispatch-route))
               (setf route-dispatched t)
               (do-run-hooks (sock) (run-hooks :pre-route request response)
                 (flet ((run-route (route)
                          (if route
                              (let ((route-fn (getf route :curried-route)))
                                (log:debu1 "(route) Dispatch ~a: ~s" sock route)
                                (funcall route-fn request response)
                                ;; if route expects chunking and all body chunks
                                ;; have come in already, run the chunk callback
                                ;; with the body buffer (otherwise the route's
                                ;; body callback will never be called).
                                (let ((request-body-cb (request-body-callback request)))
                                  (when (and (getf route :allow-chunking)
                                             (getf route :buffer-body))
                                    (if (and body-buffer
                                             body-finished-p)
                                        ;; the body has finished processing, either
                                        ;; send it into the chunking function or set
                                        ;; up a callback that is called when
                                        ;; with-chunking is called that pumps the body
                                        ;; into the newly setup chunking callback
                                        (let ((body (fast-io:finish-output-buffer body-buffer)))
                                          (if request-body-cb
                                              ;; with-chunking already called, great. pass
                                              ;; in the body
                                              (funcall request-body-cb body t)
                                              ;; set up a callback that fires when
                                              ;; with-chunking is called. it'll pass the
                                              ;; body into the with-chunking callback
                                              ;; once set
                                              (setf (request-body-callback-setcb request) (lambda (body-cb) (funcall body-cb body t)))))
                                        ;; chunking hasn't started yet AND we haven't called
                                        ;; with-chunking yet. this is kind of an edge case,
                                        ;; but it needs to be handled. we set up a callback
                                        ;; that runs once with-chunking is called that
                                        ;; *hopes* the body has finished chunking and if so,
                                        ;; fires with-chunking with the body.
                                        (setf (request-body-callback-setcb request) (lambda (body-cb)
                                                                                      (when body-buffer
                                                                                        (let ((body (fast-io:finish-output-buffer body-buffer)))
                                                                                          (funcall body-cb body body-finished-p)))))))))
                              (progn
                                (log:warn "(route) Missing route: ~s" route)
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
                         (block next
                           (handler-bind
                               ((use-next-route
                                  ;; caught a use-next-route condition, push the current
                                  ;; route onto the exclude list, load the next route, and
                                  ;; try again
                                  (lambda (e)
                                    (declare (ignore e))
                                    (log:debu1 "(route) Next route")
                                    (push route route-exclude)
                                    (setf route (find-route (fast-http:http-method http)
                                                            route-path
                                                            :exclude route-exclude))
                                    (return-from next))))
                             ;; run our route and break the loop if successful
                             (progn
                               (run-route route)
                               (return-from run-route))))))))
                 (do-run-hooks (sock) (run-hooks :post-route request response)
                   nil)))
             (header-callback (headers)
               "Called when our HTTP parser graciously passes us a block of
                parsed headers. Allows us to find which route we're going to
                dispatch to, and if needed, set up chunking *before* the body
                starts flowing in. Responsible for the :parsed-headers hook."
               (blackbird:catcher
                 (let* ((method (fast-http:http-method http))
                        (resource (fast-http:http-resource http))
                        (parsed-uri (quri:uri resource))
                        (path (do-urlencode:urldecode (quri:uri-path parsed-uri) :lenientp t))
                        (host (get-header headers "host")))
                   (log:debug "(request)  ~a ~a ~s ~a ~a"
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
                       (when (string= (get-header headers "expect") "100-continue")
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
                         (dispatch-route)))))
                 ;; pipe all uncaught errors we get to the main event handler
                 ;; (with our socket object).
                 ;;
                 ;; note that errors that are caught here are more or less fatal
                 ;; and as such, we stop processing the request any further by
                 ;; setting `error-occurred-p` to T which notifies the other
                 ;; callbacks (body/finished) that they shall not proceed any
                 ;; further with this request.
                 (error (e)
                   (setf error-occurred-p t)
                   (main-event-handler e sock))))
             (body-callback (chunk start end)
               "Called (sometimes multiple times per request) when the HTTP
                parser sends us a chunk of body content, which mainly occurs
                during a chunked HTTP request. This function is responsible
                for calling our :body-chunk, and also for buffering body data
                if the route allows."
               (when error-occurred-p
                 (return-from body-callback))
               ;; forward the chunk to the callback provided in the chunk-enabled
               ;; router
               (do-run-hooks (sock) (run-hooks :body-chunk request chunk start end body-finished-p)
                 (let ((request-body-cb (request-body-callback request)))
                   (cond ((and request-body-cb body-buffer)
                          ;; we have a body chunking callback and the body has
                          ;; been buffering. append the latest chunk to the
                          ;; buffer and send the entire buffer into the body cb.
                          ;; then, nil the buffer so we know we don't have to do
                          ;; body buffering anymore
                          (fast-io:fast-write-sequence chunk body-buffer start end)
                          (funcall request-body-cb (fast-io:finish-output-buffer body-buffer) body-finished-p)
                          (setf body-buffer nil))
                         (request-body-cb
                          ;; we have a chunking callback set up by the route, no
                          ;; need to do anything fancy. just send the chunk in.
                          (funcall request-body-cb chunk body-finished-p))
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
                          (fast-io:fast-write-sequence chunk body-buffer start end))))))
             (finish-callback ()
               "Called when an entire HTTP request has been parsed. Responsible
                for the :body-complete hook."
               (when error-occurred-p
                 (return-from finish-callback))
               (setf body-finished-p t)
               (body-callback (make-array 0 :element-type 'cl-async:octet) 0 0)
               ;; make sure we always dispatch at the end.
               (do-run-hooks (sock) (run-hooks :body-complete request)
                 (dispatch-route))))
      ;; make an HTTP parser. will be attached to the socket and will be
      ;; responsible for running all of the above callbacks directly as data
      ;; filters in from the read callback.
      (let ((parser (fast-http:make-parser
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
  (log:debu1 "(connect) ~a" sock)
  ;; TODO pass client address info into :connect hook
  (do-run-hooks (sock) (run-hooks :connect sock)
    (setup-parser sock)))

(defun read-data (sock data)
  "A simple read-cb handler that passes data to the HTTP parser attached to the
   socket the data is coming in on. The parser runs all necessary callbacks
   directly, so this function just blindly feeds the data in."
  (log:debu1 "(read) ~a: ~a bytes" sock (length data))
  ;; grab the parser stored in the socket and pipe the data into it
  (let ((parser (getf (as:socket-data sock) :parser)))
    (funcall parser data)))

