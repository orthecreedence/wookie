Wookie
======
<img src="http://orthecreedence.github.com/wookie/wookie-smile_small.jpg"
     alt="Wookie accepts our servitude."
     align="left" />
Wookie is an asynchronous HTTP server written in common lisp. It is in beta at
the moment, with many features you probably want unimplemented for now. Wookie
is completely undocumented at the moment, although as its API solidifies this
will be remedied. Wookie is named after my dog who is extremely tempermental, 
image-conscious, and makes sounds like a wookie when you squeeze him. He views
himself as royalty, and we are all forever in his debt for his invaluable gift
of existing on the same planet as us. This project is dedicated to him.

Originally, the goal was to port Hunchentoot to async, but Wookie took a
divergent turn and is now its own project.

Wookie requires git versions of: [cl-libevent2](/orthecreedence/cl-libevent2),
[cl-async](/orthecreedence/cl-async), [http-parse](/orthecreedence/http-parse) 
<br><br><br>

For the brave
-------------
```common-lisp
(ql:quickload :wookie)  ; make sure it's in your ASD path or local-projects/

(defpackage :wookie-test
  (:use :cl :wookie-plugin-export))
(in-package :wookie-test)

(setf wookie:*log-level* wookie:+log-info+)  ; a good level between debug and warn
(wookie:load-plugins)  ; loads GET, POST, multipart handlers (see wookie-plugins/)
(wookie:clear-routes)

(wookie:defroute (:get "/") (req res)
  (wookie:send-response res :body "Hello!"))

(wookie:defroute (:put "/albums/([0-9]+)") (req res args)
  (wookie:send-response res :body (format nil "Album ~a updated!" (car args))))

(as:start-event-loop
  (lambda ()
    (let ((listener (make-instance 'wookie:listener :port 8090)))
      (wookie:start-server listener)))
  :catch-app-errors t)
```

License
-------
Wookie is MIT licensed, but only under the terms that you swear unconditional
compliance and servitude to the dog, Wookie, and accept him as your king.
