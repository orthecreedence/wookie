Wookie
======
<img src="http://orthecreedence.github.com/wookie/wookie-smile_small.jpg"
     alt="Wookie accepts our servitude."
     align="left" />
Wookie is an asynchronous HTTP server written in common lisp. It is not for
public use, yet. It's named after my dog who is extremely tempermental, 
image-conscious, and makes sounds like a wookie when you squeeze him. He views
himself as royalty, and we are all forever in his debt for his invaluable gift
of existing on the same planet as us. This project is dedicated to him.

Wookie borrows ideas from the excellent Hunchentoot. Originally, the goal was to
port Hunchentoot to async, but Wookie took a divergent turn and is now its own
project.

Wookie requires git versions of: [cl-libevent2](/orthecreedence/cl-libevent2),
[cl-async](/orthecreedence/cl-async), [http-parse](/orthecreedence/http-parse) 
<br><br><br><br><br>

For the brave
-------------
```common-lisp
(ql:quickload :wookie)  ; make sure it's in your ASD path or local-projects/

(defpackage :wookie-test
  (:use :cl :wookie-plugin-export))
(in-package :wookie-test)

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

Wookie is barely even a prototype at this point. Follow the issues list for a
TODO overview.

License
-------
Wookie is MIT licensed, but only under the terms that you swear unconditional
compliance and servitude to the dog, Wookie, and accept him as your king.
