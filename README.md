Wookie
======
<img src="http://orthecreedence.github.com/wookie/wookie-smile_small.jpg"
     alt="Wookie accepts our servitude."
     align="left" />
Wookie is an asynchronous HTTP server written in common lisp. It is in beta at
the moment. Wookie is named after my dog who is extremely tempermental, 
image-conscious, and makes sounds like a wookie when you squeeze him. He views
himself as royalty, and we are all forever in his debt for his invaluable gift
of existing on the same planet as us. This project is dedicated to him.

Originally, the goal was to port Hunchentoot to async, but Wookie took a
divergent turn and is now its own project.

Wookie requires git versions of: [cl-libevent2](/orthecreedence/cl-libevent2),
[cl-async](/orthecreedence/cl-async), [http-parse](/orthecreedence/http-parse) 
<br><br><br><br>

Documentation
-------------
Wookie's documentation lives on [the Wookie website](http://orthecreedence.github.com/wookie).
It's a work in progress, but should take shape at a steady pace.

Example(s)
----------
```common-lisp
(defpackage :wookie-test
  (:use :cl :wookie-plugin-export))
(in-package :wookie-test)

;; loads the core plugins.
(wookie:load-plugins)

;; set up a vhost that contains some routes. vhosts should come before other
;; (non-vhost routes) or the more general routes will take precedence
(wookie:with-vhost "sarcastic-ass.com"
  (wookie:defroute (:get "/") (req res)
    (wookie:send-response res :body "Welcome to sarcastic-ass.com, wow SUPER glad you're here."))

  (wookie:defroute (:get "/contact") (req res)
    (wookie:send-response res :body "Thanks SOOO much for trying to contact us. Can't WAIT to hear from you..."))

  ;; def-directory-route is part of the package wookie-plugin-export. it maps a
  ;; local directory to a web path and lists files in directories (and also
  ;; serves files)
  (def-directory-route "/" "/srv/www/sarcastic-ass/assets")

  ;; define a catch-all for this vhost (any page that's not found will return a
  ;; 404 page.
  (wookie:defroute (:get ".+") (res res)
    (wookie:send-response res :status 404 :body "GREAT job, wow, you found a page that doesn't exist.")))

;; define a non-vhost homepage route
(wookie:defroute (:get "/") (req res)
  (wookie:send-response res :body "This is my web server."))

;; we can capture URL variables via regex groups (in the "args" param list).
;; let's do some streamed/chunked output as well.
(wookie:defroute (:get "/albums/([0-9]+)") (req res args)
  (let* ((album-id (car args))
         (album (get-album-by-id album-id)))
    (let* ((chunk-stream (wookie:start-response res))
           (char-stream (flexi-streams:make-flexi-stream chunk-stream)))
      (format char-stream "Album-name: ~a~%" (getf album :name))
      (format char-stream "Album-date: ~a~%" (getf album :date))
      (wookie:finish-response res))))

;; allow streaming file uploads
(wookie:defroute (:post "/files" :chunk t) (req res)
  (let ((s3-uploader nil))
    (wookie:with-chunking req (chunk finishedp)
      ;; this body is called whenever a chunk comes in on the request. this allows
      ;; your app to stream HTTP content somewhere else (like amazon S3 or
      ;; something).
      (unless s3-uploader
        (setf s3-uploader (my-app:make-s3-uploader)))
      (my-app:send-data-to-s3 s3-uploader chunk)
      ;; was the last chunk sent?
      (when finishedp
        (my-app:finish-s3-upload s3-uploader)
        (wookie:send-response res :body "File uploaded!")))))

;; start the server...
(as:start-event-loop
  (lambda ()
    (let ((listener (make-instance 'wookie:listener :post 8090)))
      (wookie:start-server listener)))
  :catch-app-errors t)
```

License
-------
Wookie is MIT licensed, but only under the terms that you swear unconditional
compliance and servitude to the dog, Wookie, and accept him as your king.
