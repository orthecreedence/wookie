(asdf:defsystem wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:cl-async 
               #+(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:http-parse)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "route" :depends-on ("util"))
   (:file "plugin" :depends-on ("util"))
   (:file "hook" :depends-on ("util"))
   (:file "request-response" :depends-on ("util"))
   (:file "acceptor" :depends-on ("request-response" "route" "hook" "plugin"))))
