(asdf:defsystem wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:alexandria
               #:cl-async 
               #-(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:chunga
               #:http-parse
               #:puri
               #:local-time
               #:do-urlencode
               #:cl-fad)
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   (:file "config" :depends-on ("package"))
   (:file "route" :depends-on ("config"))
   (:file "plugin" :depends-on ("config"))
   (:file "hook" :depends-on ("config"))
   (:file "request-response" :depends-on ("config"))
   (:file "listener" :depends-on ("request-response" "route" "hook" "plugin"))))
