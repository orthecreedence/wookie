(asdf:defsystem wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.1"
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
               #:cl-fad
               #:zsort)
  :components
  ((:file "util")
   (:file "package" :depends-on ("util"))
   (:file "config" :depends-on ("package"))
   (:file "error-handler" :depends-on ("config"))
   (:file "route" :depends-on ("error-handler"))
   (:file "plugin" :depends-on ("config"))
   (:file "hook" :depends-on ("config"))
   (:file "request-response" :depends-on ("error-handler"))
   (:file "listener" :depends-on ("request-response" "route" "hook" "plugin"))
   #-(or :wookie-no-ssl) (:file "listener-ssl" :depends-on ("listener"))))
