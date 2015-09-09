(asdf:defsystem wookie
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.3.15"
  :description "An evented webserver for Common Lisp."
  :depends-on (#:alexandria
               #:blackbird
               #:cl-async 
               #-(or :wookie-no-ssl) #:cl-async-ssl
               #:cl-ppcre
               #:babel
               #:chunga
               #:fast-http
               #:quri
               #:do-urlencode
               #:cl-fad
               #:fast-io
               #:vom)
  :components
  ((:file "config")
   (:file "util" :depends-on ("config"))
   (:file "package" :depends-on ("util"))
   (:file "state" :depends-on ("package"))
   (:file "error" :depends-on ("package" "state"))
   (:file "route" :depends-on ("error"))
   (:file "plugin" :depends-on ("package" "state"))
   (:file "hook" :depends-on ("package" "state"))
   (:file "request-response" :depends-on ("error" "hook"))
   (:file "parser" :depends-on ("error" "request-response" "route" "hook" "plugin"))
   (:file "listener" :depends-on ("error" "request-response" "route" "hook" "plugin" "parser"))
   #-(or :wookie-no-ssl) (:file "listener-ssl" :depends-on ("listener"))
   (:file "helper" :depends-on ("listener" "plugin" "package"))))

