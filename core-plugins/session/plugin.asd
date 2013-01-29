(defplugin wookie-plugin-core-session
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.1"
  :description "A session plugin for Wookie"
  :depends-on (#:wookie #:wookie-plugin-core-cookie)
  :components
  ((:file "session")))

