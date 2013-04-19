(wookie:defplugin wookie-plugin-core-http-var
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2.1"
  :description "A plugin that makes accessing GET/POST/multipart form vars easy."
  :depends-on (#:wookie)
  :components
  ((:file "http-var")))
