(defpackage :wookie
  (:use :cl :wookie-config :wookie-util)
  (:export #:*log-level*

           #:*hide-version*
           #:*tmp-file-store*
           
           #:wookie-error
           #:wookie-error-msg
           #:wookie-error-socket
           #:add-error-handler

           #:route-error
           #:route-not-found
           #:clear-routes
           #:clear-route
           #:next-route
           #:defroute
           #:with-vhost

           #:clear-hooks
           #:add-hook
           #:remove-hook

           #:response-error
           #:response-error-response
           #:response-already-sent
           #:request
           #:request-socket
           #:request-method
           #:request-resource
           #:request-headers
           #:request-uri
           #:request-plugin-data
           #:request-body-callback
           #:request-http
           #:response
           #:response-headers
           #:response-finished-p
           #:with-chunking
           #:send-response
           #:start-response
           #:finish-response
           ;#:add-request-error-handler

           #:*enabled-plugins*
           #:*plugin-folders*
           #:register-plugin
           #:plugin-config
           #:plugin-request-data
           #:load-plugins
           #:unload-plugin
           #:defplugin
           #:defplugfun
           

           #:listener
           #:listener-bind
           #:listener-port
           #:listener-backlog
           #:start-server

           #-(or :wookie-no-ssl) #:ssl-listener
           #-(or :wookie-no-ssl) #:listener-certificate
           #-(or :wookie-no-ssl) #:listener-key
           #-(or :wookie-no-ssl) #:listener-password))

