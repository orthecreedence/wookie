(defpackage :wookie
  (:use :cl)
  (:export #:*hide-version*
           
           #:clear-routes
           #:clear-route
           #:defroute

           #:clear-hooks
           #:add-hook
           #:remove-hook

           #:request
           #:request-method
           #:request-resource
           #:request-plugin-data
           #:request-body-callback
           #:request-http
           #:response
           #:response-headers
           #:send-response
           #:start-response
           #:finish-response

           #:acceptor
           #:start-server))

