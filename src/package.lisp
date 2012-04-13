(defpackage http-response-handler
  (:use #:cl #:alexandria #:drakma)
  (:export #:handle-http-response
           #:http-error #:http-client-error #:http-server-error
           #:retryable-http-error #:retry-after
           #:body #:status #:headers #:uri #:reason-phrase
           #:retry-after #:seconds-until-retry))
