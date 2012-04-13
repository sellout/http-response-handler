(defpackage http-response-handler.system
  (:use #:cl #:asdf))

(in-package #:http-response-handler.system)

(defsystem http-response-handler
  :depends-on (alexandria drakma)
  :pathname "src/"
  :components ((:file "package")
               (:file "http-response-handler" :depends-on ("package"))))
