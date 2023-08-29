(in-package :cl-user)
(defpackage :reddit-test-system
  (:use :asdf :cl))
(in-package :reddit-test-system)

;;;
;;; (asdf:test-system :reddit-test)
;;;;
(defsystem reddit-test
  :depends-on (:reddit
               :reddit-db
               :rove)
  :pathname "test"
  :components ((:file "config-test")
               (:file "memcached-test")
               (:file "data-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :reddit.config :set-test-config)
                    (uiop:symbol-call :rove :run c)))

