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
               :rove
               :cl-package-locks)
  :pathname "test"
  :components ((:file "autocompute-test")
               (:file "config-test")
               (:file "cookiehash-test")
               (:file "data-test")
               (:file "frame-test")
               (:file "memcached-test")
               (:file "recommend-test")
               (:file "search-test")
               (:file "sites-test")
               (:file "util-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :reddit.config :set-test-config)
                    (uiop:symbol-call :rove :run c)))

