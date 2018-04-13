;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit-test-asd
  (:use :cl :asdf))
(in-package :reddit-test-asd)

(defsystem reddit-test
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:reddit
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cookiehash"))))
  :description "Test system for reddit"
  
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
