#|
  Logging library for reddit1.0

  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage reddit-logger-hunchentoot-asd
  (:use :cl :asdf))
(in-package :reddit-logger-hunchentoot-asd)

(defsystem reddit-logger-hunchentoot
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:reddit
               :hunchentoot)
  :components ((:module "src/logging"
                :components
                ((:file "hunchentoot"))))
  :description "Logging library by hunchentoot")
