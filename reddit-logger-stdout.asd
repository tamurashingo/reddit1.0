#|
  Logging library for reddit1.0

  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage reddit-logger-stdout-asd
  (:use :cl :asdf))
(in-package :reddit-logger-stdout-asd)

(defsystem reddit-logger-stdout
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:reddit
               :hunchentoot)
  :components ((:module "src/logging"
                :components
                ((:file "stdout"))))
  :description "Logging library by format")
