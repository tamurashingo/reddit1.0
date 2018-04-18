;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit-db-asd
  (:use :cl :asdf))
(in-package :reddit-db-asd)

(defsystem reddit-db
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:hunchentoot
               :cl-who
               :clsql
               :clsql-postgresql
               :trivial-http)
  :components ((:module "src"
                :components
                ((:file "view-defs")
                 (:file "data" :depends-on ("logging" "view-defs" "util"))
                 (:file "logging")
                 (:file "util")))
               (:module "db"
                :components
                ((:file "migration"))))
  :description "database migration")

