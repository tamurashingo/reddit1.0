;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/data
  (:use :cl
        :rove
        :reddit.data))
(in-package :reddit-test/data)

(setup
  (reddit.config:set-config '(:environment "test"
                              :database (:type :postgresql
                                         :database "reddit_test"
                                         :server "127.0.0.1"
                                         :port "5432"
                                         :username "pgsql"
                                         :password "pgcwip42:")))
  (reddit.main::connect-database))


(teardown
  (reddit.main::distonnect-database)
  (format T "ok"))
