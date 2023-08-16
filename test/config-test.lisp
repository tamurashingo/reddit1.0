;;;;
;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/config
  (:use :cl
        :rove
        :reddit.config))
(in-package :reddit-test/config)

(deftest set-default-config
  (testing "docker"
    (set-docker-config)
    (ok (equal reddit.config::*default-config* reddit.config::*docker*)))
  (testing "development"
    (set-development-config)
    (ok (equal reddit.config::*default-config* reddit.config::*development*))))

(deftest config-value
  (testing "original value"
    ;;
    ;; prepare
    ;;
    (let ((*test-config*
            '(:environment "test"
              :database (:type :postgresql
                         :database "reddit"
                         :server "db"
                         :port "5432"
                         :username "pgsql"
                         :password "pgcwip42:")
              :memcached (:server "memcached"
                          :port 11211)
              :mail (:server "mailserver"
                     :port 1025
                     :username "username"
                     :password "password")
              :logger (:logger-name "reddit-logger-stdout"))))
      (set-config *test-config*)

      ;; check environment name
      (ok (string= (environment-name) "test"))

      ;; check database
      (ok (string= "POSTGRESQL" (symbol-name (database-type-name))))
      (ok (equal '("db" "reddit" "pgsql" "pgcwip42:" "5432") (database-connection-string)))
      ;; check memcached
      (ok (string= (memcached-server) "memcached"))
      (ok (eql (memcached-port) 11211))
      ;; check mail
      (ok (string= (mail-server) "mailserver"))
      (ok (string= (mail-username) "username"))
      (ok (string= (mail-password) "password"))
      ;; check logger
      (ok (string= (logger-name) "reddit-logger-stdout")))))


