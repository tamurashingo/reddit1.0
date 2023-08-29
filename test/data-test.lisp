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
  (reddit.main::connect-database)
  (reddit.logging:initialize-logger (reddit.config:logger-name))
  (clsql:start-sql-recording :type :both)
  (reddit.db.migration:rebuild))


(teardown
  (clsql:stop-sql-recording :type :both)
  (reddit.main::disconnect-database))

(deftest register-user-and-check
  (testing "register normal user"
    ;; prepare
    (add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1")

    ;; validate
    (ok (valid-login-p "tamu1" "password1"))
    (ok (valid-user-p "tamu1"))
    (ng (fake-user-p "tamu1")))

  (testing "register fake user"
    ;; prepare
    (add-user "tamu2" "tamu2@example.com" "password2" "127.0.0.1" T)

    ;; validate
    (ok (valid-login-p "tamu2" "password2"))
    (ok (valid-user-p "tamu2"))
    (ok (fake-user-p "tamu2")))

  (testing "change fake user to normal user"
    ;; prepare
    (add-user "tamu3" "tamu3@example.com" "password3" "127.0.0.1" T)
    (add-user "tamu3" "tamu3@example.com" "password3" "127.0.0.1")

    ;; validate
    (ok (valid-login-p "tamu3" "password3"))
    (ok (valid-user-p "tamu3"))
    (ng (fake-user-p "tamu3"))))


(deftest article
  (testing "article"
    ;; prepare
    (add-user "tamu-article-1" "tamu-article-1@example.com" "password1" "127.0.0.1")

    ;; do
    (insert-article "this is yahoo japan"
                    "https://www.yahoo.co.jp"
                    (valid-user-p "tamu-article-1")
                    "127.0.0.1")

    ;; validate
    (ok (not (null (reddit.data::article-id-from-url "https://www.yahoo.co.jp"))))
    (ok (not (null (get-article "https://www.yahoo.co.jp"))))))

