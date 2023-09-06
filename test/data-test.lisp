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
  (testing "insert article"
    ;; prepare
    (add-user "tamu-article-1" "tamu-article-1@example.com" "password1" "127.0.0.1")

    ;; do
    (insert-article "this is yahoo japan"
                    "https://www.yahoo.co.jp"
                    (valid-user-p "tamu-article-1")
                    "127.0.0.1")

    ;; validate
    (ok (not (null (reddit.data::article-id-from-url "https://www.yahoo.co.jp"))))
    (ok (not (null (get-article "https://www.yahoo.co.jp"))))
    (ok (not (null (reddit.data::get-mod-article (valid-user-p "tamu-article-1")
                                                 (reddit.data::article-id-from-url "https://www.yahoo.co.jp")))))
    (ok (not (null (reddit.data::get-like-site (valid-user-p "tamu-article-1")
                                               (reddit.data::article-id-from-url "https://www.yahoo.co.jp"))))))

  (testing "insert same article"
    ;; prepare
    (add-user "tamu-article-2" "tamu-article-2@example.com" "password2" "127.0.0.1")
    (add-user "tamu-article-3" "tamu-article-3@example.com" "password3" "127.0.0.1")

    ;; do
    (insert-article "yahoo news"
                    "https://news.yahoo.co.jp"
                    (valid-user-p "tamu-article-2")
                    "127.0.0.1")

    ;; validate
    (ok (null (insert-article "yahoo news is cool"
                              "https://news.yahoo.co.jp"
                              (valid-user-p "tamu-article-2")
                              "127.0.0.1"))))

  (testing "remove article"
    ;; prepare
    (add-user "tamu-article-4" "tamu-article-4@example.com" "password4" "127.0.0.1")

    ;; do
    (insert-article "search engine"
                    "https://www.google.com"
                    (valid-user-p "tamu-article-4")
                    "127.0.0.1")

    ;; pre-validate
    (ok (not (null (reddit.data::article-id-from-url "https://www.google.com"))))

    ;; do remove
    (reddit.data::remove-article (valid-user-p "tamu-article-4")
                                 (reddit.data::article-id-from-url "https://www.google.com"))

    ;; validate
    (ok (null (reddit.data::article-id-from-url "https://www.google.com")))))


(deftest neuterd
  (testing "neuterd by user"
    ;; prepare
    (add-user "tamu-neuterd-1" "tamu-neuterd-1@example.com" "password1" "127.0.0.1")
    (clsql:query (format NIL "insert into neuter (userid) values (~A)"
                         (valid-user-p "tamu-neuterd-1")))

    ;; do
    (insert-article "reddit programming"
                    "https://www.reddit.com/r/programming/"
                    (valid-user-p "tamu-neuterd-1")
                    "127.0.0.1")

    ;; validate
    (ok (null (reddit.data::article-id-from-url "https://www.reddit.com/r/programming/"))))

  (testing "neuterd by ip address"
    ;; prepare
    (add-user "tamu-neuterd-2" "tamu-neuterd-2@example.com" "password2" "127.0.0.1")
    (clsql:query "insert into neuter (ip) values ('192.168.1.1')")

    ;; do
    (insert-article "reddit lisp"
                    "https://www.reddit.com/r/lisp/"
                    (valid-user-p "tamu-neuterd-2")
                    "192.168.1.1")

    ;; validate
    (ok (null (reddit.data::article-id-from-url "https://www.reddit.com/r/lisp/")))))


(deftest options
  (testing "option not registered"
    ;; prepare

    ;; do
    (setf opt (reddit.data::get-user-options 1))

    ;; validate default value
    (ok (not (null opt)))
    (ok (eql (reddit.view-defs:options-userid opt) 1))
    (ok (not (null (reddit.view-defs:options-promoted opt))))
    (ok (null (reddit.view-defs:options-demoted opt)))
    (ok (eql (reddit.view-defs:options-numsites opt) 25))
    (ok (null (reddit.view-defs:options-visible opt)))
    (ok (null (reddit.view-defs:options-frame opt))))

  (testing "option registered"
    ;; prepare
    (clsql:query "insert into options (userid, numsites, promoted, demoted, visible, frame) values (2, 30, false, true, true, true)")

    ;; do
    (setf opt2 (reddit.data::get-user-options 2))

    ;; validate
    (ok (not (null opt2)))
    (ok (eql (reddit.view-defs:options-userid opt2) 2))
    (ok (null (reddit.view-defs:options-promoted opt2)))
    (ok (not (null (reddit.view-defs:options-demoted opt2))))
    (ok (eql (reddit.view-defs:options-numsites opt2) 30))
    (ok (not (null (reddit.view-defs:options-visible opt2))))
    (ok (not (null (reddit.view-defs:options-frame opt2))))))

(deftest profile-visible
  (testing "no registered user"
    (ok (null (reddit.data::profile-visible 3000))))

  (testing "registered user: visiblle: t"
    ;; prepare
    (clsql:query "insert into options (userid, visible) values (3001, true)")

    ;; do

    ;; validate
    (ok (not (null (reddit.data::profile-visible 3001))))))


(deftest mod-user
  (testing "no registered mod-user"
    ;; prepare

    ;; do
    (reddit.data::mod-user 4000 4100 4200 "192.168.100.101" 1)
    (setf mu1 (reddit.data::get-mod-user 4000 4100 4200))

    ;; validate
    (ok (not (null mu1)))
    (ok (eql (reddit.view-defs:moduser-amount mu1) 1))
    (ok (string= (reddit.view-defs:moduser-ip mu1) "192.168.100.101"))
    (ok (not (null (reddit.view-defs:moduser-date mu1)))))

  (testing "registered mod-user"
    ;; prepare
    ;; (clsql:"insert into mod_user (userid, article, target, date, ip, amount) values ()")

    ;; do

    ;; validate
    ))
