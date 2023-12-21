;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/recommend
  (:use :cl
        :rove
        :reddit.recommend))
(in-package :reddit-test/recommend)

(setup
 (format T "setup~%")
 (reddit.main::connect-database)
 (reddit.logging:initialize-logger (reddit.config:logger-name))
 (reddit.db.migration:rebuild))

(teardown
 (format T "teardown~%")
 (reddit.main::disconnect-database))

(deftest is-email
  (ok (reddit.recommend::is-email "test@example.com"))
  (ok (reddit.recommend::is-email "test@example.info"))
  ;; no @ character
  (ng (reddit.recommend::is-email "test.example.com"))
  ;; only one @ is allowed
  (ng (reddit.recommend::is-email "this@is@invalid.com"))
  ;; not string (runtime error)
  (ok (signals (reddit.recommend::is-email '("test@example.com")))))


(deftest tokens
  (testing "token: ,"
    ;; comma only
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc,def")))
    ;; comma + space
    (ok (equal '("abc" "def" "ghi")
               (reddit.recommend::tokens "abc, def, ghi")))
    ;; double comma
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc,,def"))))
  (testing "token: ;"
    ;; semicolon only
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc;def")))
    ;; semicolon + space
    (ok (equal '("abc" "def" "ghi")
               (reddit.recommend::tokens "abc; def; ghi")))
    ;; double semicolon
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc;;def"))))
  (testing "token: [space]"
    ;; space only
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc def")))
    ;; double space
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens "abc  def"))))
  (testing "token: [tab]"
    ;; tab only
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens (format NIL "abc~Adef" #\Tab))))
    ;; tab + space
    (ok (equal '("abc" "def" "ghi")
               (reddit.recommend::tokens (format NIL "abc~A def~A ghi" #\Tab #\Tab))))
    ;; double space
    (ok (equal '("abc" "def")
               (reddit.recommend::tokens (format NIL "abc~A~Adef" #\Tab #\Tab)))))
  (testing "token: mix"
    (ok (equal '("abc" "def" "ghi")
               (reddit.recommend::tokens "abc, def; ghi")))))


(deftest email-lst
  ;; prepare
  (reddit.data::add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1")
  (reddit.data::set-alias 1 "Mr.T" "tamu1+alias1@example.com")
  (reddit.data::set-alias 1 "tm" "tamu1+alias2@example.com")

  ;; single
  (ok (equal '("tamu1@example.com")
             (reddit.recommend::email-lst '("tamu1@example.com") (reddit.user-info::get-info 1))))

  ;; double
  (ok (equal '("tamu1@example.com" "tamu2@example.com")
             (sort
              (reddit.recommend::email-lst '("tamu1@example.com" "tamu2@example.com") (reddit.user-info::get-info 1))
              #'string<=)))

  ;; alias(single)
  (ok (equal '("tamu1+alias1@example.com" "tamu1@example.com" "tamu2@example.com")
             (sort
              (reddit.recommend::email-lst '("tamu1@example.com" "tamu2@example.com" "Mr.T") (reddit.user-info::get-info 1))
              #'string<=)))

  ;; alias(double)
  (ok (equal '("tamu1+alias1@example.com" "tamu1+alias2@example.com" "tamu1@example.com" "tamu2@example.com")
             (sort
              (reddit.recommend::email-lst '("tm" "tamu1@example.com" "tamu2@example.com" "Mr.T") (reddit.user-info::get-info 1))
              #'string<=)))

  ;; not alias
  (ok (equal '("tamu1+alias1@example.com")
             (reddit.recommend::email-lst '("Mr.T" "Mr.S") (reddit.user-info::get-info 1)))))


(deftest decode-aliases
  (ok (equal '("tamu1+alias1@example.com" "tamu1+alias2@example.com" "tamu1@example.com" "tamu2@example.com")
             (sort
              (reddit.recommend::decode-aliases "tm;tamu1@example.com;tamu2@example.com Mr.T" (reddit.user-info::get-info 1))
              #'string<=))))

