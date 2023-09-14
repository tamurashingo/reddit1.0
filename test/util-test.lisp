;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/util
  (:use :cl
        :rove
        :reddit.util))
(in-package :reddit-test/util)

(setup
 (format T "setup~%"))

(teardown
 (format T "teardown~%"))

(deftest website-stream
  (pass "tokenizer not used"))

(deftest website-string
  (testing "http connection"
    (website-string "http://www.example.com"))
  (testing "https connection"
    (website-string "https://www.example.com")))


(deftest website-title
  (testing "http connection"
    (ok (string= (website-title "http://www.example.com") "Example Domain")))
  (testing "https connection"
    (ok (string= (website-title "https://www.example.com") "Example Domain"))))

(deftest tl-domain
  (testing "http with www"
    (ok (string= (tl-domain "http://www.example.com") "example.com")))
  (testing "http without www"
    (ok (string= (tl-domain "http://example.com") "example.com")))
  (testing "https with www"
    (ok (string= (tl-domain "https://www.example.com") "example.com")))
  (testing "https without www"
    (ok (string= (tl-domain "https://example.com") "example.com")))
  (testing "sub domain"
    (ok (string= (tl-domain "https://subdomain.example.com") "subdomain.example.com"))))


(deftest replace-alist
  (testing "replace '() to '()(a . 1) (b . 2))"
    (setf a1 (replace-alist '()
                            '((a . 1) (b . 2))))

    (ok (eql (length a1) 2))
    (ok (equal (assoc 'a a1) '(a . 1)))
    (ok (equal (assoc 'b a1) '(b . 2))))

  (testing "original not contains new alist"
    (setf a2 (replace-alist '((a . 1) (b . 2))
                            '((c . 3))))

    (ok (eql (length a2) 3))
    (ok (equal (assoc 'a a2) '(a . 1)))
    (ok (equal (assoc 'b a2) '(b . 2)))
    (ok (equal (assoc 'c a2) '(c . 3))))

  (testing "replace one"
    (setf a3 (replace-alist '((a . 1) (b . 2))
                            '((b . 3))))

    (ok (eql (length a3) 2))
    (ok (equal (assoc 'a a3) '(a . 1)))
    (ok (equal (assoc 'b a3) '(b . 3))))


  (testing "replace and add"
    (setf a4 (replace-alist '((a . 1) (b . 2) (c . 3))
                            '((a . 4) (d . 5))))

    (ok (eql (length a4) 4))
    (ok (equal (assoc 'a a4) '(a . 4)))
    (ok (equal (assoc 'b a4) '(b . 2)))
    (ok (equal (assoc 'c a4) '(c . 3)))
    (ok (equal (assoc 'd a4) '(d . 5)))))

(deftest create-url
  (ok (string= (create-url "https://www.google.com/" '())
               "https://www.google.com/?"))
  (ok (string= (create-url "https://www.google.com/" '(("q" . "commonlisp")))
               "https://www.google.com/?q=commonlisp"))
  (ok (string= (create-url "https://www.google.com/" '(("q" . "commonlisp")
                                                       ("session" . "thisissessionid")))
               "https://www.google.com/?q=commonlisp&session=thisissessionid")))


(deftest minutes
  (ok (eql (reddit.util::minutes 0) 0))
  (ok (eql (reddit.util::minutes 1) 0))
  (ok (eql (reddit.util::minutes 59) 0))
  (ok (eql (reddit.util::minutes 60) 1))
  (ok (eql (reddit.util::minutes 61) 1))
  (ok (eql (reddit.util::minutes 119) 1))
  (ok (eql (reddit.util::minutes 120) 2))
  (ok (eql (reddit.util::minutes 121) 2)))

(deftest hours
  (ok (eql (reddit.util::hours 0) 0))
  (ok (eql (reddit.util::hours 1) 0))
  (ok (eql (reddit.util::hours 3599) 0))
  (ok (eql (reddit.util::hours 3600) 1))
  (ok (eql (reddit.util::hours 3601) 1))
  (ok (eql (reddit.util::hours 7199) 1))
  (ok (eql (reddit.util::hours 7200) 2))
  (ok (eql (reddit.util::hours 7201) 2)))


(deftest days
  (ok (eql (reddit.util::days 0) 0))
  (ok (eql (reddit.util::days 1) 0))
  (ok (eql (reddit.util::days 86399) 0))
  (ok (eql (reddit.util::days 86400) 1))
  (ok (eql (reddit.util::days 86401) 1))
  (ok (eql (reddit.util::days 172799) 1))
  (ok (eql (reddit.util::days 172800) 2))
  (ok (eql (reddit.util::days 172801) 2)))

