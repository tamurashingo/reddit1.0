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

