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



