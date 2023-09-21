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


(deftest age-str
  (cl-package-locks:with-packages-unlocked (cl)
    (let ((old (symbol-function 'get-universal-time)))
      (setf (symbol-function 'get-universal-time)
            #'(lambda () 3900000000))

      (testing "diff 0 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 20 :second 0))
                     "0 minutes")))

      (testing "diff 1 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 19 :second 59))
                     "0 minutes")))

      (testing "diff 60 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 19 :second 0))
                     "1 minute")))

      (testing "diff 61 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 18 :second 59))
                     "1 minute")))

      (testing "diff 119 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 18 :second 1))
                     "1 minute")))

      (testing "diff 120 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 21 :minute 18 :second 0))
                     "2 minutes")))

      (testing "diff 3600 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 20 :minute 20 :second 0))
                     "60 minutes")))

      (testing "diff 7198 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 19 :minute 20 :second 2))
                     "119 minutes")))

      (testing "diff 7199 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 19 :minute 20 :second 1))
                     "1 hour")))

      (testing "diff 7200 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 2
                                               :hour 19 :minute 20 :second 0))
                     "2 hours")))

      (testing "diff 86399 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 1
                                               :hour 21 :minute 20 :second 1))
                     "23 hours")))

      (testing "diff 86400 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 8 :day 1
                                               :hour 21 :minute 20 :second 0))
                     "1 day")))

      (testing "diff 172800 sec"
        (ok (string= (age-str (clsql:make-time :year 2023 :month 7 :day 31
                                               :hour 21 :minute 20 :second 0))
                     "2 days")))

      (setf (symbol-function 'get-universaltime)
              old))))


(deftest sanitize
  (testing "int"
    (ok (null (sanitize "" 'int)))
    (ok (eql (sanitize "10" 'int) 10))
    (ok (eql (sanitize "10.1" 'int) 10))
    (ok (eql (sanitize "100page" 'int) 100))

    (ok (eql (sanitize "10" 'int '(20 30 40)) 10)))

  (testing "string"
    (ok (string= (sanitize "" 'string) ""))
    (ok (string= (sanitize "param" 'string) "param"))
    (ok (string= (sanitize "param2" 'string '("param1" "param2" "param3")) "param2"))
    (ok (null (sanitize "param4" 'string '("param1" "param2" "param3")))))

  (testing "sym"
    (ok (eql (sanitize "" 'sym) :||))
    (ok (eql (sanitize "param" 'sym) :PARAM))
    (ok (eql (sanitize "param2" 'sym '(:PARAM1 :PARAM2 :PARAM3)) :PARAM2))
    (ok (null (sanitize "param4" 'sym '(:PARAM1 :PARAM2 :PARAM3))))))


(deftest add-rlist
  (pass "add-rlist is not used"))


(deftest decode-user-url
  (ok (equal (multiple-value-list (decode-user-url "/user/tamu/"))
             '("tamu" "")))
  (ok (equal (multiple-value-list (decode-user-url "/user/tamu/basic"))
             '("tamu" "basic")))

  (ok (equal (multiple-value-list (decode-user-url "/blog/latest"))
             '(NIL))))


(deftest 2weeks
  (skip "2weeks = 60sec * 60min * 24hour * 7day * 2week -> 1,209,600"))

(deftest -2weeks
  (skip "2weeks = 60sec * 60min * 24hour * 7day * 2week -> 1,209,600"))

(deftest 5minutes
  (skip "5minutes = 60sec * 5min -> 300"))

(deftest set-cookie-list
  (skip "can't test"))

(deftest add-to-cookie-list
  (skip "can't test"))

(deftest get-cookie-list
  (skip "can't test"))

(deftest good-nytimes-p
  (skip "nytimes changed"))

(deftest nytimes-link-p
  (skip "nytimes changed"))

(deftest good-nytimes
  (skip "nytimes changed"))

(deftest nytimes-safe-url
  (skip "nytimes changed"))


(deftest base-url
  (testing "https://www.google.com without last '/'"
    (ok (string= (base-url "https://www.google.com")
                 "google.com")))

  (testing "https://www.google.com/ with last '/'"
    (ok (string= (base-url "https://www.google.com/")
                 "google.com")))

  (testing "https://subdomain.example.com/ without www"
    (ok (string= (base-url "https://subdomain.example.com")
                 "subdomain.example.com")))

  (testing "https://subdomain.example.com without www and last '/'"
    (ok (string= (base-url "https://subdomain.example.com/")
                 "subdomain.example.com")))

  (testing "https://www.subsub.sub.example.com"
    (ok (string= (base-url "https://www.subsub.sub.example.com")
                 "subsub.sub.example.com"))))

(deftest add-http
  (skip "http:// deprecated, use add-https"))

(deftest add-https
  (testing "http, https exists"
    (ok (string= (add-https "http://example.com")
                 "http://example.com"))
    (ok (string= (add-https "https://example.com")
                 "https://example.com")))
  (testing "http, https not exists"
    (ok (string= (add-https "example.com")
                 "https://example.com"))))

(deftest makestr
  (ok (string= (makestr)
               ""))
  (ok (string= (makestr "a")
               "a"))
  (ok (string= (let ((id 3))
                 (makestr "id=" id))
               "id=3")))

(deftest key-str
  (ok (string= (key-str)
               ""))
  (ok (string= (key-str "key")
               "key"))
  (ok (string= (key-str "key" "012")
               "key-012"))
  (ok (string= (key-str "this key" "234" "567")
               "this_key-234-567")))

(deftest esc-quote
  (ok (string= (esc-quote "this is a pen")
               "this is a pen"))
  (ok (string= (esc-quote "I say 'this is a pen'")
               "I say &#039;this is a pen&#039;")))

(deftest shorten-str
  (testing "string > length"
    (ok (string= (shorten-str "this is a pen" 3)
                 "thi")))
  (testing "string = length"
    (ok (string= (shorten-str "this is a pen" 13)
                 "this is a pen")))
  (testing "string < length"
    (ok (string= (shorten-str "this is a pen" 20)
                 "this is a pen"))))


(deftest when-bind
  (ok (expands '(when-bind (val 10)
                 (+ val 20))
               `(let ((val 10))
                  (when val
                    (+ val 20))))))

(deftest when-bind*
  (testing "when binding result has no NIL, returns evaluated body"
    (ok (eql (when-bind* ((a 1)
                         (b (+ a 2))
                         (c (+ b 3)))
              (+ a b c))
             10)))
  (testing "when binding result has NIL, returns NIL"
    (ok (null (when-bind* ((a 1)
                           (b NIL)
                           (c (+ a 1)))
                (+ a c))))))

(deftest with-parameters
  (ok (expands '(with-parameters ((username "username")
                                  (password "password"))
                 (login-check username password))
               `(let ((username (or (hunchentoot:post-parameter "username")
                                    (hunchentoot:get-parameter "username")))
                      (password (or (hunchentoot:post-parameter "password")
                                    (hunchentoot:get-parameter "password"))))
                  (login-check username password)))))
