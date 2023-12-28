;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/search
  (:use :cl
        :rove
        :reddit.search))
(in-package :reddit-test/search)

(setup
 (format T "setup~%")
 (reddit.main::connect-database)
 (reddit.logging:initialize-logger (reddit.config:logger-name))
 (reddit.db.migration:rebuild))

(teardown
 (format T "teardown~%")
 (reddit.main::disconnect-database))

(deftest search-char
  (testing "alphabet"
    (ok (reddit.search::search-char #\A))
    (ok (reddit.search::search-char #\B))
    (ok (reddit.search::search-char #\C))
    (ok (reddit.search::search-char #\D))
    (ok (reddit.search::search-char #\E))
    (ok (reddit.search::search-char #\F))
    (ok (reddit.search::search-char #\G))
    (ok (reddit.search::search-char #\H))
    (ok (reddit.search::search-char #\I))
    (ok (reddit.search::search-char #\J))
    (ok (reddit.search::search-char #\K))
    (ok (reddit.search::search-char #\L))
    (ok (reddit.search::search-char #\M))
    (ok (reddit.search::search-char #\N))
    (ok (reddit.search::search-char #\O))
    (ok (reddit.search::search-char #\P))
    (ok (reddit.search::search-char #\Q))
    (ok (reddit.search::search-char #\R))
    (ok (reddit.search::search-char #\S))
    (ok (reddit.search::search-char #\T))
    (ok (reddit.search::search-char #\U))
    (ok (reddit.search::search-char #\V))
    (ok (reddit.search::search-char #\W))
    (ok (reddit.search::search-char #\X))
    (ok (reddit.search::search-char #\Y))
    (ok (reddit.search::search-char #\Z))

    (ok (reddit.search::search-char #\a))
    (ok (reddit.search::search-char #\b))
    (ok (reddit.search::search-char #\c))
    (ok (reddit.search::search-char #\d))
    (ok (reddit.search::search-char #\e))
    (ok (reddit.search::search-char #\f))
    (ok (reddit.search::search-char #\g))
    (ok (reddit.search::search-char #\h))
    (ok (reddit.search::search-char #\i))
    (ok (reddit.search::search-char #\j))
    (ok (reddit.search::search-char #\k))
    (ok (reddit.search::search-char #\l))
    (ok (reddit.search::search-char #\m))
    (ok (reddit.search::search-char #\n))
    (ok (reddit.search::search-char #\o))
    (ok (reddit.search::search-char #\p))
    (ok (reddit.search::search-char #\q))
    (ok (reddit.search::search-char #\r))
    (ok (reddit.search::search-char #\s))
    (ok (reddit.search::search-char #\t))
    (ok (reddit.search::search-char #\u))
    (ok (reddit.search::search-char #\v))
    (ok (reddit.search::search-char #\w))
    (ok (reddit.search::search-char #\x))
    (ok (reddit.search::search-char #\y))
    (ok (reddit.search::search-char #\z)))
  (testing "number"
    (ok (reddit.search::search-char #\0))
    (ok (reddit.search::search-char #\1))
    (ok (reddit.search::search-char #\2))
    (ok (reddit.search::search-char #\3))
    (ok (reddit.search::search-char #\4))
    (ok (reddit.search::search-char #\5))
    (ok (reddit.search::search-char #\6))
    (ok (reddit.search::search-char #\7))
    (ok (reddit.search::search-char #\8))
    (ok (reddit.search::search-char #\9)))
  (testing "quote double-quote space"
    (ok (reddit.search::search-char #\'))
    (ok (reddit.search::search-char #\"))
    (ok (reddit.search::search-char #\space)))

  (testing "ascii: non-printable char"
    (loop for c below 32
          do (ng (reddit.search::search-char (code-char c))))
    (ng (reddit.search::search-char (code-char 127))))

  (testing "ascii: other"
    (ng (reddit.search::search-char #\!))
    (ng (reddit.search::search-char #\#))
    (ng (reddit.search::search-char #\$))
    (ng (reddit.search::search-char #\%))
    (ng (reddit.search::search-char #\&))
    (ng (reddit.search::search-char #\())
    (ng (reddit.search::search-char #\)))
    (ng (reddit.search::search-char #\*))
    (ng (reddit.search::search-char #\+))
    (ng (reddit.search::search-char #\,))
    (ng (reddit.search::search-char #\-))
    (ng (reddit.search::search-char #\.))
    (ng (reddit.search::search-char #\/))
    (ng (reddit.search::search-char #\:))
    (ng (reddit.search::search-char #\;))
    (ng (reddit.search::search-char #\<))
    (ng (reddit.search::search-char #\=))
    (ng (reddit.search::search-char #\>))
    (ng (reddit.search::search-char #\?))
    (ng (reddit.search::search-char #\@))
    (ng (reddit.search::search-char #\[))
    (ng (reddit.search::search-char #\\))
    (ng (reddit.search::search-char #\]))
    (ng (reddit.search::search-char #\^))
    (ng (reddit.search::search-char #\_))
    (ng (reddit.search::search-char #\`))
    (ng (reddit.search::search-char #\{))
    (ng (reddit.search::search-char #\|))
    (ng (reddit.search::search-char #\}))
    (ng (reddit.search::search-char #\~))))

(deftest to-search-str
  (testing "normal"
    (ok (string= "dog"
                 (reddit.search::to-search-str "dog")))
    (ok (string= "this|is|a|pen"
                 (reddit.search::to-search-str "this is a pen")))
    (ok (string= "'File|Not|Found'&Java"
                 (reddit.search::to-search-str "\"File Not Found\" and Java"))))
  (testing "double quote -> quote"
    (ok (string= "this|is|a|'dog'"
                 (reddit.search::to-search-str "this is a \"dog\""))))
  (testing "multiple space -> single space"
    (ok (string= "this|is|the|dog"
                 (reddit.search::to-search-str "this is  the  dog"))))
  (testing "' and ' -> &"
    (ok (string= "cats&dogs"
                 (reddit.search::to-search-str "cats and dogs")))
    (ok (string= "a&b&c"
                 (reddit.search::to-search-str "a and b and c"))))
  (testing "remove trailing space"
    (ok (string= "this|is|a|pen"
                 (reddit.search::to-search-str "this is a pen       "))))
  (testing "remove leading space"
    (ok (string= "this|is|a|pen"
                 (reddit.search::to-search-str "    this is a pen"))))
  (testing "' or ' -> |"
    (ok (string= "cats|dogs"
                 (reddit.search::to-search-str "cats or dogs"))))
  (testing "space -> |"
    (ok (string= "cats|dogs"
                 (reddit.search::to-search-str "cats dogs")))))

(deftest search-sites
  ;; prepare
  (reddit.data:add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1")
  (reddit.data:insert-article "this is yahoo japan" "https://www.yahoo.co.jp" 1 "127.0.0.1") 

  ;; validate
  (let ((result (search-sites "yahoo")))
    (ok (eql 1 (length result)))
    (ok (string= "this is yahoo japan" (reddit.view-defs:article-title (car result)))))

  (let ((result (search-sites "google")))
    (ok (eql 0 (length result)))))

