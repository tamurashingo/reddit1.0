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
    (clsql:query "insert into mod_user (userid, article, target, date, ip, amount) values (4001, 4101, 4202, now(), '192.168.100.102', 2)")

    ;; do
    (reddit.data::mod-user 4001 4101 4201 "192.168.100.103" 5)
    (setf mu2 (reddit.data::get-mod-user 4001 4101 4201))

    ;; validate
    (ok (not (null mu2)))
    (ok (eq (reddit.view-defs:moduser-amount mu2) 5))
    (ok (string= (reddit.view-defs:moduser-ip mu2) "192.168.100.103"))
    (ok (not (null (reddit.view-defs:moduser-date mu2))))))

(deftest check-and-mod-user
  (testing "mod other's article"
    ;; prepare
    ;; register user1
    (add-user "mod-tamu1" "mod-tamu1@example.com" "password1" "127.0.0.1")
    ;; register user2
    (add-user "mod-tamu2" "mod-tamu2@example.com" "password2" "127.0.0.1")
    (let ((user2 (reddit.data::get-user "mod-tamu2")))
      (setf (slot-value user2 'reddit.view-defs::karma) 20)
      (clsql:update-records-from-instance user2))

    ;; insert article
    (insert-article "HN"
                    "https://news.ycombinator.com/"
                    (valid-user-p "mod-tamu1")
                    "127.0.0.1")

    ;; do
    (reddit.data::check-and-mod-user (valid-user-p "mod-tamu2")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/")
                                     "192.168.100.104"
                                     10)
    (setf mu3 (reddit.data::get-mod-user (valid-user-p "mod-tamu2") ; mod-user
                                         (valid-user-p "mod-tamu1") ; article-submitter
                                         (reddit.data::article-id-from-url "https://news.ycombinator.com/"))) ; article

    (format T "~A" (clsql:query "select * from mod_user"))


    ;; validate
    (ok (not (null mu3)))
    (ok (eq (reddit.view-defs:moduser-amount mu3) 10))
    (ok (string= (reddit.view-defs:moduser-ip mu3) "192.168.100.104")))

  (testing "mod own article -> no-mod"
    ;; prepare
    (add-user "mod-tamu3" "mod-tamu3@example.com" "password3" "127.0.0.1")
    (let ((user3 (reddit.data::get-user "mod-tamu3")))
      (setf (slot-value user3 'reddit.view-defs::karma) 20)
      (clsql:update-records-from-instance user3))

    ;; insert article
    (insert-article "HN newest"
                    "https://news.ycombinator.com/newest"
                    (valid-user-p "mod-tamu3")
                    "127.0.0.1")

    ;; do
    (reddit.data::check-and-mod-user (valid-user-p "mod-tamu3")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/newest")
                                     "192.168.100.105"
                                     20)
    (setf mu4 (reddit.data::get-mod-user (valid-user-p "mod-tamu3")
                                         (valid-user-p "mod-tamu3")
                                         (reddit.data::article-id-from-url "https://news.ycombinator.com/newest")))

    ;; validate
    (ok (null mu4)))

  (testing "mod 3days ago article -> no-mod"
    ;; prepare
    (add-user "mod-tamu4" "mod-tamu4@example.com" "password4" "127.0.0.1")
    (add-user "mod-tamu5" "mod-tamu5@example.com" "password5" "127.0.0.1")
    (let ((user5 (reddit.data::get-user "mod-tamu5")))
      (setf (slot-value user5 'reddit.view-defs::karma) 20)
      (clsql:update-records-from-instance user5))

    ;; insert article
    (insert-article "HN past"
                    "https://news.ycombinator.com/front"
                    (valid-user-p "mod-tamu4")
                    "127.0.0.1")
    (let ((a (get-article "https://news.ycombinator.com/front")))
      (setf (reddit.view-defs:article-date a) (clsql:time- (clsql:get-time)
                                                           (clsql:make-duration :day 2 :second 1)))
      (clsql:update-records-from-instance a))

    ;; do
    (reddit.data::check-and-mod-user (valid-user-p "mod-tamu5")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/front")
                                     "192.168.100.105"
                                     20)
    (setf mu5 (reddit.data::get-mod-user (valid-user-p "mod-tamu5")
                                         (valid-user-p "mod-tamu4")
                                         (reddit.data::article-id-from-url "https://news.ycombinator.com/front")))

    ;; validate
    (ok (null mu5)))

  (testing "user karma less than equal 10 -> no-mod"
    ;; prepare
    (add-user "mod-tamu6" "mod-tamu6@example.com" "password6" "127.0.0.1")
    (add-user "mod-tamu7" "mod-tamu7@example.com" "password7" "127.0.0.1")
    (let ((user7 (reddit.data::get-user "mod-tamu7")))
      (setf (slot-value user7 'reddit.view-defs::karma) 10)
      (clsql:update-records-from-instance user7))

    ;; insert article
    (insert-article "HN comments"
                    "https://news.ycombinator.com/newcomments"
                    (valid-user-p "mod-tamu6")
                    "127.0.0.1")

    ;; do
    (reddit.data::check-and-mod-user (valid-user-p "mod-tamu7")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/newcomments")
                                     "192.168.100.107"
                                     30)
    (setf mu6 (reddit.data::get-mod-user (valid-user-p "mod-tamu7")
                                         (valid-user-p "mod-tamu6")
                                         (reddit.data::article-id-from-url "https://news.ycombinator.com/newcomments")))

    ;; validate
    (ok (null mu6))))


(deftest mod-article
  (testing "no registered mod_article"
    ;; prepare

    ;; do
    (reddit.data::mod-article 5001 5101 "192.168.100.001" 10)
    (setf ma1 (reddit.data::get-mod-article 5001 5101))

    ;; validate
    (ok (not (null ma1)))
    (ok (eql (reddit.view-defs:modarticle-amount ma1) 10))
    (ok (string= (reddit.view-defs:modarticle-ip ma1) "192.168.100.001")))

  (testing "registered mod_article"
    ;; prepare
    (clsql:query "insert into mod_article (userid, article, date, ip, amount) values (5002, 5102, now(), '192.168.100.001', 10)")

    ;; do
    (reddit.data::mod-article 5002 5102 "192.168.100.002" 20)
    (setf ma2 (reddit.data::get-mod-article 5002 5102))

    ;; validate
    (ok (not (null ma2)))
    (ok (eql (reddit.view-defs:modarticle-amount ma2) 20))
    (ok (string= (reddit.view-defs:modarticle-ip ma2) "192.168.100.002"))))


(deftest check-and-mod-article
  ;;; same as mod-article
  (testing "no registered mod_article"
    ;; prepare

    ;; do
    (reddit.data::mod-article 6001 6101 "192.168.100.001" 10)
    (setf cma1 (reddit.data::get-mod-article 6001 6101))

    ;; validate
    (ok (not (null cma1)))
    (ok (eql (reddit.view-defs:modarticle-amount cma1) 10))
    (ok (string= (reddit.view-defs:modarticle-ip cma1) "192.168.100.001")))

  (testing "registered mod_article"
    ;; prepare
    (clsql:query "insert into mod_article (userid, article, date, ip, amount) values (6002, 6102, now(), '192.168.100.001', 10)")

    ;; do
    (reddit.data::mod-article 6002 6102 "192.168.100.002" 20)
    (setf cma2 (reddit.data::get-mod-article 6002 6102))

    ;; validate
    (ok (not (null cma2)))
    (ok (eql (reddit.view-defs:modarticle-amount cma2) 20))
    (ok (string= (reddit.view-defs:modarticle-ip cma2) "192.168.100.002"))))


(deftest view-link
  (testing "valid user clicks"
    ;; prepare
    (add-user "tamu8" "tamu8@example.com" "password8" "127.0.0.1")
    (insert-article "HN ask"
                    "https://news.ycombinator.com/ask"
                    (valid-user-p "tamu8")
                    "127.0.0.1")


    ;; do
    (reddit.data::view-link (valid-user-p "tamu8")
                            (reddit.data::article-id-from-url "https://news.ycombinator.com/ask")
                            "192.168.100.101")

    ;; validate
    (ok (reddit.data::user-clicked-p (valid-user-p "tamu8")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/ask"))))

  (testing "invalid user clicks"
    ;; prepare
    (add-user "tamu9" "tamu9@example.com" "password9" "127.0.0.1")
    (insert-article "HN show"
                    "https://news.ycombinator.com/show"
                    (valid-user-p "tamu9")
                    "127.0.0.1")
    (clsql:query "insert into neuter (ip) values ('192.168.200.100')")

    ;; do
    (reddit.data::view-link (valid-user-p "tamu9")
                            (reddit.data::article-id-from-url "https://news.ycombinator.com/show")
                            "192.168.200.100")

    ;; validate
    (ng (reddit.data::user-clicked-p (valid-user-p "tamu9")
                                     (reddit.data::article-id-from-url "https://news.ycombinator.com/show")))))


(deftest like-site
  (testing "first like-site"
    ;; prepare

    ;; do
    (reddit.data::like-site 1001 2001 T)
    (setf ls1 (reddit.data::get-like-site 1001 2001))
    (setf lsu1 (reddit.data::like-site-user 1001 2001))

    ;; validate
    (ok (not (null ls1)))
    (ok (eql (reddit.view-defs:like-userid ls1) 1001))
    (ok (eql (reddit.view-defs:like-articleid ls1) 2001))
    (ok (reddit.view-defs:like-like ls1))
    (ok (eq lsu1 :like)))

  (testing "second like-site"
    ;; prepare

    ;; do
    (reddit.data::like-site 1001 2001 nil)
    (setf ls2 (reddit.data::get-like-site 1001 2001))
    (setf lsu2 (reddit.data::like-site-user 1001 2001))

    ;; validate
    (ok (not (null ls2)))
    (ok (eql (reddit.view-defs:like-userid ls2) 1001))
    (ok (eql (reddit.view-defs:like-articleid ls2) 2001))
    (ok (not (reddit.view-defs:like-like ls2)))
    (ok (eq lsu2 :dislike)))


  (testing "unlike-site"
    ;; prepare

    ;; do
    (reddit.data::unlike-site 1001 2001)
    (setf ls3 (reddit.data::get-like-site 1001 2001))
    (setf lsu3 (reddit.data::like-site-user 1001 2001))

    ;; validate
    (ok (null ls3))
    (ok (null lsu3))))

(deftest like-and-mod
  (testing "like-and-mod"
    ;; prepare
    (add-user "tamu10" "tamu10@example.com" "password10" "127.0.0.1")
    (let ((user10 (reddit.data::get-user "tamu10")))
      (setf (slot-value user10 'reddit.view-defs::karma) 20)
      (clsql:update-records-from-instance user10))

    (add-user "tamu11" "tamu11@example.com" "password11" "127.0.0.1")
    (insert-article "domestic news"
                    "https://news.yahoo.co.jp/categories/domestic"
                    (valid-user-p "tamu11")
                    "127.0.0.1")

    ;; do
    (reddit.data::like-and-mod (valid-user-p "tamu10")
                               (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")
                               T
                               "192.168.110.001")
    (setf lam1 (reddit.data::get-like-site (valid-user-p "tamu10")
                                           (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")))

    ;; validate
    (ok (not (null lam1)))
    (ok (not (null (reddit.data::get-mod-article (valid-user-p "tamu10")
                                                 (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")))))
    (ok (not (null (reddit.data::get-mod-user (valid-user-p "tamu10")
                                              (valid-user-p "tamu11")
                                              (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")))))))

(deftest unlike-and-mod
  (testing "unlik-and-mod"
    ;; prepare
    (ok (eql (car (clsql:query "select count(*) from mod_user where ip = '192.168.110.001'" :flatp T :field-names NIL)) 1))
    (ok (eql (car (clsql:query "select count(*) from mod_article where ip = '192.168.110.001'" :flatp T :field-names NIL)) 1))
    (ok (eql (car (clsql:query "select count(*) from mod_user where ip = '192.168.110.002'" :flatp T :field-names NIL)) 0))
    (ok (eql (car (clsql:query "select count(*) from mod_article where ip = '192.168.110.002'" :flatp T :field-names NIL)) 0))

    ;; do
    (reddit.data::unlike-and-mod (valid-user-p "tamu10")
                                 (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")
                                 "192.168.110.002")
    (setf uam1 (reddit.data::get-like-site (valid-user-p "tamu10")
                                           (reddit.data::article-id-from-url "https://news.yahoo.co.jp/categories/domestic")))

    ;; validate
    (ok (null uam1))
    ;; update ip address from original mod_user
    (ok (eql (car (clsql:query "select count(*) from mod_user where ip = '192.168.110.001'" :flatp T :field-names NIL)) 0))
    (ok (eql (car (clsql:query "select count(*) from mod_user where ip = '192.168.110.002'" :flatp T :field-names NIL)) 1))
    ;; update ip address from original mod_article
    (ok (eql (car (clsql:query "select count(*) from mod_article where ip = '192.168.110.001'" :flatp T :field-names NIL)) 0))
    (ok (eql (car (clsql:query "select count(*) from mod_article where ip = '192.168.110.002'" :flatp T :field-names NIL)) 1))))




(deftest alias
  (testing "alias"
    ;;
    (reddit.data::set-alias 1001 "a-username" "a-value")
    (ok (not (null (reddit.data::get-alias 1001 "a-username"))))

    (reddit.data::remove-alias 1001 "a-username")
    (ok (null (reddit.data::get-alias 1001 "a-username")))))


(deftest basic-info
  (testing "karma zero user"
    ;; prepare
    (add-user "tamu12" "tamu12@example.com" "password12" "127.0.0.1")

    ;; do
    (setf info1 (reddit.data::basic-info "tamu12"))

    ;; validate
    (ok (not (null info1)))
    (ok (string= (car info1) "0")))

  (testing "karma 10 user"
    ;; prepare
    (add-user "tamu13" "tamu13@example.com" "password13" "127.0.0.1")
    (let ((user13 (reddit.data::get-user "tamu13")))
      (setf (slot-value user13 'reddit.view-defs::karma) 10)
      (clsql:update-records-from-instance user13))


    ;; do
    (setf info2 (reddit.data::basic-info "tamu13"))

    ;; validate
    (ok (not (null info2)))
    (ok (string= (car info2) "10"))))


(deftest user-stats
  (testing "user just after register"
     ;; prepare
     (add-user "tamu14" "tamu14@example.com" "password14" "127.0.0.1")

     ;; do
     (setf stat1 (reddit.data::user-stats "tamu14"))

     ;; validate
     (ok (not (null stat1)))
     (ok (equal stat1 '("0" "0" "0"))))

  (testing "user article 3 like 2 dislike 1"
     ;; prepare
     (add-user "tamu15" "tamu15@example.com" "password15" "127.0.0.1")
     (insert-article "search a"
                     "https://www.google.com/search?q=a"
                     (valid-user-p "tamu15")
                     "127.0.0.1")
     (insert-article "search b"
                     "https://www.google.com/search?q=b"
                     (valid-user-p "tamu15")
                     "127.0.0.1")
     (insert-article "search c"
                     "https://www.google.com/search?q=c"
                     (valid-user-p "tamu15")
                     "127.0.0.1")
     (reddit.data::like-site (valid-user-p "tamu15")
                             (reddit.data::article-id-from-url "https://www.google.com/search?q=c")
                             NIL)

     ;; do
     (setf stat2 (reddit.data::user-stats "tamu15"))

     ;; validate
     (ok (not (null stat2)))
     (ok (equal stat2 '("3" "2" "1")))))


(deftest user-email
  (testing "valid user"
    (ok (string= (reddit.data::user-email "tamu15") "tamu15@example.com")))

  (testing "invalid user"
    (ok (null (reddit.data::user-email "tamu16")))))

(deftest change-password
  (testing "old password correct"
    (ok (not (null (reddit.data::change-password (valid-user-p "tamu15")
                                                 "password15"
                                                 "p@ssw0rd15")))))
  (testing "old password incorrect"
    (ok (null (reddit.data::change-password (valid-user-p "tamu15")
                                            "12345678"
                                            "qwertyui")))))

(deftest user-from-email
  (testing "valid user"
    (ok (eql (reddit.data::user-from-email "tamu15@example.com") (valid-user-p "tamu15"))))

  (testing "invalid user"
    (ok (null (reddit.data::user-from-email "tamu16@example.com")))))


(deftest change-email
  (testing "valid user"
    (reddit.data::change-email (valid-user-p "tamu15") "tamu15@example.co.jp")
    (ok (eql (reddit.data::user-from-email "tamu15@example.co.jp") (valid-user-p "tamu15"))))

  (testing "invalid user"
    (reddit.data::change-email 10001 "tamu16@example.co.jp")
    (ok (null (reddit.data::user-from-email "tamu16@example.co.jp")))))

(deftest karma
  (testing "no registered user"
    (ok (eql (reddit.data::karma 10001) 0)))

  (testing "user just after registered"
    (add-user "tamu16" "tamu16@example.com" "password16" "127.0.0.1")
    (ok (eql (reddit.data::karma (valid-user-p "tamu16")) 0)))

  (testing "karma 10 user"
    (let ((user16 (reddit.data::get-user "tamu16")))
      (setf (slot-value user16 'reddit.view-defs::karma) 10)
      (clsql:update-records-from-instance user16))
    (ok (eql (reddit.data::karma (valid-user-p "tamu16")) 10))))


(deftest login-from-email
  (testing "valid user"
    (ok (equal (reddit.data::login-from-email "tamu16@example.com") '("tamu16" "password16"))))

  (testing "invalid user"
    (ok (null (reddit.data::login-from-email "tamu17@example.com")))))


(deftest top-submitters
  (testing "just run"
    (reddit.data::top-submitters 20 :day)
    (reddit.data::top-submitters 20 :week)
    (reddit.data::top-submitters 20 NIL)))

