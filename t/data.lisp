(in-package :cl-user)
(defpackage reddit-test.data
  (:use :cl
        :reddit.data
        :prove)
  (:import-from :reddit.config
                :set-environment))
(in-package :reddit-test.data)

(clsql:locally-enable-sql-reader-syntax)

(plan nil)

(defparameter *user-data1-id* (add-user "data1" "data1@sample.com" "password" "192.168.0.1"))

;; user-pass
(subtest "user-pass"
  (is (user-pass "data1")
      "password"))


;; get-user
(subtest "get-user"
  (isnt (get-user "data1")
        nil)
  (is-type (get-user "data1")
           'reddit.view-defs:user)

  (isnt (get-user *user-data1-id*)
        nil)
  (is-type (get-user *user-data1-id*)
           'reddit.view-defs:user))


;; valid-login-p
(subtest "vaid-login-p"
  (is (valid-login-p "data1" "password")
      *user-data1-id*)

  (is (valid-login-p "data1" "invalid password")
      nil))


;; valid-user-p
(subtest "valid-user-p"
  (is (valid-user-p "data1")
      *user-data1-id*
      "by name")

  (is (valid-user-p *user-data1-id*)
      *user-data1-id*
      "by id")

  (is (valid-user-p "data1" :return-sn T)
      "data1"
      "by name returns name")

  (is (valid-user-p *user-data1-id* :return-sn T)
      "data1"
      "by id returns name")

  (is (valid-user-p "data2")
      nil)
  (is (valid-user-p "data2" :return-sn T)
      nil)
  (is (valid-user-p 999999)
      nil)
  (is (valid-user-p 999999 :return-sn T)
      nil))

;; add-user
(subtest "add-user"
  (diag "add normal user")
  ;; fake: nil
  ;; valid-user-p: nil
  (add-user "add-user-1" "add-user-1@sample.com" "password" "192.168.0.1")
  (let ((user (get-user "add-user-1")))
    (is (reddit.view-defs:user-name user)
        "add-user-1")
    (is (reddit.view-defs:user-emai user)
        "add-user-1@sample.com")
    (is (slot-value user 'reddit.view-defs::password)
        "password")
    (is (reddit.view-defs:user-karma user)
        0)
    (is (reddit.view-defs:user-ip user)
        "192.168.0.1"))


  (diag "add fake user")
  ;; fake: t
  ;; valid-user-p: nil
  (add-user "add-user-2" "add-user-2@sample.com" "password2" "192.168.0.2" T)
  (let ((user (get-user "add-user-2")))
    (is (reddit.view-defs:user-name user)
        "add-user-2")
    (is (reddit.view-defs:user-emai user)
        "add-user-2@sample.com")
    (is (slot-value user 'reddit.view-defs::password)
        "password2")
    (is (reddit.view-defs:user-karma user)
        0)
    (is (reddit.view-defs:user-ip user)
        "192.168.0.2")
    (ok (< (reddit.view-defs:user-id user) 0)))

  (diag "override fake user to normal user")
  ;; fake: nil
  ;; valid-user-p: t
  ;; fake-user-p: t
  (add-user "add-user-2" "add-user-2-fake@sample.com" "password-fake" "192.168.0.3")
  (let ((user (get-user "add-user-2"))
        (old-user (get-user "add-user-22")))
    (is (reddit.view-defs:user-name user)
        "add-user-2")
    (is (reddit.view-defs:user-emai user)
        "add-user-2-fake@sample.com")
    (is (slot-value user 'reddit.view-defs::password)
        "password-fake")
    (is (reddit.view-defs:user-karma user)
        0)
    (is (reddit.view-defs:user-ip user)
        "192.168.0.3")
    (ok (> (reddit.view-defs:user-id user) 0))

    (is (reddit.view-defs:user-name old-user)
        "add-user-22")
    (is (reddit.view-defs:user-emai old-user)
        "add-user-2@sample.com")
    (is (slot-value old-user 'reddit.view-defs::password)
        "password2")
    (is (reddit.view-defs:user-karma old-user)
        0)
    (is (reddit.view-defs:user-ip old-user)
        "192.168.0.2")
    (ok (< (reddit.view-defs:user-id old-user) 0)))

  (diag "user already registered")
  ;; fake: nil
  ;; valid-user-p: t
  ;; vake-user-p: nil
  (handler-case (add-user "add-user-1" "add-user-1-error@sample.com" "error" "192.168.0.4")
    (error () (ok "user already exists"))
    (:no-error () (fail "no signal"))))



;; insert-article
(defparameter *article-user-id* (add-user "article-user" "article@sample.com" "password" "192.168.0.1"))
(subtest "insert-article"
  (diag "normal pattern")
  (let ((article (insert-article "title" "http://sample.com/1.html" *article-user-id* "192.168.0.1")))
    (isnt article nil)
    (let ((article-db (get-article (reddit.view-defs:article-id article))))
      (is (reddit.view-defs:article-url article-db)
          "http://sample.com/1.html")
      (is (reddit.view-defs:article-title article-db)
          "title")
      (is (reddit.view-defs:article-submitterid article-db)
          *article-user-id*)))

  (diag "another user calls insert-article")
  (let* ((another-user (add-user "article-user2" "article-2@sample.com" "password" "192.168.0.2"))
         (article (insert-article "title-2" "http://sample.com/1.html" another-user "192.168.0.2")))
    (is article nil))

  (diag "title is nil")
  (let ((article (insert-article nil "http://sample.com/2.html" *article-user-id* "192.168.0.3")))
    (is article nil))

  (diag "url is nil")
  (let ((article (insert-article "title-3" nil *article-user-id* "192.168.0.4")))
    (is article nil))

  (diag "submitter is nil")
  (let ((article (insert-article "title-4" "http://sample.com/3.html" nil "192.168.0.5")))
    (is article nil))

  (diag "ip is nil")
  (let ((article (insert-article "title-5" "http://sample.com/4.html" *article-user-id* nil)))
    (is article nil))

  (diag "exist fake user")
  (add-user "article-fake-user" "article-fake-user@sample.com" "password" "192.168.0.6" T)
  (let ((article (insert-article "title-6" "http://sample.com/6.html" *article-user-id* "192.168.0.0.6" "article-fake-user")))
    (isnt article nil)
    (is (reddit.view-defs:article-url article)
        "http://sample.com/6.html")
    (is (reddit.view-defs:article-title article)
        "title-6")
    (ok (< (reddit.view-defs:article-submitterid article) 0)))

  (diag "exist same url")
  (let ((article (insert-article "title-7" "http://sample.com/1.html" *article-user-id* "192.168.0.1")))
    (is article nil))
  (let ((article (insert-article "title-7" "https://sample.com/1.html" *article-user-id* "192.168.0.1")))
    (is article nil))
  (let ((article (insert-article "title-7" "http://www.sample.com/1.html" *article-user-id* "192.168.0.1")))
    (is article nil))
  (let ((article (insert-article "title-7" "https://www.sample.com/1.html" *article-user-id* "192.168.0.1")))
    (is article nil)))


;; get-article
(defparameter *article-id* (reddit.view-defs:article-id (insert-article "get-article-test" "http://sample.com/get-article.html" *article-user-id* "192.168.0.1")))
(subtest "get-article"
  (diag "get-article by id")
  (let ((article (get-article *article-id*)))
    (isnt article nil)
    (is (reddit.view-defs:article-url article)
        "http://sample.com/get-article.html")
    (is (reddit.view-defs:article-title article)
        "get-article-test")
    (is-type (reddit.view-defs:article-submitter article)
             'reddit.view-defs:user)
    (is (reddit.view-defs:user-name (reddit.view-defs:article-submitter article))
        "article-user"))

  (diag "get-article by url")
  (let ((article (get-article "http://sample.com/get-article.html")))
    (isnt article nil)
    (is (reddit.view-defs:article-url article)
        "http://sample.com/get-article.html")
    (is (reddit.view-defs:article-title article)
        "get-article-test")
    (is-type (reddit.view-defs:article-submitter article)
             'reddit.view-defs:user)
    (is (reddit.view-defs:user-name (reddit.view-defs:article-submitter article))
        "article-user")))

;; remove-article
(subtest "remove-article"
  (remove-article *article-user-id* *article-id*)
  (let ((article (get-article *article-id*)))
    (is article nil)))

;; neuter
(subtest "neuterd"
  (let ((neutered-user-id (add-user "neutered-user" "neutered@sample.com" "password" "192.168.0.1")))

    (insert-article "neutered-article" "http://sample.com/neutered-article.html" neutered-user-id "192.168.1.1")

    (is (reddit.data::neuterd neutered-user-id) nil)
    (is (reddit.data::neuterd "192.168.1.1") nil)

    (diag "user-id")
    (clsql:insert-records :into [neuter]
                          :attributes '(userid)
                          :values (list neutered-user-id))

    (isnt (reddit.data::neuterd neutered-user-id) nil)

    (clsql:delete-records :from [neuter])

    (diag "ip-address")
    (clsql:insert-records :into [neuter]
                          :attributes '(ip)
                          :values '("192.168.1.1"))

    (isnt (reddit.data::neuterd "192.168.1.1") nil)))

;; options
(subtest "options"
  ;; delete all options records
  (clsql:delete-records :from [options])

  (diag "create new instance when no records")
  (let ((option (get-user-options 1)))
    (is-type option 'reddit.view-defs:options)
    (is (slot-value option 'reddit.view-defs::userid) 1)
    (is (slot-value option 'reddit.view-defs::numsites) 25)
    (is (slot-value option 'reddit.view-defs::promoted) t)
    (is (slot-value option 'reddit.view-defs::demoted) nil)
    (is (slot-value option 'reddit.view-defs::visible) nil)
    (is (slot-value option 'reddit.view-defs::frame) nil))

  (clsql:insert-records :into [options]
                        :attributes '(userid numsites promoted demoted visible frame)
                        :values (list 2     ; userid
                                      30    ; numsites
                                      nil   ; promoted
                                      t     ; demoted
                                      t     ; visible
                                      t     ; frame
                                      ))

  (diag "new instance from records")
  (let ((option (get-user-options 2)))
    (is-type option 'reddit.view-defs:options)
    (is (slot-value option 'reddit.view-defs::userid) 2)
    (is (slot-value option 'reddit.view-defs::numsites) 30)
    (is (slot-value option 'reddit.view-defs::promoted) nil)
    (is (slot-value option 'reddit.view-defs::demoted) t)
    (is (slot-value option 'reddit.view-defs::visible) t)
    (is (slot-value option 'reddit.view-defs::frame) t))

  (diag "profile-visible")
  (is (profile-visible 1) nil)
  (is (profile-visible 2) t))


;; mod-user
;; skip tests because internal use only

;; mod-article
;; skip tests because internal use only


;; click on a link
(subtest "view-link"
  (let* ((user-id (add-user "click-user" "click-user@example.com" "password" "192.168.0.1"))
         (article (insert-article "title" "http://www.yahoo.co.jp/news.html" user-id "192.168.0.1")))

    ;; no-click
    (is (reddit.data::user-clicked-p user-id (reddit.view-defs:article-id article)) nil)
    ;; click 1
    (view-link user-id (reddit.view-defs:article-id article) "192.168.0.1")
    (is (reddit.data::user-clicked-p user-id (reddit.view-defs:article-id article)) t)
    ;; click 2
    (view-link user-id (reddit.view-defs:article-id article) "192.168.0.1")
    (is (reddit.data::user-clicked-p user-id (reddit.view-defs:article-id article)) t)))


;; like-site
(subtest "like-site"
  (let* ((user-id (add-user "like-user" "like-user@example.com" "password" "192.168.0.1"))
         (article (insert-article "like-user-article" "http://www.yahoo.co.jp/like-site.html" user-id "192.168.0.1"))
         (article-id (reddit.view-defs:article-id article)))
    ;; check get-like-site returns a record
    (is (reddit.view-defs::like-like (reddit.data::get-like-site user-id article-id)) t)

    ;; unlike-site
    (reddit.data::unlike-site user-id article-id)

    ;; check get-like-site returns no record
    (is (reddit.data::get-like-site user-id article-id) nil)))


(finalize)

(clsql:locally-disable-sql-reader-syntax)
