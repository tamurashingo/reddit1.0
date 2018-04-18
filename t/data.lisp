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

(defparameter *data1-id* (add-user "data1" "data1@sample.com" "password" "192.168.0.1"))

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

  (isnt (get-user *data1-id*)
        nil)
  (is-type (get-user *data1-id*)
           'reddit.view-defs:user))


;; valid-login-p
(subtest "vaid-login-p"
  (is (valid-login-p "data1" "password")
      *data1-id*)

  (is (valid-login-p "data1" "invalid password")
      nil))


;; valid-user-p
(subtest "valid-user-p"
  (is (valid-user-p "data1")
      *data1-id*
      "by name")

  (is (valid-user-p *data1-id*)
      *data1-id*
      "by id")

  (is (valid-user-p "data1" :return-sn T)
      "data1"
      "by name returns name")

  (is (valid-user-p *data1-id* :return-sn T)
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

(finalize)

(clsql:locally-disable-sql-reader-syntax)
