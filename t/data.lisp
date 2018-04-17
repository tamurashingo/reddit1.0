(in-package :cl-user)
(defpackage reddit-test.data
  (:use :cl
        :reddit.data
        :prove)
  (:import-from :reddit.config
                :set-environment))
(in-package :reddit-test.data)


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
           'user)

  (isnt (get-user *data1-id*)
        nil)
  (is-type (get-user *data1-id*)
           'user))


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


(finalize)
