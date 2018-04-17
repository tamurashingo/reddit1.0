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
(is (user-pass "data1")
    "password")


;; get-user
(isnt (get-user "data1")
      nil)
(is-type (get-user "data1")
         'user)

(isnt (get-user *data1-id*)
      nil)
(is-type (get-user *data1-id*)
         'user)


;; valid-login-p
(is (valid-login-p "data1" "password")
    *data1-id*)

(is (valid-login-p "data1" "invalid password")
    nil)


(finalize)
