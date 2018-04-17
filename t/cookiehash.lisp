(in-package :cl-user)
(defpackage reddit-test.cookiehash
  (:use :cl
        :reddit.cookiehash
        :prove))
(in-package :reddit-test.cookiehash)

(plan nil)


;; prepare
(reddit.data:add-user "cookiehash1" "cookiehash@sample.com" "password" "192.168.0.1")

(defparameter *cookiehash-user1* (cookie-str "cookiehash1" "password"))
(defparameter *cookiehash-user2* (cookie-str "cookiehash2" "password"))


;; valid cookiehash
(isnt (valid-cookie *cookiehash-user1*)
      nil)

;; invalid user
(is (valid-cookie *cookiehash-user2*)
    nil)


(finalize)
