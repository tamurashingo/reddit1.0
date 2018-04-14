(in-package :cl-user)
(defpackage reddit-test.cookiehash
  (:use :cl
        :reddit.cookiehash
        :prove))
(in-package :reddit-test.cookiehash)

(plan nil)


;;; prepare

(reddit.config:set-environment :test)
(reddit.main:connect-database)

(reddit.data:add-user "cookiehash" "cookiehash@sample.com" "password" "192.168.0.1")


(defparameter *hash* (cookie-str "cookiehash" "password"))


(isnt (valid-cookie *hash*)
      nil)


(reddit.main:disconnect-database)

(finalize)
