;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/cookiehash
  (:use :cl
        :rove
        :reddit.cookiehash))
(in-package :reddit-test/cookiehash)

(setup
 (format T "setup~%")
 (reddit.main::connect-database)
 (reddit.logging:initialize-logger (reddit.config:logger-name))
 (clsql:start-sql-recording :type :both)
 (reddit.db.migration:rebuild))


(teardown
 (clsql:stop-sql-recording :type :both)
 (reddit.main::disconnect-database)
 (format T "teardown~%"))


(deftest cookie-str
  (testing "registered user"
    ;; prepare
    (reddit.data:add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1")

    (ok (not (null (cookie-str "tamu1" "password1")))))

  (testing "un-registered user"
    (ok (not (null (cookie-str "tamu2" "password2"))))))

(deftest valid-cookie
  (testing "registered user"
    (setf *cookie-1* (cookie-str "tamu1" "password1"))
    (ok (valid-cookie *cookie-1*)))

  (testing "un-registered user"
    (setf *cookie-2* (cookie-str "tamu2" "password2"))
    (ng (valid-cookie *cookie-2*))))

