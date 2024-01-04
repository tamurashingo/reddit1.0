;;;; Copyright 2023 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage :reddit-test/sites
  (:use :cl
        :rove
        :reddit.sites))
(in-package :reddit-test/sites)

(setup
  (format T "setup~%")
  (reddit.main::connect-database)
  (reddit.logging:initialize-logger (reddit.config:logger-name))
  (reddit.db.migration:rebuild))

(teardown
  (format T "teardown~%")
  (reddit.main::disconnect-database))


(deftest get-articles
  ;; prepare
  (let* ((u (reddit.data:add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1"))
         (a1 (reddit.data:insert-article "this is yahoo japan" "https://www.yahoo.co.jp" 1 "127.0.0.1"))
         (a2 (reddit.data:insert-article "this is google" "https://www.google.com" 1 "127.0.0.1"))
         (a3 (reddit.data:insert-article "this is github" "https://github.com" 1 "127.0.0.1"))
         *result*)

    (setf (reddit.view-defs:article-date a1) (clsql:make-time :year 2023 :month 12 :day 1)
          (reddit.view-defs:article-date a2) (clsql:make-time :year 2023 :month 12 :day 1)
          (reddit.view-defs:article-date a3) (clsql:make-time :year 2023 :month 12 :day 2))
    (clsql:update-records-from-instance a1)
    (clsql:update-records-from-instance a2)
    (clsql:update-records-from-instance a3)
    (clsql:execute-command "update articles set pop = 144 where id = 1") ; 60 * 60 * 24 / 2000 -> 43.2
    (clsql:execute-command "update articles set pop = 143 where id = 2")
    (clsql:execute-command "update articles set pop = 100 where id = 3")

    ;; do
    (setf *result* (reddit.sites::get-articles))

    ;; validate
    (ok (eql 3 (length *result*)))

    (ok (string= "this is yahoo japan" (reddit.view-defs:article-title (car *result*)))
        "24hour = 43.2pops")
    (ok (string= "this is github" (reddit.view-defs:article-title (cadr *result*)))
        "github: > 100 google: 143 - 43.2 -> 99.8")
    (ok (string= "this is google" (reddit.view-defs:article-title (caddr *result*))))))
