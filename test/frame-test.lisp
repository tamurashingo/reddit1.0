;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/frame
  (:use :cl
        :rove
        :reddit.frame))
(in-package :reddit-test/frame)

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

(deftest reddit-frame
  ;; prepare
  (reddit.data:add-user "tamu1" "tamu1@example.com" "password1" "127.0.0.1")
  (reddit.data::insert-article "yahoo news"
                               "https://news.yahoo.co.jp"
                               (reddit.data::valid-user-p "tamu1")
                               "192.168.100.101")
  (ok (string=
       (reddit-frame (reddit.data::get-article "https://news.yahoo.co.jp"))
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">

<html>
  <head>
    <title>yahoo news
    </title>
  </head>
  <frameset framespacing='0' rows='30px, 100%'>
    <frame frameborder='0' scrolling='no' src='/toolbar?id=1' />
    <frame frameborder='0' src='https://news.yahoo.co.jp' />
  </frameset>
</html>")))


(deftest reddit-toolbar
  ;; prepare
  (reddit.data:add-user "tamu2" "tamu2@example.com" "password2" "192.168.100.102")
  (setf *user2* (reddit.data::valid-user-p "tamu2"))
  (reddit.data::insert-article "google"
                               "https://www.google.com"
                               *user2*
                               "192.168.100.102")
  (setf *article2* (reddit.data::get-article "https://www.google.com"))

  (testing "logged-in-p T"
    (cl-package-locks:with-packages-unlocked (reddit.user-info hunchentoot)
      (let ((old-logged-in-p (symbol-function 'reddit.user-info:logged-in-p))
            (old-post-parameter (symbol-function 'hunchentoot:post-parameter))
            (old-uid (symbol-function 'reddit.user-info:uid)))
        (setf (symbol-function 'reddit.user-info:logged-in-p)
              #'(lambda () T))
        (setf (symbol-function 'hunchentoot:post-parameter)
              #'(lambda (x) (format NIL "~A" *user2*)))
        (setf (symbol-function 'reddit.user-info:uid)
              #'(lambda () *user2*))

        ;; do
        (reddit-toolbar)

        ;; validate
        (ok T)

        (setf (symbol-function 'reddit.user-info:uid)
              old-uid)
        (setf (symbol-function 'hunchentoot:post-parameter)
              old-post-parameter)
        (setf (symbol-function 'reddit.user-info:logged-in-p)
              old-logged-in-p))))

  (testing "logged-in-p NIL"
    (cl-package-locks:with-packages-unlocked (reddit.user-info hunchentoot)
      (let ((old-logged-in-p (symbol-function 'reddit.user-info:logged-in-p))
            (old-post-parameter (symbol-function 'hunchentoot:post-parameter))
            (old-uid (symbol-function 'reddit.user-info:uid)))
        (setf (symbol-function 'reddit.user-info:logged-in-p)
              #'(lambda () nil))
        (setf (symbol-function 'hunchentoot:post-parameter)
              #'(lambda (x) (format NIL "~A" *user2*)))
        (setf (symbol-function 'reddit.user-info:uid)
              #'(lambda () *user2*))

        ;; do
        (reddit-toolbar)

        ;; validate
        (ok T)

        (setf (symbol-function 'reddit.user-info:uid)
              old-uid)
        (setf (symbol-function 'hunchentoot:post-parameter)
              old-post-parameter)
        (setf (symbol-function 'reddit.user-info:logged-in-p)
              old-logged-in-p)))))

