;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit.config
  (:use :cl)
  (:export :config
           :set-environment))
(in-package :reddit.config)

(defparameter *config*
  '(:production
    (
     ;; environment
     :environment "production"
     
     ;; for cookie hash
     :hash-salt "blargo"

     ;; database
     :database-type :postgresql
     :database-name "reddit"
     :database-user "pgsql"
     :database-password "pgcwip42:"
     :database-server ""
     
     ;; mail
     :mail-prog "/usr/bin/mail"
     :mail-user "ZG1haWw="
     :mail-pass "Ymxhcmc="
     :mail-server "216.55.162.13"
     
     ;; memcached
     :memcached-server "127.0.0.1"
     :memcached-port 11211)

    :development
    (
     ;; environment
     :environment "development"
     
     ;; for cookie hash
     :hash-salt "blargo"

     ;; database
     :database-type :postgresql
     :database-name "reddit"
     :database-user "root"
     :database-password "password"
     :database-server ""

     ;; mail
     :mail-prog "/usr/bin/mail"
     :mail-user "test"
     :mail-pass "test"
     :mail-server "127.0.0.1"

     ;; memcached
     :memcached-server "127.0.0.1"
     :memcached-port 11211)

    :test
    (
     ;; environment
     :environment "test"
     
     ;; for cookie hash
     :hash-salt "blargo"

     ;; database
     :database-type :postgresql
     :database-name "reddit"
     :database-user "root"
     :database-password "password"
     :database-server ""

     ;; mail
     :mail-prog "/usr/bin/mail"
     :mail-user "test"
     :mail-pass "test"
     :mail-server "127.0.0.1"

     ;; memcached
     :memcached-server "127.0.0.1"
     :memcached-port 11211)))
    
(defparameter *environment* :production)

(defun set-environment (env)
  (setf *environment* env))


(defun config ()
  (getf *config* *environment*))
