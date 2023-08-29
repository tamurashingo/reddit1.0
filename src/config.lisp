;;;; Copyright 2023 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit.config
  (:use :cl)
  (:export :set-config
           :set-docker-config
           :set-development-config
           :set-test-config
           :environment-name
           :database-connection-string
           :database-type-name
           :database-username
           :memcached-server
           :memcached-port
           :mail-server
           :mail-port
           :mail-username
           :mail-password
           :logger-name))

(in-package :reddit.config)

(defvar *default-config*)

(defun env-or-default (env-name default-value)
  (or (uiop:getenv env-name)
      default-value))

(defparameter *docker*
  `(:environment "docker"
    :database (:type :postgresql
               :database ,(env-or-default "REDDIT_DATABASE_DATABASE" "reddit")
               :server ,(env-or-default "REDDIT_DATABASE_SERVER" "db")
               :port ,(env-or-default "REDDIT_DATABASE_PORT" "5432")
               :username ,(env-or-default "REDDIT_DATABASE_USERNAME" "pgsql")
               :password ,(env-or-default "REDDIT_DATABASE_PASSWORD" "pgcwip42:"))
    :memcached (:server ,(env-or-default "REDDIT_MEMCACHED_SERVER" "memcached")
                :port ,(parse-integer (env-or-default "REDDIT_MEMCACHED_PORT" "11211")))
    :mail (:server ,(env-or-default "REDDIT_MAIL_SERVER" "mail")
           :username ,(env-or-default "REDDIT_MAIL_USERNAME" "username")
           :password ,(env-or-default "REDDIT_MAIL_PASSWORD" "password")
           :port ,(parse-integer (env-or-default "REDDIT_MAIL_PORT" "25")))
    :logger (:logger-name "stdout")))

(defparameter *development*
  `(:environment "development"
    :database (:type :postgresql
               :database ,(env-or-default "REDDIT_DATABASE_DATABASE" "reddit")
               :server ,(env-or-default "REDDIT_DATABASE_SERVER" "127.0.0.1")
               :port ,(env-or-default "REDDIT_DATABASE_PORT" "5432")
               :username ,(env-or-default "REDDIT_DATABASE_USERNAME" "pgsql")
               :password ,(env-or-default "REDDIT_DATABASE_PASSWORD" "pgcwip42:"))
    :memcached (:server ,(env-or-default "REDDIT_MEMCACHED_SERVER" "127.0.0.1")
                :port ,(parse-integer (env-or-default "REDDIT_MEMCACHED_PORT" "11211")))
    :mail (:server ,(env-or-default "REDDIT_MAIL_SERVER" "127.0.0.1")
           :username ,(env-or-default "REDDIT_MAIL_USERNAME" "username")
           :password ,(env-or-default "REDDIT_MAIL_PASSWORD" "password")
           :port ,(parse-integer (env-or-default "REDDIT_MAIL_PORT" "25")))
    :logger (:logger-name "stdout")))

(defparameter *test*
  `(:environment "test"
    :database (:type :postgresql
               :database ,(env-or-default "REDDIT_DATABASE_DATABASE" "reddit_test")
               :server ,(env-or-default "REDDIT_DATABASE_SERVER" "db")
               :port ,(env-or-default "REDDIT_DATABASE_PORT" "5432")
               :username ,(env-or-default "REDDIT_DATABASE_USERNAME" "pgsql")
               :password ,(env-or-default "REDDIT_DATABASE_PASSWORD" "pgcwip42:"))
    :memcached (:server ,(env-or-default "REDDIT_MEMCACHED_SERVER" "memcached")
                :port ,(parse-integer (env-or-default "REDDIT_MEMCACHED_PORT" "11211")))
    :mail (:server ,(env-or-default "REDDIT_MAIL_SERVER" "mail")
           :username ,(env-or-default "REDDIT_MAIL_USERNAME" "username")
           :password ,(env-or-default "REDDIT_MAIL_PASSWORD" "password")
           :port ,(parse-integer (env-or-default "REDDIT_MAIL_PORT" "25")))
    :logger (:logger-name "stdout")))

(defun set-config (conf)
  (setf *default-config* conf))

(defun set-docker-config ()
  (set-config *docker*))

(defun set-development-config ()
  (set-config *development*))

(defun set-test-config ()
  (set-config *test*))

(defun environment-name ()
  (getf *default-config* :environment))

;;;
;;; database
;;;
(defun database-connection-string ()
  (let* ((database (getf *default-config* :database))
         (type (getf database :type)))
    ;;
    (cond ((eq type :postgresql)
           `(,(getf database :server)
             ,(getf database :database)
             ,(getf database :username)
             ,(getf database :password)
             ,(getf database :port)))
          ((eq type :mysql)
           `(,(getf database :server)
             ,(getf database :database)
             ,(getf database :username)
             ,(getf database :password)
             ,(getf database :port)))
          ((eq type :sqlite)
           '()))))

(defun database-type-name ()
  (let ((database (getf *default-config* :database)))
    (getf database :type)))

(defun database-username ()
  (let ((database (getf *default-config* :database)))
    (getf database :username)))

(defun memcached-server ()
  (let ((memcached (getf *default-config* :memcached)))
    (getf memcached :server)))

(defun memcached-port ()
  (let ((memcached (getf *default-config* :memcached)))
    (getf memcached :port)))


;;;
;;; mail
;;;
(defun mail-server ()
  (let ((mail (getf *default-config* :mail)))
    (getf mail :server)))

(defun mail-port ()
  (let ((mail (getf *default-config* :mail)))
    (getf mail :port)))

(defun mail-username ()
  (let ((mail (getf *default-config* :mail)))
    (getf mail :username)))

(defun mail-password ()
  (let ((mail (getf *default-config* :mail)))
    (getf mail :password)))


;;;
;;; logger
;;;
(defun logger-name ()
  (let ((logger (getf *default-config* :logger)))
    (getf logger :logger-name)))
