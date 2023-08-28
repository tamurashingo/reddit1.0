;;;; Copyright 2023 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit.config
  (:use :cl)
  (:export :set-config
           :set-docker-config
           :set-development-config
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


(defparameter *docker*
  '(:environment "docker"
    :database (:type :postgresql
               :database "reddit"
               :server "db"
               :port "5432"
               :username "pgsql"
               :password "pgcwip42:")
    :memcached (:server "memcached"
                :port 11211)
    :mail (:server "mailserver"
           :username "username"
           :password "password"
           :port 1025)
    :logger (:logger-name "stdout")))

(defparameter *development*
  '(:environment "development"
    :database (:type :postgresql
               :database "reddit"
               :server "127.0.0.1"
               :port "5432"
               :username "pgsql"
               :password "pgcwip42:")
    :memcached (:server "memcached"
                :port 11211)
    :mail (:server "127.0.0.1"
           :username "username"
           :password "password"
           :port 25)
    :logger (:logger-name "stdout")))


(defun set-config (conf)
  (setf *default-config* conf))

(defun set-docker-config ()
  (set-config *docker*))

(defun set-development-config ()
  (set-config *development*))

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
