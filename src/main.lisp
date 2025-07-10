;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage reddit.main
  (:use :cl)
  (:import-from :hunchentoot
                :*dispatch-table*
                :create-prefix-dispatcher
                :create-regex-dispatcher
                :create-static-file-dispatcher-and-handler
                :easy-acceptor
                :start
                :stop)
  (:import-from :reddit.config
                :database-connection-string
                :database-type-name
                :logger-name)
  (:import-from :reddit.frame
                :reddit-toolbar)
  (:import-from :reddit.rss
                :rss-hot
                :rss-new
                :rss-pop)
  (:import-from :reddit.sites
                :create-cache
                :destroy-cache)
  (:import-from :reddit.user-panel
                :page-user)
  (:import-from :reddit.web
                :viewlink
                :page-default
                :page-submit
                :page-front
                :page-pop
                :page-new
                :page-saved
                :page-submitters
                :page-search
                :ajax-op
                :page-test
                :logout
                :page-password
                :page-lucky
                :default-handler
                :page-blog
                :page-help)
  (:import-from :reddit.conditions
                :configuration-not-set)
  (:import-from :reddit.logging
                :initialize-logger))
(in-package :reddit.main)


(defvar *reddit-acceptor* nil)
(defvar *not-initialized* T)

(defun initialize-once ()
  (when *not-initialized*
    (initialize-logger (logger-name))
    (reddit.memcached:initialize)
    (initialize-acceptor)
    (initialize-dispatch-table)
    (setf *not-initialized* nil)))

(defun initialize-acceptor ()
  (setf *reddit-acceptor* (make-instance 'easy-acceptor :port 8000)))

(defun initialize-dispatch-table ()
  (setq *dispatch-table*
        (nconc
         (list (create-static-file-dispatcher-and-handler
                "/favicon.ico"
                (make-pathname :directory "/reddit/static/" :name "favicon" :type "ico" :version nil
                               :defaults (load-time-value *load-pathname*))
                "image/x-icon"))
         (mapcar (lambda (args)
                   (apply #'create-prefix-dispatcher args))
                 '(("/rss/new" rss-new)
                   ("/rss/hot" rss-hot)
                   ("/rss/pop" rss-pop)
                   ("/viewlink" viewlink)
                   ("/browse" page-default)
                   ("/submit" page-submit)
                   ("/hot" page-front)
                   ("/pop" page-pop)
                   ("/new" page-new)
                   ("/saved" page-saved)
                   ("/topsub" page-submitters)
                   ("/search" page-search)
                   ("/aop" ajax-op)
                   ("/test" page-test)
                   ("/logout" logout)
                   ("/share" page-submit)
                   ("/password" page-password)
                   ("/lucky" page-lucky)
                   ("/user/" page-user)
                   ("/toolbar" reddit-toolbar)
                   ("/" page-default)))
         (list (create-static-file-dispatcher-and-handler
                "/blog/atom.xml" "/home/reddit/reddit/web/blog/atom.xml" "text/xml"))
         (mapcar (lambda (args)
                   (apply #'create-regex-dispatcher args))
                 '(("/blog/.+" default-handler)
                   ("/blog/?" page-blog)
                   ("/help/.+" default-handler)
                   ("/help/?" page-help))))))

(defun connect-database ()
  (clsql:connect (database-connection-string) :database-type (database-type-name) :if-exists :old))
(defun disconnect-database ()
  (clsql:disconnect))

(defun startup-reddit ()
  (when (or (not (boundp 'reddit.config::*default-config*))
            (null reddit.config::*default-config*))
    (error (make-condition 'configuration-not-set)))
  (initialize-once)
  (connect-database)
  (create-cache)
  (start *reddit-acceptor*))

(defun shutdown-reddit ()
  (stop *reddit-acceptor*)
  (destroy-cache)
  (disconnect-database))


