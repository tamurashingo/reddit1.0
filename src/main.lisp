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
                :config
                :set-environment)
  (:import-from :reddit.data
                :*conn-spec*
                :*database-type*)
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
  (:export :connect-database
           :disconnect-database))
(in-package :reddit.main)


(defvar *reddit-acceptor* nil)
(defvar *not-initialized* T)

(defun initialize-once ()
  (when *not-initialized*
    (initialize-acceptor)
    (setf *not-initialized* nil)))

(defun initialize ()
  (initialize-dispatch-table))

(defun initialize-acceptor ()
  (setf *reddit-acceptor* (make-instance 'easy-acceptor :port 8000)))

(defun initialize-dispatch-table ()
  (setq *dispatch-table*
        (nconc
         (list (create-static-file-dispatcher-and-handler
                "/favicon.ico"
                (make-pathname :directory (config :favicon-path) :name "favicon" :type "ico" :version nil
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
                   ("/toolbar" reddit-toolbar)))
         (list (create-static-file-dispatcher-and-handler
                "/blog/atom.xml" (config :atom-path) "text/xml"))
         (mapcar (lambda (args)
                   (apply #'create-regex-dispatcher args))
                 '(("/blog/.+" default-handler)
                   ("/blog/?" page-blog)
                   ("/help/.+" default-handler)
                   ("/help/?" page-help))))))

(defun connect-database ()
  (clsql:connect
   `(,(config :database-server)
      ,(config :database-name)
      ,(config :datbase-user)
      ,(config :database-password))
   :database-type (config :database-type)
   :if-exists :old))

(defun disconnect-database ()
  (clsql:disconnect))

(defun startup-reddit (&key env)
  (when (not (null env))
    (set-environment env))
  (initialize-once)
  (initialize)
  (connect-database)
  (create-cache)
  (start *reddit-acceptor*))

(defun shutdown-reddit ()
  (stop *reddit-acceptor*)
  (destroy-cache)
  (disconnect-database))


