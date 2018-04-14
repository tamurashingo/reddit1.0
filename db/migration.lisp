;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage :reddit.db.migration
  (:use :cl)
  (:import-from :reddit.view-defs
                :user
                :article
                :article-with-sn
                :wtf
                :click
                :like
                :moduser
                :modarticle
                :neuter
                :options
                :alias))
(in-package :reddit.db.migration)

(defun migrate ()
  (clsql:create-view-from-class 'user)
  (clsql:create-view-from-class 'article)
  (clsql:create-view-from-class 'article-with-sn)
  (clsql:create-view-from-class 'wtf)
  (clsql:create-view-from-class 'click)
  (clsql:create-view-from-class 'like)
  (clsql:create-view-from-class 'moduser)
  (clsql:create-view-from-class 'modarticle)
  (clsql:create-view-from-class 'neuter)
  (clsql:create-view-from-class 'options)
  (clsql:create-view-from-class 'alias))


