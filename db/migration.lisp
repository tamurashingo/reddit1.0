;;;; Copyright 2018 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage :reddit.db.migration
  (:use :cl
        :reddit.db.function)
  (:import-from :reddit.config
                :database-username)
  (:export :up
           :down
           :rebuild))
(in-package :reddit.db.migration)

(defun create-table-from-class (view-class-name)
  (let ((table-name (clsql:view-table (find-class view-class-name))))
    (format T "create table: ~A  from:~A class" table-name view-class-name)
    (if (not (clsql:table-exists-p table-name :owner (database-username)))
        (progn
          (clsql:create-view-from-class view-class-name)
          (format T "... created~%"))
        (format T "... already exists~%"))))

(defun create-table (table-name description)
  (format T "create table: ~A" table-name)
  (if (not (clsql:table-exists-p table-name :owner (database-username)))
      (progn
        (clsql:create-table table-name description)
        (format T "... created~%"))
      (format T "... already exists~%")))

(defun create-sequence (sequence-name)
  (format T "create sequence: ~A~%" sequence-name)
  (when (not (clsql:sequence-exists-p sequence-name :owner (database-username)))
    (clsql:create-sequence sequence-name)))

(defun create-function (fn)
  (format T "create function: ~A" (getf fn :name))
  (clsql:execute-command (getf fn :code))
  (format T "... created~%"))

(defun drop-table-from-class (view-class-name)
  (let ((table-name (clsql:view-table (find-class view-class-name))))
    (format T "drop table: ~A  from ~A class" table-name view-class-name)
    (if (clsql:table-exists-p table-name :owner (database-username))
        (progn
          (clsql:drop-view-from-class view-class-name :owner (database-username))
          (format T "... dropped~%"))
        (format T "... not found~%"))))

(defun drop-table (table-name)
  (format T "drop table: ~A" table-name)
  (clsql:drop-table table-name :if-does-not-exist :ignore :owner (database-username))
  (format T "... dropped~%"))

(defun drop-sequence (sequence-name)
  (format T "drop sequence: ~A~%" sequence-name)
  (when (clsql:sequence-exists-p sequence-name :owner (database-username))
    (clsql:drop-sequence sequence-name)))

(defun drop-function (fn)
  (format T "drop function: ~A" (getf fn :name))
  (clsql:execute-command (getf fn :code))
  (format T "... dropped~%"))


(defun up ()
  (create-table-from-class 'reddit.view-defs:user)
  (create-table-from-class 'reddit.view-defs:article)
  ;; add column for full text search
  (clsql:execute-command
   "alter table
        articles
      add column
        idxfti tsvector GENERATED ALWAYS AS (to_tsvector('english', coalesce(url, '') || ' ' || coalesce(title, ''))) STORED")
  (clsql:execute-command
   "create index
        idxfti_idx
      on articles
        using gin(idxfti)")
  ; (create-table-from-class 'reddit.view-defs:article-with-sn)
  ;; create view for full text search
  (clsql:execute-command
   "create view
        articles_sn
    as
      select
        articles.id,
        articles.url,
        articles.title,
        articles.date,
        articles.submitter,
        articles.pop,
        users.screenname,
        articles.idxfti
      from
        articles,
        users
      where
        articles.submitter = users.id")
  (create-table-from-class 'reddit.view-defs:wtf)
  (create-table-from-class 'reddit.view-defs:click)
  (create-table-from-class 'reddit.view-defs:like)
  (create-table-from-class 'reddit.view-defs:moduser)
  (create-table-from-class 'reddit.view-defs:modarticle)
  (create-table-from-class 'reddit.view-defs:neuter)
  (create-table-from-class 'reddit.view-defs:options)
  (create-table-from-class 'reddit.view-defs:alias)
  (create-sequence "userid")
  (create-sequence "articleid")
  (create-table 'saved_sites '((userid integer)
                               (article integer)))
  (create-table 'closed_sites '((userid integer)
                                (article integer)))
  (create-function (create-function-seconds)))


(defun down ()
  (drop-table-from-class 'reddit.view-defs:alias)
  (drop-table-from-class 'reddit.view-defs:options)
  (drop-table-from-class 'reddit.view-defs:neuter)
  (drop-table-from-class 'reddit.view-defs:modarticle)
  (drop-table-from-class 'reddit.view-defs:moduser)
  (drop-table-from-class 'reddit.view-defs:like)
  (drop-table-from-class 'reddit.view-defs:click)
  (drop-table-from-class 'reddit.view-defs:wtf)
  ;(drop-table-from-class 'reddit.view-defs:article-with-sn)
  (clsql:execute-command "drop view if exists articles_sn")
  (drop-table-from-class 'reddit.view-defs:article)
  (drop-table-from-class 'reddit.view-defs:user)
  (drop-sequence "userid")
  (drop-sequence "articleid")
  (drop-table 'saved_sites)
  (drop-table 'closed_sites)
  (drop-function (drop-function-seconds)))

(defun rebuild ()
  (down)
  (up))

