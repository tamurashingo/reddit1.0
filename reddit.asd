;;;; Silly emacs, this is -*- Lisp -*- (or thereabouts)

(in-package #:cl-user)

(defpackage #:reddit-system
  (:use #:asdf #:cl))

(in-package #:reddit-system)

(defsystem reddit
  :depends-on (:hunchentoot
               :bordeaux-threads
               :cl-ppcre
               :trivial-http
               :cl-who
               :clsql
               :clsql-postgresql
               :cl-smtp
               :ironclad
               :cl-memcached)
  :components ((:module "src"
                :components
                ((:file "packages" :depends-on ("main"))
                 (:file "main" :depends-on ("frame" "rss" "user-panel" "web"))
                 (:file "cookiehash" :depends-on ("data" "view-defs" "util"))
                 (:file "recommend" :depends-on ("user-info"))
                 (:file "frame" :depends-on ("data" "user-info" "util" "view-defs"))
                 (:file "autocompute")
                 (:file "user-info" :depends-on ("data" "util"))
                 (:file "web" :depends-on ("autocompute" "cookiehash" "data" "frame" "mail" "memcached" "recommend" "sites" "user-info" "util" "view-defs"))
                 (:file "data" :depends-on ("view-defs" "util"))
                 (:file "view-defs")
                 (:file "util")
                 (:file "search")
                 ;;(:file "options" :depends-on ("packages" "data"))
                 (:file "memcached")
                 ;;(:file "crc" :depends-on ("packages"))
                 (:file "rss" :depends-on ("memcached" "sites"))
                 (:file "sites" :depends-on ("autocompute" "data" "search" "user-info" "util" "view-defs"))
                 (:file "mail" :depends-on ("data"))
                 (:file "user-panel" :depends-on ("data" "sites" "user-info" "util" "view-defs" "web"))))))
