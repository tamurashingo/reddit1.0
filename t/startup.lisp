(in-package :cl-user)
(defpackage :reddit-test.startup
  (:use :cl
        :prove)
  (:import-from :reddit.config
                :set-environment))
(in-package :reddit-test.startup)

(clsql:locally-enable-sql-reader-syntax)
(set-environment :test)
(reddit.logging:initialize-logger :stdout)
(reddit.main:connect-database)


(plan nil)

(pass "database connected")

(clsql:delete-records :from [alias])
(clsql:delete-records :from [articles])
(clsql:delete-records :from [clicks])
(clsql:delete-records :from [like_site])
(clsql:delete-records :from [mod_article])
(clsql:delete-records :from [mod_user])
(clsql:delete-records :from [neuter])
(clsql:delete-records :from [options])
(clsql:delete-records :from [users])
(clsql:delete-records :from [wtf])

(pass "all tables deleted")


(finalize)
(clsql:locally-disable-sql-reader-syntax)
