(in-package :cl-user)
(defpackage :reddit-test.teardown
  (:use :cl))
(in-package :reddit-test.teardown)

(reddit.main:disconnect-database)

(plan nil)

(pass "database disconnected")


(finalize)
