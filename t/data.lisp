(in-package :cl-user)
(defpackage reddit-test.data
  (:use :cl
        :reddit.data
        :prove)
  (:import-from :reddit.config
                :set-environment))
(in-package :reddit-test.data)

(set-environment :test)
(reddit.main::connect-database)

(plan nil)


(reddit.data::valid-user-p "foo")

(reddit.data::add-user "foo" "foo@sample.com" "password" "192.168.0.1")

(reddit.data::valid-user-p "foo")


(finalize)

(reddit.main::disconnect-database)
