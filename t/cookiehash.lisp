(in-package :cl-user)
(defpackage reddit-test.cookiehash
  (:use :cl
        :reddit.cookiehash
        :prove))
(in-package :reddit-test.cookiehash)

(plan nil)

(finalize)
