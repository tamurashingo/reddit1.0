;;;;
;;;; Copyright 2023 tamura shingo
;;;; MIT License
;;;;

(in-package :cl-user)
(defpackage :reddit-test/memcached
  (:use :cl
        :rove
        :reddit.memcached))
(in-package :reddit-test/memcached)

(setup
  (initialize))

(teardown
  (cl-memcached:mc-del "test1" :memcache reddit.memcached::*memcached*)
  (cl-memcached:mc-del "key1" :memcache reddit.memcached::*memcached*)
  (cl-memcached:mc-del "key2" :memcache reddit.memcached::*memcached*))


(deftest test/mc-set/mc-get
  (testing "mc-get returns null when not set"
    (initialize)
    (ok (null (reddit.memcached::mc-get "test1"))))
  (testing "mc-get returns value when set"
    (initialize)
    (reddit.memcached::mc-set "key1" "value")
    (ok (string= (reddit.memcached::mc-get "key1")
                 "value"))))


(deftest test/cached
  (testing "cached"
    (initialize)
    (ok (null (reddit.memcached::mc-get "key2")))
    (ok (string= (cached ("key2" 0)
                   (format NIL "values"))
                 "values"))
    (ok (string= (reddit.memcached::mc-get "key2")
                 "values"))
    (ok (string= (cached ("key2" 0)
                   (format NIL "value2"))
                 "values"))))
