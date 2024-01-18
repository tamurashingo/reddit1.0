;;;; Copyright 2024 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage :reddit-test/autocompute
  (:use :cl
        :rove
        :reddit.autocompute))
(in-package :reddit-test/autocompute)

(setup
 (format T "setup~%"))

(teardown
 (format T "teardown~%"))


(deftest get-processes
  (let ((th (bt:make-thread
             #'(lambda ()
                 (loop (sleep 10)))
             :name "get-processes-test"))
        result)
    (setf result (reddit.autocompute::get-processes "get-processes-test"))
    (ok (not (null result)))
    (ok (eql 1 (length result)))

    (bt:destroy-thread th)
    (sleep 1) ; flaky?

    (ok (null (reddit.autocompute::get-processes "get-processes-test")))))


(deftest destroy-processes
  (testing "delete exists process"
    (let ((th (bt:make-thread
               #'(lambda ()
                   (loop (sleep 10)))
               :name "destroy-processes-test"))
          result)
      (setf result (reddit.autocompute::get-processes "destroy-processes-test"))
      (ok (not (null result)))
      (ok (eql 1 (length result)))

      (destroy-processes "destroy-processes-test")
      (sleep 1) ; flaky?

      (ok (null (reddit.autocompute::get-processes "destroy-processes-test")))))
  (testing "delete non-exist process"
    (destroy-processes "destroy-processes-test"))
  (testing "delete multiple processes"
     (dotimes (x 5)
       (bt:make-thread #'(lambda () (loop (sleep 10)))
                       :name "multiple-process"))

     (ok (eql 5 (length (reddit.autocompute::get-processes "multiple-process"))))

     (destroy-processes "multiple-process")
     (sleep 1)

     (ok (null (reddit.autocompute::get-processes "multiple-process")))))


(deftest update-thread
  (let ((auto (make-instance 'ac :name "universal-time"
                                 :period 1
                                 :fn #'(lambda ()
                                         (get-universal-time)))))

    (unwind-protect
         (progn
           ;; wait 5 sec
           (sleep 5)
           ;; initial value is null and updated to universal-time
           (ok (not (null (ac-val auto)))))
      (destroy-processes (ac-name auto)))))


(deftest update-immediately
    (let ((auto (make-instance 'ac :name "universal-time"
                                   :period 100
                                   :fn #'(lambda ()
                                           (get-universal-time)))))

      (unwind-protect
           (progn
             ;; initial value is null
             (ok (null (ac-val auto)))
             ;; update immediately
             (ac-update auto)
             ;; updated to universal-time
             (ok (not (null (ac-val auto)))))
        (destroy-processes (ac-name auto)))))
