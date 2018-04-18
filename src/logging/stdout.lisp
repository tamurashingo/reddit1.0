(in-package :cl-user)
(defpackage reddit.logging.stdout
  (:use :cl
        :reddit.logging)
  (:export :<stdout-logger>))
(in-package :reddit.logging.stdout)

(defclass <stdout-logger> (<reddit-logger>) ())

(setf reddit.logging::*logger* (make-instance '<stdout-logger>))

(defmethod log-message-logger ((logger <stdout-logger>) log-level format-string &rest format-arguments)
  (apply #'format T (concatenate 'string "[~A]" format-string "~%") log-level format-arguments))
