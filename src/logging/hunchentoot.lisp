(in-package :cl-user)
(defpackage reddit.logging.hunchentoot
  (:use :cl
        :reddit.logging)
  (:import-from :hunchentoot
                :log-message*)
  (:export :<hunchentoot-logger>))
(in-package :reddit.logging.hunchentoot)

(defclass <hunchentoot-logger> (<reddit-logger>) ())

(setf reddit.logging::*logger* (make-instance '<hunchentoot-logger>))

(defmethod log-message-logger ((logger <hunchentoot-logger>) log-level format-string &rest format-arguments)
  (apply #'log-message* log-level format-string format-arguments))



