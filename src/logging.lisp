(in-package :cl-user)
(defpackage :reddit.logging
  (:use :cl)
  (:export :initialize-logger
           :log-message
           :log-message-logger
           :<reddit-logger>))
(in-package :reddit.logging)

(defun initialize-logger (logger-name)
  "initialize logging library. logger-name is keyword for logging library"
  (let ((logger-system (intern (format NIL "REDDIT-LOGGER-~A" logger-name) :keyword)))
    (format T "-> ~A~%" logger-system)
    #+quicklisp (ql:quickload logger-system :verbose NIL :silent T)
    #-quicklisp
    (asdf:load-system logger-system :verbose NIL)))

(defclass <reddit-logger> () ()
  (:documentation "base class for logger"))

(defgeneric log-message-logger (logger log-level format-string &rest format-arguments)
  (:documentation "logging message. log-level is one of :ERROR, :INFO, :WARNING"))

;(defmethod log-message-logger ((logger <reddit-logger>) log-level format-string &rest format-arguments)
;  (declare (ignore log-level format-string format-arguments))
;  T)

(defun log-message (log-level format-string &rest format-arguments)
  (apply #'log-message-logger *logger* log-level format-string format-arguments))


(defparameter *logger* (make-instance '<reddit-logger>))

