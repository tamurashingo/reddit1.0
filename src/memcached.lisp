;;;; Copyright 2018 Reddit, Inc.
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is furnished to do
;;;; so, subject to the following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included in all
;;;; copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.

(in-package :cl-user)
(defpackage reddit.memcached
  (:use :cl)
  (:export :cached))
(in-package :reddit.memcached)

(defparameter *STORED* "STORED")
(defparameter *NOTSTORED* "NOT_STORED")
(defparameter *END* "END")
(defparameter *DELETED* "DELETED")
(defparameter *NOTFOUND* "NOT_FOUND")
(defparameter *OK* "OK")
(defparameter *ERROR* "ERROR")
(defparameter *CLIENTERROR* "CLIENT_ERROR")
(defparameter *SERVERERROR* "SERVER_ERROR")
(defparameter *VALUE* "VALUE")


(defmacro cached ((key &optional (exp 0)) &body body) 
  (let ((k (gensym)))
    `(let ((,k ,key))
        (or (mc-get ,k)
            (let ((val (progn ,@body)))
              (mc-set ,k val ,exp)
              val)))))

(defvar *memcached* (cl-memcached:make-memcache :ip "127.0.0.1" :port 11211 :name "reddit memcached"))

(defun mc-get (key)
  (cl-memcached:mc-get-value key :memcache *memcached*))

(defun mc-set (key val &optional (exp 0))
  (let* ((val-str (with-output-to-string (s) (prin1 val s)))
         (response (cl-memcached:mc-set key val-str :memcache *memcached*)))
    (cond ((string= *STORED* response) :STORED)
          ((string= *NOTSTORED* response) :NOTSTORED)
          (t response))))


(defun mc-delete (key &optional (time 0))
  (let ((response (cl-memcached:mc-del key :memcache *memcached*)))
    (cond
      ((string= response *DELETED*) :DELETED)
      ((string= response *NOTFOUND*) :NOTFOUND)
      (t response))))

