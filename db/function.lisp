;;;; Copyright 2023 tamura shingo
;;;;
;;;; MIT License

(in-package :cl-user)
(defpackage :reddit.db.function
  (:use :cl)
  (:export :create-function-seconds
           :drop-function-seconds))

(in-package :reddit.db.function)

(defun create-function-seconds ()
  '(:name "seconds"
    :code
    "CREATE FUNCTION seconds(date TIMESTAMP WITHOUT TIME ZONE)
RETURNS REAL AS $$
  BEGIN
    RETURN -(EXTRACT(EPOCH FROM date) - EXTRACT(EPOCH FROM CURRENT_TIMESTAMP));
  END;
$$ LANGUAGE plpgsql;"))

(defun drop-function-seconds ()
  '(:name "seconds"
    :code
    "DROP FUNCTION IF EXISTS seconds(TIMESTAMP)"))

