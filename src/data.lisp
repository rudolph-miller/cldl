(in-package :cl-user)
(defpackage cldl.data
  (:use #:cl
        #:annot.doc
        #:annot.class))
(in-package :cldl.data)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Class of data"
(defclass data ()
  ((expected :initform nil
             :type (or null number)
             :initarg :expected
             :accessor data-expected)
   (input :initform nil
          :type (or null list)
          :initarg :input
          :accessor data-input)))

(defmethod print-object ((data data) stream)
  (print-unreadable-object (data stream :type t :identity t)
    (with-slots (expected input) data
      (format stream ":EXPECTED ~a :INPUT ~a" expected input))))

@export
(defun make-data (&key expected input)
  (make-instance 'data :expected expected :input input))

@export
(defun make-data-set (list)
  (mapcar #'(lambda (data)
              (apply #'make-data data))
          list))
