(in-package :cl-user)
(defpackage cldl.data
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.util
                #:mean-and-standard-deviation
                #:normalize))
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

@export
(defun normalize-data-set (data-set)
  (let* ((length (length (data-input (car data-set))))
         (t-array (make-array length :initial-element nil))
         means-and-standard-deviations)
    (loop for data in data-set
          for i from 0
          do (loop for value in (data-input data)
                   for j from 0
                   do (push value (aref t-array j))))
    (loop for i from 0 below length
          for list = (aref t-array i)
          do (multiple-value-bind (mean standard-deviation)
                 (mean-and-standard-deviation list)
               (setq means-and-standard-deviations
                     (append means-and-standard-deviations
                             (list (cons mean standard-deviation))))))
    (mapcar #'(lambda (data)
                (let ((input
                        (loop for value in (data-input data)
                              for mean-and-standard-deviation
                                in means-and-standard-deviations
                              for mean = (car mean-and-standard-deviation)
                              for standard-deviation = (cdr mean-and-standard-deviation)
                              collecting (normalize value mean standard-deviation))))
                  (make-instance 'data
                                 :input input
                                 :expected (data-expected data))))
            data-set)))
