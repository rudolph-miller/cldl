(in-package :cl-user)
(defpackage cldl.util
  (:use #:cl
        #:annot.doc)
  (:import-from #:c2mop
                #:class-direct-subclasses))
(in-package :cldl.util)

(syntax:use-syntax :annot)

(defvar +random-digits+ (expt 10 24))

(setf *random-state* (make-random-state t))

(defun random<1 ()
  (/ (random +random-digits+) +random-digits+))

@export
@doc
"Get a random number from normal distribution."
(defun normal-random (mean standard-deviation)
  (let ((x (random<1))
        (y (random<1)))
    (+ mean
       (* standard-deviation
          (sqrt (* -2 (log x)))
          (cos (* 2 pi y))))))

@export
@doc
"Pick given count numbers of items from the given list"
(defun pick-randomly (list count)
  (let ((length (length list)))
    (if (<= length count)
        list
        (loop with result = nil
              with used-indexes = nil
              while (< (length result) count)
              for index = (random length)
              unless (find index used-indexes)
                do (push index used-indexes)
                   (push (elt list index) result)
              finally (return result)))))

@export
@doc
"Get mean and standard-deviation"
(defun mean-and-standard-deviation (list)
  (let* ((length (length list))
         (sum (reduce #'+ list))
         (mean (/ sum length))
         (variance (/
                    (reduce #'+
                            (mapcar #'(lambda (x)
                                        (expt (- x mean) 2))
                                    list))
                    length))
         (standard-deviation (sqrt variance)))
    (values mean standard-deviation)))

@export
@doc
"Normalize value by mean and standard-deviation"
(defun normalize (x mean standard-deviation)
  (/ (- x mean) standard-deviation))

@export
(defun subclasses (class)
  (let ((subclasses (class-direct-subclasses
                     (etypecase class
                       (symbol (find-class class))
                       (class class)))))
    (if subclasses
        (append subclasses
                (mapcan #'class-direct-subclasses subclasses))
        nil)))

@export
(defun mapcar* (function list)
  (loop for item in list
        for i from 0
        collecting (funcall function item i)))

@export
(defun partition (list k)
  (loop with size  = (floor (/ (length list) k))
        while (> (length list) 0)
        for part = (subseq list 0 size)
        collecting part
        until (< (length part) size)
        do (setq list (subseq list size))))
