(in-package :cl-user)
(defpackage cldl.util
  (:use #:cl
        #:annot.doc))
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
