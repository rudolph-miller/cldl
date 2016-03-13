(in-package :cl-user)
(defpackage cldl.functions
  (:use #:cl)
  (:import-from #:cldl.util
                #:mapcar*)
  (:import-from #:cldl.differentiable-function
                #:def-d-function))
(in-package :cldl.functions)

(syntax:use-syntax :annot)

@export 'softmax
(def-d-function (softmax :take-value-set t) (values)
  (:fn (let* ((list (mapcar #'exp values))
              (sum (reduce #'+ list)))
         (mapcar #'(lambda (value)
                     (/ value sum))
                 list))))

@export 'multi-class-cross-entropy
(def-d-function (multi-class-cross-entropy :take-value-set t) (values expected-values)
  (:fn (* -1
          (reduce #'+
                  (mapcar #'(lambda (value expected)
                              (* expected (log value)))
                          values
                          expected-values))))

  (:diff (mapcar #'(lambda (value expected)
                     (- value expected))
                 values
                 expected-values)))

@export 'rectified-linear-unit
(def-d-function rectified-linear-unit (value)
  (:fn (if (< value 0) 0 value))

  (:diff (if (< value 0) 0 1)))
