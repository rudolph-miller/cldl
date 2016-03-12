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
(def-d-function (multi-class-cross-entropy :take-value-set t) (values expected)
  (:fn (* -1
          (reduce #'+
                  (mapcar* #'(lambda (value index)
                               (* (if (= index expected) 1 0)
                                  (log value)))
                           values))))

  (:diff (mapcar* #'(lambda (value index)
                      (- value (if (= index expected) 1 0)))
                  values)))

@export 'rectified-linear-unit
(def-d-function rectified-linear-unit (value)
  (:fn (if (< value 0) 0 value))

  (:diff (if (< value 0) 0 1)))
