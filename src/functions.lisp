(in-package :cl-user)
(defpackage cldl.functions
  (:use #:cl)
  (:import-from #:cldl.util
                #:mapcar*)
  (:import-from #:cldl.function
                #:def-function-set))
(in-package :cldl.functions)

(syntax:use-syntax :annot)

@export 'softmax
(def-function-set softmax (:output :multiple-values t)
  (:activation
   (lambda (output-values)
     (let* ((list (mapcar #'exp output-values))
            (sum (reduce #'+ list)))
       (mapcar #'(lambda (value)
                   (apply #'/ (mapcar #'(lambda (x)
                                          (coerce x 'double-float))
                                      (list value sum))))
               list))))

  (:error
   (lambda (output-values expected)
     (* -1
        (reduce #'+
                (mapcar* #'(lambda (value index)
                             (* (if (= index expected) 1 0)
                                (log value)))
                         output-values)))))

  (:delta
   (lambda (output-values expected)
     (mapcar* #'(lambda (value index)
                  (- value (if (= index expected) 1 0)))
              output-values))))

@export 'rectified-linear-unit
(def-function-set rectified-linear-unit (:hidden)
  (:activation
   (lambda (value)
     (if (< value 0) 0 value)))

  (:diff-of-activation
   (lambda (value)
     (if (< value 0) 0 1))))
