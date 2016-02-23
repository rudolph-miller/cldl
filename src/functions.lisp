(in-package :cl-user)
(defpackage cldl.functions
  (:use #:cl)
  (:import-from #:cldl.function
                #:def-function-set))
(in-package :cldl.functions)

(syntax:use-syntax :annot)

@export 'softmax
(def-function-set softmax (:multiple-values t)
  (:fn (output-values)
       (let* ((list (mapcar #'exp output-values))
              (sum (reduce #'+ list)))
         (mapcar #'(lambda (value)
                     (apply #'/ (mapcar #'(lambda (x)
                                            (coerce x 'double-float))
                                        (list value sum))))
                 list)))
  (:diff (output-values expected)
         (loop for value in output-values
               for i from 0
               collecting (- value (if (= i expected) 1 0))))
  (:error (output-values expected)
          (* -1
             (reduce #'+
                     (loop for value in output-values
                           for i from 0
                           collecting (* (if (= i expected) 1 0)
                                         (log value)))))))

@export 'rectified-linear-unit
(def-function-set rectified-linear-unit ()
  (:fn (value)
       (if (< value 0) 0 value))
  (:diff (value)
         (if (< value 0) 0 1)))
