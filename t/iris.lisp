(in-package :cl-user)
(defpackage cldl-test.iris
  (:use #:cl
        #:prove)
  (:import-from #:uiop
                #:read-file-lines)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cldl
                #:softmax
                #:multi-class-cross-entropy
                #:rectified-linear-unit
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:make-layers
                #:make-data-set
                #:dnn))
(in-package :cldl-test.iris)

(plan nil)

(defun data-path ()
  (flet ((find-component-by-name (name module)
           (find name
                 (asdf/component:module-components module)
                 :key #'asdf/component:component-name
                 :test #'equal)))
    (let* ((t-module (find-component-by-name "t" (asdf:find-system :cldl-test)))
           (data-module (find-component-by-name "data" t-module))
           (data (find-component-by-name "iris.txt" data-module)))
      (when data
        (asdf/component:component-pathname data)))))

(defun data ()
  (mapcar #'(lambda (line)
              (mapcar #'parse-integer
                      (split-sequence #\Space line)))
          (read-file-lines (data-path))))

(defun data-set ()
  (make-data-set
   (mapcar #'(lambda (data)
               (let ((expected (loop for i from 0 to 2
                                     with expected-index = (car data)
                                     if (= i expected-index)
                                       collecting 1
                                     else
                                       collecting 0)))
                 (list :expected expected
                       :input (cdr data))))
           (data))))

(defun main (&optional (training-count 0) (silent nil))
  (let* ((layers (make-layers (list (list 'input-layer 4)
                                    (list 'hidden-layer 10
                                          :activation-function 'rectified-linear-unit)
                                    (list 'output-layer 3
                                          :activation-function 'softmax
                                          :error-function 'multi-class-cross-entropy))))
         (dnn (make-instance 'dnn
                             :layers layers
                             :learning-coefficient 0.001))
         (correct-fn (lambda (output-values expected-values)
                       (= (position (apply #'max output-values) output-values)
                          (position 1 expected-values))))
         (start (get-internal-real-time)))
    (cldl:run dnn
              (data-set)
              correct-fn
              :training-count training-count
              :silent silent
              :fold-num 10
              :error-threshold 0.01)
    (format t "Time: ~amsec~%" (- (get-internal-real-time) start))))

(finalize)
