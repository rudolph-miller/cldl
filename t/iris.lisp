(in-package :cl-user)
(defpackage cldl-test.iris
  (:use #:cl
        #:prove)
  (:import-from #:uiop
                #:read-file-lines)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cldl.unit
                #:unit-output-value
                #:output-units)
  (:import-from #:cldl.functions
                #:softmax
                #:rectified-linear-unit)
  (:import-from #:cldl.layer
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:make-layers)
  (:import-from #:cldl.connection
                #:connect)
  (:import-from #:cldl.data
                #:make-data-set
                #:data-input
                #:data-expected)
  (:import-from #:cldl.dnn
                #:dnn
                #:dnn-layers
                #:predict
                #:test
                #:train))
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
               (list :expected (car data)
                     :input (cdr data)))
           (data))))

(defun separete-data-set (data-set)
  (loop with size  = (floor (/ (length data-set) 10))
        while (> (length data-set) 0)
        for list = (subseq data-set 0 size)
        collecting list
        until (< (length list) size)
        do (setq data-set (subseq data-set size))))

(defun main (&optional (training-count 0))
  (let* ((layers (make-layers (list (list 'input-layer 4)
                                    (list 'hidden-layer 10 'rectified-linear-unit)
                                    (list 'output-layer 3 'softmax))))
         (connections (connect layers))
         (data-sets (separete-data-set (data-set)))
         (dnn (make-instance 'dnn
                             :layers layers
                             :connections connections
                             :learning-coefficient 0.001))
         (correc-count 0)
         (test-count 0))
    (dolist (test-data-set data-sets)
      (let ((train-data-set (apply #'append (remove test-data-set data-sets)))
            (partial-correct-count 0))
        (loop repeat training-count
              do (train dnn train-data-set)
              until (and (< (test dnn test-data-set) 0.01)
                         (princ "Break! ")))
        (dolist (data test-data-set)
          (incf test-count)
          (let ((result (predict dnn (data-input data))))
            (when (= (position (apply #'max result) result)
                     (data-expected data))
              (incf correc-count)
              (incf partial-correct-count))))
        (format t "~,2f%~%" (* 100 (/ partial-correct-count
                                      (length test-data-set))))))
    (let ((rate (/ correc-count test-count)))
      (format t "Accuracy: ~,2f%" (* 100 rate))
      rate)))

(finalize)
