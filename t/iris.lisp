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
                #:rectified-linear-unit
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:make-layers
                #:connect-layers
                #:make-data-set
                #:data-input
                #:data-expected
                #:dnn
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

(defun main (&optional (training-count 0) (silent nil))
  (let* ((layers (make-layers (list (list 'input-layer 4)
                                    (list 'hidden-layer 10 'rectified-linear-unit)
                                    (list 'output-layer 3 'softmax))))
         (connections (connect-layers layers))
         (data-sets (separete-data-set (data-set)))
         (start (get-internal-real-time))
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
              until (and (< (test dnn train-data-set) 0.01)
                         (if silent
                             t
                             (princ "Break! "))))
        (dolist (data test-data-set)
          (incf test-count)
          (let ((result (predict dnn (data-input data))))
            (when (= (position (apply #'max result) result)
                     (data-expected data))
              (incf correc-count)
              (incf partial-correct-count))))
        (unless silent
          (format t "~,2f%~%" (* 100 (/ partial-correct-count
                                        (length test-data-set)))))))
    (let ((rate (/ correc-count test-count)))
      (format t "Accuracy: ~,2f%~%" (* 100 rate))
      (format t "Time: ~amsec~%" (- (get-internal-real-time) start))
      rate)))

(finalize)
