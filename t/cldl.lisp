(in-package :cl-user)
(defpackage cldl-test
  (:use #:cl
        #:prove)
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
(in-package :cldl-test)

(plan nil)

(defun data-path ()
  (flet ((component-by-name (name components)
           (find name
                 components
                 :key #'asdf/component:component-name
                 :test #'equal)))
    (let* ((t-module (component-by-name "t"
                                        (asdf/component:module-components
                                         (asdf:find-system :cldl-test))))
           (data (component-by-name "data.txt"
                                    (asdf/component:module-components t-module))))
      (when data
        (asdf/component:component-pathname data)))))

(defun data ()
  (with-open-file (in (data-path) :direction :input)
    (loop with result = nil
          for line = (read-line in nil nil)
          while line
          do (setq result
                   (append result
                           (list (mapcar #'parse-integer
                                         (split-sequence #\Space line)))))
          finally (return result))))

(defun data-set ()
  (make-data-set
   (mapcar #'(lambda (data)
               (list :expected (car data)
                     :input (cdr data)))
           (data))))

(defun max-unit (units)
  (loop with max-unit = (car units)
        for unit in (cdr units)
        if (> (unit-output-value unit) (unit-output-value max-unit))
          do (setq max-unit unit)
        finally (return max-unit)))

(defun take (n list)
  (loop for (item . rest) on list
        for i from 0 below (1- n)
        while rest
        collecting item into result
        finally (return ( (append result (list item)) rest))))

(defun separete-data-set (data-set)
  (let ((size (floor (/ (length data-set) 10))))
    (loop with result
          do (multiple-value-bind (head tail) (take size data-set)
               (push head result)
               (when (null tail)
                 (return result))
               (setq data-set tail)))))

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
    (loop for data-set in data-sets
          for i from 0
          for train-data-set = nil
          for test-data-set = nil
          do (loop for data-set in data-sets
                   for j from 0
                   if (= i j)
                     do (setq test-data-set data-set)
                   else
                     do (setq train-data-set
                              (append data-set train-data-set)))
             (loop repeat training-count
                   do (train dnn train-data-set)
                   until (and (< (test dnn data-set) 0.01)
                              (princ "Break! ")))
             (let ((tmp 0))
               (dolist (data test-data-set)
                 (incf test-count)
                 (when (= (position (max-unit (predict dnn (data-input data)))
                                    (layer-units (car (last (dnn-layers dnn)))))
                          (data-expected data))
                   (incf correc-count)
                   (incf tmp)))
               (format t "~,2f%~%" (* 100 (/ tmp (length test-data-set))))))
    (let ((rate (/ correc-count test-count)))
      (format t "Accuracy: ~,2f%" (* 100 rate))
      rate)))

(finalize)
