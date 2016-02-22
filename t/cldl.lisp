(in-package :cl-user)
(defpackage cldl-test
  (:use #:cl
        #:prove)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cldl.unit
                #:unit-output-value
                #:generate-units
                #:output-units)
  (:import-from #:cldl.connection
                #:connect)
  (:import-from #:cldl.data
                #:make-data-set
                #:data-input
                #:data-expected
                #:normalize-data-set)
  (:import-from #:cldl.dnn
                #:dnn
                #:dnn-units
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
          do (push (mapcar #'parse-integer
                           (split-sequence #\Space line))
                   result)
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
        finally (return (values (append result
                                        (list item))
                                rest))))

(defun separete-data-set (data-set)
  (let ((size (floor (/ (length data-set) 10))))
    (loop with result
          do (multiple-value-bind (head tail) (take size data-set)
               (push head result)
               (when (null tail)
                 (return result))
               (setq data-set tail)))))

(defun main (&optional (training-count 0))
  (let* ((units (generate-units (list 4 4 4 3)))
         (connections (connect units))
         (data-sets (separete-data-set (normalize-data-set (data-set))))
         (dnn (make-instance 'dnn :units units :connections connections))
         (correc-count 0)
         (test-count 0))
    (dolist (data-set data-sets)
      (let ((test-data-set (list (car data-set)
                                 (cadr data-set)))
            (train-data-set (cddr data-set)))
        (loop repeat training-count
              do (train dnn train-data-set)
              until (< (test dnn data-set) 0.01))
        (dolist (data test-data-set)
          (incf test-count)
          (when (= (position (max-unit (predict dnn (data-input data)))
                             (output-units (dnn-units dnn)))
                   (data-expected data))
            (incf correc-count)))))
    (let ((rate (/ correc-count test-count)))
      (format t "Accuracy: ~,2f%" (* 100 rate))
      rate)))

(finalize)
