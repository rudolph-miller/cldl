(in-package :cl-user)
(defpackage cldl-test
  (:use #:cl
        #:prove)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cldl.unit
                #:generate-units
                #:output-units)
  (:import-from #:cldl.connection
                #:connect)
  (:import-from #:cldl.data
                #:make-data-set
                #:data-input
                #:data-expected)
  (:import-from #:cldl.dnn
                #:dnn
                #:dnn-units
                #:predict))
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

(defun test ()
  (let* ((units (generate-units (list 4 4 4 3)))
         (connections (connect units))
         (data-set (data-set))
         (dnn (make-instance 'dnn :units units :connections connections))
         (correc-count 0))
    (dolist (data data-set)
      (when (= (position (predict dnn (data-input data))
                         (output-units (dnn-units dnn)))
               (data-expected data))
        (incf correc-count)))
    (let ((rate (/ correc-count (length data-set))))
      (format t "Accuracy: ~,2f%" rate)
      rate)))


(finalize)
