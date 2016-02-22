(in-package :cl-user)
(defpackage cldl.dnn
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.util
                #:pick-randomly)
  (:import-from #:cldl.unit
                #:hidden-unit
                #:output-unit
                #:unit-input-value
                #:unit-output-value
                #:unit-left-connections
                #:input-units
                #:hidden-unit-set
                #:output-units
                #:activate)
  (:import-from #:cldl.connection
                #:connection-left-unit
                #:connection-weight)
  (:import-from #:cldl.data
                #:data-input
                #:data-expected))
(in-package :cldl.dnn)

(syntax:use-syntax :annot)

@export
@doc
"Function for calculate output-values.
Default: Softmax function"
(defvar *OUTPUT-FUNCTION*
  (lambda (output-values)
    (let* ((list (mapcar #'exp output-values))
           (sum (reduce #'+ list)))
      (mapcar #'(lambda (value)
                  (/ value sum))
              list))))

@export
@doc
"Error function
Default: For multi class classification"
(defvar *ERROR-FUNCTION*
  (lambda (output-units expected)
    (* -1
       (reduce #'+
               (loop for unit in output-units
                     for i from 0
                     collecting (* (if (= i expected) 1 0)
                                   (log (unit-output-value unit))))))))


@export
@export-accessors
@doc
"Class of Deep Neural Network."
(defclass dnn ()
  ((units :initform nil
          :type list
          :initarg :units
          :accessor dnn-units)
   (connections :initform nil
                :type list
                :initarg :connections
                :accessor dnn-connections)
   (learning-coefficient :initform 0.01
                         :type number
                         :initarg :learning-coefficient
                         :accessor dnn-learning-coefficient)
   (mini-batch-size :initform 10
                    :type number
                    :initarg :mini-batch-size
                    :accessor dnn-mini-batch-size)))

(defmethod print-object ((dnn dnn) stream)
  (print-unreadable-object (dnn stream :type t :identity t)
    (with-slots (learning-coefficient mini-batch-size) dnn
      (format stream
              ":LEARING-COEFFICIENT ~a :MINI-BATCH-SIZE ~a"
              learning-coefficient
              mini-batch-size))))

(defun calculate-unit-input-value (unit)
  (reduce #'+
          (mapcar #'(lambda (connection)
                      (* (unit-output-value
                          (connection-left-unit connection))
                         (connection-weight connection)))
                  (unit-left-connections unit))))

@export
(defun predict (dnn input)
  (let ((dnn-units (dnn-units dnn)))
    (loop for input-unit in (cdr (input-units dnn-units))
          for value in input
          do (setf (unit-input-value input-unit) value
                   (unit-output-value input-unit) value))
    (dolist (hidden-units (hidden-unit-set dnn-units))
      (dolist (unit hidden-units)
        (when (typep unit 'hidden-unit)
          (setf (unit-input-value unit)
                (calculate-unit-input-value unit))
          (activate unit))))
    (let ((output-units (output-units dnn-units)))
      (dolist (unit output-units)
        (setf (unit-input-value unit)
              (calculate-unit-input-value unit)))
      (let ((output-values (funcall *OUTPUT-FUNCTION*
                                    (mapcar #'unit-input-value output-units))))
        (loop for unit in output-units
              for value in output-values
              do (setf (unit-output-value unit) value))
        output-units))))

(defun test (dnn data-set)
  (let ((picked-data-set (pick-randomly data-set (dnn-mini-batch-size dnn))))
    (/ (reduce #'+
               (mapcar #'(lambda (data)
                           (funcall *ERROR-FUNCTION*
                                    (predict dnn (data-input data))
                                    (data-expected data)))
                       picked-data-set))
       (length picked-data-set))))
