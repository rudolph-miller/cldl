(in-package :cl-user)
(defpackage cldl.dnn
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.util
                #:pick-randomly
                #:mean-and-standard-deviation
                #:normalize)
  (:import-from #:cldl.unit
                #:hidden-unit
                #:output-unit
                #:unit-input-value
                #:unit-output-value
                #:unit-left-connections
                #:unit-right-connections
                #:unit-delta
                #:input-units
                #:hidden-unit-set
                #:output-units)
  (:import-from #:cldl.layer
                #:layer
                #:layer-units)
  (:import-from #:cldl.connection
                #:connection-left-unit
                #:connection-right-unit
                #:connection-weight
                #:connection-weight-diff)
  (:import-from #:cldl.data
                #:data
                #:data-input
                #:data-expected))
(in-package :cldl.dnn)

(syntax:use-syntax :annot)

@export
@doc
"Activation functon for output-units
Default: Softmax function"
(defparameter *OUTPUT-ACTIVATION-FUNCTION*
  (lambda (output-values)
    (let* ((list (mapcar #'exp output-values))
           (sum (reduce #'+ list)))
      (mapcar #'(lambda (value)
                  (apply #'/ (mapcar #'(lambda (x)
                                         (coerce x 'double-float))
                                     (list value sum))))
              list))))

@export
@doc
"Error function.
Default: For multi class classification"
(defparameter *OUTPUT-ERROR-FUNCTION*
  (lambda (output-values expected)
    (* -1
       (reduce #'+
               (loop for value in output-values
                     for i from 0
                     collecting (* (if (= i expected) 1 0)
                                   (log value)))))))

@export
@doc
"Backpropagation function for output-units."
(defparameter *OUTPUT-BACKPROPAGATION-FUNCTION*
  (lambda (output-values expected)
    (loop for value in output-values
          for i from 0
          collecting (- value (if (= i expected) 1 0)))))


@export
@doc
"Activation function for hidden-units.
Set output-value as activated input-value.
Default: Rectified Linear Unit"
(defparameter *HIDDEN-ACTIVATION-FUNCTION*
  (lambda (hidden-units)
    (mapcar #'(lambda (unit)
                (let ((input (unit-input-value unit)))
                  (if (< input 0) 0 input)))
            hidden-units)))

@export
@doc
"Backpropagation function for hidden-units."
(defvar *HIDDEN-BACKPROPAGATION-FUNCTION*
  (lambda (hidden-units)
    (mapcar #'(lambda (unit)
                (reduce #'+
                        (mapcar #'(lambda (connection)
                                    (* (unit-delta (connection-right-unit connection))
                                       (connection-weight connection)
                                       (if (< (unit-input-value unit) 0)
                                           0
                                           1)))
                                (unit-right-connections unit))))
            hidden-units)))

@export
@export-accessors
@doc
"Class of Deep Neural Network."
(defclass dnn ()
  ((layers :initform nil
           :type list
           :initarg :layers
           :accessor dnn-layers)
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
                    :accessor dnn-mini-batch-size)
   (input-means :initform nil
                :type list
                :initarg :input-means
                :accessor dnn-input-means)
   (input-standard-deviations :initform nil
                              :type list
                              :initarg :input-standard-deviations
                              :accessor dnn-input-standard-deviations)))

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

(defun normalize-input (input means standard-deviations)
  (mapcar #'(lambda (value mean standard-deviation)
              (normalize value mean standard-deviation))
          input
          means
          standard-deviations))

@export
(defun predict (dnn input)
  (let ((layers (dnn-layers dnn)))
    (map nil
         #'(lambda (input-unit value)
             (setf (unit-input-value input-unit) value
                   (unit-output-value input-unit) value))
         (layer-units (car layers))
         (normalize-input input
                          (dnn-input-means dnn)
                          (dnn-input-standard-deviations dnn)))
    (dolist (hidden-layer (butlast (cdr layers)))
      (let ((hidden-units (layer-units hidden-layer)))
        (dolist (unit hidden-units)
          (setf (unit-input-value unit)
                (calculate-unit-input-value unit)))
        (map nil
             #'(lambda (hidden-unit value)
                 (setf (unit-output-value hidden-unit) value))
             hidden-units
             (funcall *HIDDEN-ACTIVATION-FUNCTION* hidden-units))))
    (let ((output-units (layer-units (car (last layers)))))
      (dolist (unit output-units)
        (setf (unit-input-value unit)
              (calculate-unit-input-value unit)))
      (map nil
           #'(lambda (unit value)
               (setf (unit-output-value unit) value))
           output-units
           (funcall *OUTPUT-ACTIVATION-FUNCTION*
                    (mapcar #'unit-input-value
                            output-units)))
      output-units)))

(defun pick-data-set (dnn data-set)
  (pick-randomly data-set (dnn-mini-batch-size dnn)))

@export
@doc
"Test dnn"
(defun test (dnn data-set)
  (let ((picked-data-set (pick-data-set dnn data-set)))
    (/ (reduce #'+
               (mapcar #'(lambda (data)
                           (funcall *OUTPUT-ERROR-FUNCTION*
                                    (mapcar #'unit-output-value
                                            (predict dnn (data-input data)))
                                    (data-expected data)))
                       picked-data-set))
       (length picked-data-set))))

(defun calculate-means-and-standard-deviations (data-set)
  (let (means standard-deviations)
    (apply #'map
           nil
           #'(lambda (&rest rest)
               (multiple-value-bind (mean standard-deviation)
                   (mean-and-standard-deviation rest)
                 (push mean means)
                 (push standard-deviation standard-deviations)))
           (mapcar #'data-input data-set))
    (values (nreverse means)
            (nreverse standard-deviations))))

@export
@doc
"Train dnn by given data-set"
(defun train (dnn data-set)
  (flet ((backpropagate (units backpropagations)
           (map nil
                #'(lambda (unit delta)
                    (setf (unit-delta unit) delta)
                    (dolist (connection (unit-left-connections unit))
                      (incf (connection-weight-diff connection)
                            (* delta (unit-output-value
                                      (connection-left-unit connection))))))
                units
                backpropagations)))
    (multiple-value-bind (means standard-deviations)
        (calculate-means-and-standard-deviations data-set)
      (setf (dnn-input-means dnn) means
            (dnn-input-standard-deviations dnn) standard-deviations)
      (let ((picked-data-set (pick-data-set dnn data-set)))
        (dolist (data picked-data-set)
          (predict dnn (data-input data))
          (let* ((output-units (layer-units (car (last (dnn-layers dnn)))))
                 (output-backpropagations (funcall *OUTPUT-BACKPROPAGATION-FUNCTION*
                                                   (mapcar #'unit-output-value
                                                           output-units)
                                                   (data-expected data))))
            (backpropagate output-units output-backpropagations)))
        (dolist (hidden-layer (reverse (dnn-layers dnn)))
          (let* ((hidden-units (layer-units hidden-layer))
                 (hidden-backpropagations (funcall *HIDDEN-BACKPROPAGATION-FUNCTION*
                                                  hidden-units)))
            (backpropagate hidden-units hidden-backpropagations)))
        (dolist (outer-connections (dnn-connections dnn))
          (dolist (inner-connectios outer-connections)
            (dolist (connection inner-connectios)
              (decf (connection-weight connection)
                    (* (dnn-learning-coefficient dnn)
                       (connection-weight-diff connection)))
              (setf (connection-weight-diff connection) 0))))))))
