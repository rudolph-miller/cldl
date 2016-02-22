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
                #:unit-right-connections
                #:unit-delta
                #:input-units
                #:hidden-unit-set
                #:output-units
                #:activate)
  (:import-from #:cldl.connection
                #:connection-left-unit
                #:connection-right-unit
                #:connection-weight
                #:connection-weight-diff)
  (:import-from #:cldl.data
                #:data-input
                #:data-expected))
(in-package :cldl.dnn)

(syntax:use-syntax :annot)

@export
@doc
"Activation functon for output-units
Default: Softmax function"
(defvar *OUTPUT-ACTIVATION-FUNCTION*
  (lambda (output-values)
    (let* ((list (mapcar #'exp output-values))
           (sum (reduce #'+ list)))
      (mapcar #'(lambda (value)
                  (/ value sum))
              list))))

@export
@doc
"Error function.
Default: For multi class classification"
(defvar *OUTPUT-ERROR-FUNCTION*
  (lambda (output-units expected)
    (* -1
       (reduce #'+
               (loop for unit in output-units
                     for i from 0
                     collecting (* (if (= i expected) 1 0)
                                   (log (unit-output-value unit))))))))

@export
@doc
"Backpropagation function for output-units."
(defvar *OUTPUT-BACKPROPAGATION-FUNCTION*
  (lambda (output-units expected)
    (loop for unit in output-units
          for i from 0
          collecting ( - (unit-output-value unit)
                         (if (= i expected) 1 0)))))

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
      (let ((output-values (funcall *OUTPUT-ACTIVATION-FUNCTION*
                                    (mapcar #'unit-input-value output-units))))
        (loop for unit in output-units
              for value in output-values
              do (setf (unit-output-value unit) value))
        output-units))))

(defun pick-data-set (dnn data-set)
  (pick-randomly data-set (dnn-mini-batch-size dnn)))

(defun test (dnn data-set)
  (let ((picked-data-set (pick-data-set dnn data-set)))
    (/ (reduce #'+
               (mapcar #'(lambda (data)
                           (funcall *ERROR-FUNCTION*
                                    (predict dnn (data-input data))
                                    (data-expected data)))
                       picked-data-set))
       (length picked-data-set))))

@export
@doc
"Train dnn by given data-set"
(defun train (dnn data-set)
  (flet ((backpropagate (units backpropagations)
           (loop for unit in units
                 for delta in backpropagations
                 do (setf (unit-delta unit) delta)
                    (dolist (connection (unit-left-connections unit))
                      (incf (connection-weight-diff connection)
                            (* delta (unit-output-value
                                      (connection-left-unit connection))))))))
    (let ((picked-data-set (pick-data-set dnn data-set)))
      (dolist (data picked-data-set)
        (predict dnn (data-input data))
        (let* ((output-units (output-units (dnn-units dnn)))
               (output-backpropagations (funcall *OUTPUT-BACKPROPAGATION-FUNCTION*
                                                 (output-units (dnn-units dnn))
                                                 (data-expected data))))
          (backpropagate output-units output-backpropagations)))
      (dolist (hidden-units (reverse (hidden-unit-set (dnn-units dnn))))
        (let* ((hidden-units-without-bias (remove-if-not #'(lambda (unit)
                                                             (typep unit 'hidden-unit))
                                                         hidden-units))
               (hidden-backpropagations (funcall *HIDDEN-BACKPROPAGATION-FUNCTION*
                                                hidden-units-without-bias)))
          (backpropagate hidden-units-without-bias hidden-backpropagations)))
      (dolist (outer-connections (dnn-connections dnn))
        (dolist (inner-connectios outer-connections)
          (dolist (connection inner-connectios)
            (decf (connection-weight connection)
                  (* (dnn-learning-coefficient dnn)
                     (connection-weight-diff connection)))
            (setf (connection-weight-diff connection) 0)))))))
