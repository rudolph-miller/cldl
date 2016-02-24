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
  (:import-from #:cldl.function
                #:function-set-error-function)
  (:import-from #:cldl.layer
                #:layer
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:layer-units
                #:layer-function-set
                #:activate
                #:back-propagate-hidden-layer
                #:back-propagate-output-layer)
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
"Returns output values"
(defun predict (dnn input)
  (dolist (layer (dnn-layers dnn))
    (etypecase layer
      (input-layer
       (map nil
            #'(lambda (input-unit value)
                (setf (unit-input-value input-unit) value
                      (unit-output-value input-unit) value))
            (layer-units layer)
            (normalize-input input
                             (dnn-input-means dnn)
                             (dnn-input-standard-deviations dnn))))
      ((or hidden-layer output-layer)
       (let ((units (layer-units layer)))
         (dolist (unit units)
           (setf (unit-input-value unit)
                 (calculate-unit-input-value unit)))
         (map nil
              #'(lambda (unit value)
                  (setf (unit-output-value unit) value))
              units
              (activate layer))))))
  (mapcar #'unit-output-value
          (layer-units (output-layer (dnn-layers dnn)))))

(defun pick-data-set (dnn data-set)
  (pick-randomly data-set (dnn-mini-batch-size dnn)))

@export
@doc
"Test dnn"
(defun test (dnn data-set)
  (let* ((picked-data-set (pick-data-set dnn data-set))
         (layers (dnn-layers dnn))
         (output-layer (car (last layers)))
         (function-set (layer-function-set output-layer))
         (error-function (function-set-error-function function-set)))
    (/ (reduce #'+
               (mapcar #'(lambda (data)
                           (funcall error-function
                                    (predict dnn (data-input data))
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

(defun hidden-back-propagate (hidden-layer)
  (mapcar #'(lambda (unit value)
              (reduce #'+
                      (mapcar #'(lambda (connection)
                                  (* (unit-delta (connection-right-unit connection))
                                     (connection-weight connection)
                                     value))
                              (unit-right-connections unit))))
          (layer-units hidden-layer)
          (back-propagate-hidden-layer hidden-layer)))

@export
@doc
"Train dnn by given data-set"
(defun train (dnn data-set)
  (flet ((back-propagate (units values)
           (map nil
                #'(lambda (unit delta)
                    (setf (unit-delta unit) delta)
                    (dolist (connection (unit-left-connections unit))
                      (incf (connection-weight-diff connection)
                            (* delta (unit-output-value
                                      (connection-left-unit connection))))))
                units
                values)))
    (multiple-value-bind (means standard-deviations)
        (calculate-means-and-standard-deviations data-set)
      (setf (dnn-input-means dnn) means
            (dnn-input-standard-deviations dnn) standard-deviations)
      (let ((picked-data-set (pick-data-set dnn data-set)))
        (dolist (data picked-data-set)
          (predict dnn (data-input data))
          (dolist (layer (reverse (cdr (dnn-layers dnn))))
            (back-propagate
             (layer-units layer)
             (etypecase layer
               (output-layer (back-propagate-output-layer layer (data-expected data)))
               (hidden-layer (hidden-back-propagate layer)))))
          (dolist (outer-connections (dnn-connections dnn))
            (dolist (inner-connectios outer-connections)
              (dolist (connection inner-connectios)
                (decf (connection-weight connection)
                      (* (dnn-learning-coefficient dnn)
                         (connection-weight-diff connection)))
                (setf (connection-weight-diff connection) 0)))))))))
