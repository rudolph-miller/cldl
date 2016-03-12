(in-package :cl-user)
(defpackage cldl.layer
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:cldl.unit
                #:unit
                #:bias-unit
                #:unit-input-value
                #:unit-output-value
                #:unit-right-connections
                #:unit-delta)
  (:import-from #:cldl.connection
                #:connect-units
                #:connection-right-unit
                #:connection-weight)
  (:import-from #:cldl.differentiable-function
                #:d-function
                #:d-function-take-value-set
                #:diff-funcall))
(in-package :cldl.layer)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Class of layer (set of units)"
(defclass layer ()
  ((bias-unit :initform nil
              :type (or null bias-unit)
              :initarg :bias-unit
              :accessor layer-bias-unit)
   (units :initform nil
          :type list
          :initarg :units
          :accessor layer-units)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type t :identity t)
    (with-slots (bias-unit units) layer
      (format stream
              ":BIAS-UNIT ~a :UNITS ~a"
              (if bias-unit 1 0)
              (length units)))))

@export
(defclass input-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))))

@export
(defclass hidden-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))
   (activation-function :initform nil
                        :type (or null d-function)
                        :initarg :activation-function
                        :accessor layer-activation-function)))

@export
(defclass output-layer (layer)
  ((activation-function :initform nil
                        :type (or null d-function)
                        :initarg :activation-function
                        :accessor layer-activation-function)
   (error-function :initform nil
                   :type (or null d-function)
                   :initarg :error-function
                   :accessor layer-error-function)))

@export
(defun make-layer (type num-of-units &rest args)
  (let* ((object (apply #'make-instance
                        type
                        args))
         (units (loop repeat num-of-units
                      collecting (make-instance 'unit))))
    (setf (layer-units object) units)
    object))

@export
(defun make-layers (list)
  (loop for (type num-of-units . args) in list
        collecting (apply #'make-layer type num-of-units args)))

@export
(defun output-layer (layers)
  (car (last layers)))

@export
(defgeneric layer-should-connect-units (layer)
  (:method ((layer input-layer))
    (cons (layer-bias-unit layer)
          (layer-units layer)))
  (:method ((layer hidden-layer))
    (cons (layer-bias-unit layer)
          (layer-units layer)))
  (:method ((layer output-layer))
    (layer-units layer)))

@export
(defun connect-layers (layers)
  (mapcar #'(lambda (left-layer right-layer)
              (connect-units
               (layer-should-connect-units left-layer)
               (remove-if #'(lambda (unit)
                              (typep unit 'bias-unit))
                          (layer-should-connect-units right-layer))))
          layers
          (cdr layers)))

@export
(defgeneric activate (layer)
  (:method ((layer layer))
    (let* ((function (layer-activation-function layer))
           (units (layer-units layer))
           (input-values (mapcar #'unit-input-value units)))
      (if (d-function-take-value-set function)
          (funcall function input-values)
          (mapcar function input-values)))))

@export
(defgeneric back-propagate-hidden-layer (hidden-layer)
  (:method ((hidden-layer hidden-layer))
    (let* ((function (layer-activation-function hidden-layer))
           (units (layer-units hidden-layer))
           (input-values (mapcar #'unit-input-value units)))
      (mapcar #'(lambda (unit value)
                  (reduce #'+
                          (mapcar #'(lambda (connection)
                                      (* (unit-delta
                                          (connection-right-unit connection))
                                         (connection-weight connection)
                                         value))
                                  (unit-right-connections unit))))
              (layer-units hidden-layer)
              (if (d-function-take-value-set function)
                  (diff-funcall function input-values)
                  (mapcar #'(lambda (value)
                              (diff-funcall function value))
                          input-values))))))

@export
(defgeneric back-propagate-output-layer (output-layer expected)
  (:method ((output-layer output-layer) expected)
    (let* ((function (layer-error-function output-layer))
           (units (layer-units output-layer))
           (output-values (mapcar #'unit-output-value units)))
      (if (d-function-take-value-set function)
          (diff-funcall function output-values expected)
          (mapcar #'(lambda (value)
                      (diff-funcall function value expected))
                  output-values)))))
