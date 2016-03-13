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
                #:unit-left-connections
                #:unit-right-connections
                #:unit-delta)
  (:import-from #:cldl.connection
                #:connect-units
                #:connection-left-unit
                #:connection-right-unit
                #:connection-weight)
  (:import-from #:cldl.differentiable-function
                #:d-function
                #:diff-funcall
                #:find-d-function))
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

(defun replace-d-functions (args indicator)
  (when-let ((name (getf args indicator)))
    (setf (getf args indicator)
          (find-d-function name))))

@export
(defclass hidden-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))
   (activation-function :initform nil
                        :type (or null d-function)
                        :initarg :activation-function
                        :accessor layer-activation-function)))

(defmethod initialize-instance :around ((class hidden-layer) &rest initargs)
  (replace-d-functions initargs :activation-function)
  (call-next-method))

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


(defmethod initialize-instance :around ((class output-layer) &rest initargs)
  (replace-d-functions initargs :activation-function)
  (replace-d-functions initargs :error-function)
  (call-next-method))

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

(defun calculate-unit-input-value (unit)
  (reduce #'+
          (mapcar #'(lambda (connection)
                      (* (unit-output-value
                          (connection-left-unit connection))
                         (connection-weight connection)))
                  (unit-left-connections unit))))

@export
(defgeneric propagate (layer)
  (:method ((layer layer))
    (let ((units (layer-units layer)))
    (dolist (unit units)
      (setf (unit-input-value unit)
            (calculate-unit-input-value unit)))
    (map nil
         #'(lambda (unit value)
             (setf (unit-output-value unit) value))
         units
         (activate layer)))))

(defmethod propagate ((layer input-layer))
  (dolist (unit (layer-units layer))
    (setf (unit-output-value unit) (unit-input-value unit))))

@export
(defgeneric activate (layer)
  (:method ((layer layer))
    (let* ((function (layer-activation-function layer))
           (units (layer-units layer))
           (input-values (mapcar #'unit-input-value units)))
      (funcall function input-values))))

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
              (diff-funcall function input-values)))))

@export
(defgeneric back-propagate-output-layer (output-layer expected)
  (:method ((output-layer output-layer) expected)
    (let* ((function (layer-error-function output-layer))
           (units (layer-units output-layer))
           (output-values (mapcar #'unit-output-value units)))
      (diff-funcall function output-values expected))))
