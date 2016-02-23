(in-package :cl-user)
(defpackage cldl.layer
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.unit
                #:unit
                #:unit-input-value
                #:input-unit
                #:hidden-unit
                #:bias-unit
                #:output-unit)
  (:import-from #:cldl.function
                #:function-set
                #:function-set-name
                #:function-set-activation-function
                #:function-set-diff-of-activation-function
                #:function-set-diff-of-error-function
                #:function-set-multiple-values
                #:find-function-set))
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
          :accessor layer-units)
   (unit-type :initform nil
              :type (or symbol class)
              :initarg :unit-type
              :accessor layer-unit-type)
   (function-set :initform nil
                 :type (or null function-set)
                 :initarg :function-set
                 :accessor layer-function-set)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type t :identity t)
    (with-slots (bias-unit units unit-type function-set) layer
      (format stream
              ":BIAS-UNIT ~a :UNITS ~a :UNIT-TYPE ~a :FUNCTION-SET ~a"
              (if bias-unit 1 0)
              (length units)
              unit-type
              (when function-set (function-set-name function-set))))))

@export
(defclass input-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))
   (unit-type :initform 'input-unit)))

@export
(defclass hidden-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))
   (unit-type :initform 'hidden-unit)))

@export
(defclass output-layer (layer)
  ((unit-type :initform 'output-unit)))

@export
(defun make-layer (type num-of-units function-set)
  (let* ((object (make-instance type
                                :function-set (etypecase function-set
                                                (symbol (find-function-set function-set))
                                                (function function-set))))
         (units (loop repeat num-of-units
                      collecting (make-instance (layer-unit-type object)))))
    (setf (layer-units object) units)
    object))

@export
(defun make-layers (list)
  (loop for (type num-of-units function-set) in list
        collecting (make-layer type num-of-units function-set)))

@export
(defun output-layer (layers)
  (car (last layers)))

@export
(defgeneric activate (layer)
  (:method ((layer layer))
    (let* ((function-set (layer-function-set layer))
           (function (function-set-activation-function function-set))
           (units (layer-units layer))
           (input-values (mapcar #'unit-input-value units)))
      (if (function-set-multiple-values function-set)
          (funcall function input-values)
          (mapcar function input-values)))))

@export
(defgeneric backpropagate-hidden-layer (hidden-layer)
  (:method ((hidden-layer hidden-layer))
    (let* ((function-set (layer-function-set hidden-layer))
           (function (function-set-diff-of-activation-function function-set))
           (units (layer-units hidden-layer))
           (input-values (mapcar #'unit-input-value units)))
      (if (function-set-multiple-values function-set)
          (funcall function input-values)
          (mapcar function input-values)))))

@export
(defgeneric backpropagate-output-layer (output-layer expected)
  (:method ((output-layer output-layer) expected)
    (let* ((function-set (layer-function-set output-layer))
           (function (function-set-diff-of-error-function function-set))
           (units (layer-units output-layer))
           (input-values (mapcar #'unit-input-value units)))
      (if (function-set-multiple-values function-set)
          (funcall function input-values expected)
          (mapcar #'(lambda (value)
                      (funcall function value expected))
                  input-values)))))
