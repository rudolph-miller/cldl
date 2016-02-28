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
  (:import-from #:cldl.function
                #:hidden-function-set
                #:output-function-set
                #:function-set-name
                #:function-set-activation-function
                #:function-set-diff-of-activation-function
                #:function-set-delta-function
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
   (function-set :initform nil
                 :type null
                 :initarg :function-set
                 :accessor layer-function-set)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type t :identity t)
    (with-slots (bias-unit units function-set) layer
      (format stream
              ":BIAS-UNIT ~a :UNITS ~a :FUNCTION-SET ~a"
              (if bias-unit 1 0)
              (length units)
              (when function-set (function-set-name function-set))))))

@export
(defclass input-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))))

@export
(defclass hidden-layer (layer)
  ((bias-unit :initform (make-instance 'bias-unit))
   (function-set :type hidden-function-set)))

@export
(defclass output-layer (layer)
  ((function-set :type output-function-set)))

@export
(defun make-layer (type num-of-units function-set)
  (let* ((object (make-instance type
                                :function-set (etypecase function-set
                                                (symbol (find-function-set function-set))
                                                (function function-set))))
         (units (loop repeat num-of-units
                      collecting (make-instance 'unit))))
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
    (let* ((function-set (layer-function-set layer))
           (function (function-set-activation-function function-set))
           (units (layer-units layer))
           (input-values (mapcar #'unit-input-value units)))
      (if (function-set-multiple-values function-set)
          (funcall function input-values)
          (mapcar function input-values)))))

@export
(defgeneric back-propagate-hidden-layer (hidden-layer)
  (:method ((hidden-layer hidden-layer))
    (let* ((function-set (layer-function-set hidden-layer))
           (function (function-set-diff-of-activation-function function-set))
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
              (if (function-set-multiple-values function-set)
                  (funcall function input-values)
                  (mapcar function input-values))))))

@export
(defgeneric back-propagate-output-layer (output-layer expected)
  (:method ((output-layer output-layer) expected)
    (let* ((function-set (layer-function-set output-layer))
           (function (function-set-delta-function function-set))
           (units (layer-units output-layer))
           (output-values (mapcar #'unit-output-value units)))
      (if (function-set-multiple-values function-set)
          (funcall function output-values expected)
          (mapcar #'(lambda (value)
                      (funcall function value expected))
                  output-values)))))
