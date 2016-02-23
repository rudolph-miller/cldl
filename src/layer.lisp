(in-package :cl-user)
(defpackage cldl.layer
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.unit
                #:unit
                #:input-unit
                #:hidden-unit
                #:bias-unit
                #:output-unit))
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
              :accessor layer-unit-type)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type t :identity t)
    (with-slots (bias-unit units unit-type) layer
      (format stream
              ":BIAS-UNIT ~a :UNITS ~a :UNIT-TYPE ~a"
              (if bias-unit 1 0)
              (length units)
              unit-type))))

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
(defun make-layer (type num-of-units)
  (let* ((object (make-instance type))
         (units (loop repeat num-of-units
                      collecting (make-instance (layer-unit-type object)))))
    (setf (layer-units object) units)
    object))

@export
(defun make-layers (list)
  (loop for (type . num-of-units) in list
        collecting (make-layer type num-of-units)))
