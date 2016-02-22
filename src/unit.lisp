(in-package :cl-user)
(defpackage cldl.unit
  (:use #:cl
        #:annot.doc
        #:annot.class))
(in-package :cldl.unit)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Base class for Unit."
(defclass unit ()
  ((left-connections :initform nil
                     :type list
                     :initarg :left-connections
                     :accessor unit-left-connections)
   (right-connections :initform nil
                      :type list
                      :initarg :right-connections
                      :accessor unit-right-connections)
   (input-value :initform 0
                :type number
                :initarg :input-value
                :accessor unit-input-value)
   (output-value :initform 0
                 :type number
                 :initarg :output-value
                 :accessor unit-output-value)
   (delta :initform 0
          :type number
          :initarg :delta
          :accessor unit-delta)))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type t :identity t)
    (with-slots (input-value output-value delta) unit
      (format stream
              ":INPUT ~a :OUTPUT ~a :DELTA ~a"
              input-value output-value delta))))

@export
(defclass input-unit (unit) ())

@export
(defclass hidden-unit (unit) ())

@export
(defclass bias-unit (unit) ())

@export
(defclass output-unit (unit) ())

@export
(defun generate-units (num-of-units)
  (when (<= (length num-of-units) 2)
    (error "At least 1 hidden units required."))
  (let (input-units hidden-unit-set output-units)
    (push (make-instance 'bias-unit) input-units)
    (dotimes (_ (car num-of-units))
      (push (make-instance 'input-unit) input-units))
    (loop for (i rest) on (cdr num-of-units)
          if rest
            do (let (hidden-units)
                 (push (make-instance 'bias-unit) hidden-units)
                 (dotimes (_ i)
                   (push (make-instance 'hidden-unit) hidden-units))
                 (push (nreverse hidden-units) hidden-unit-set))
          else
            do (dotimes (_ i)
                 (push (make-instance 'output-unit) output-units)))
    (append (list (nreverse input-units))
            (nreverse hidden-unit-set)
            (list (nreverse output-units)))))

@export
@doc
"Return input-units from generated units"
(defun input-units (units)
  (car units))

@export
@doc
"Return hidden-unit-set from generated units"
(defun hidden-unit-set (units)
  (butlast (cdr units)))

@export
@doc
"Return output-units from genarted units"
(defun output-units (units)
  (car (last units)))

@export
@doc
"Activate function.
Set output-value as activated input-value.
Default: Rectified Linear Unit"
(defgeneric activate (unit)
  (:method ((unit hidden-unit))
    (let ((input (unit-input-value unit)))
    (setf (unit-output-value unit)
          (if (< input 0)
              0
              input)))))
