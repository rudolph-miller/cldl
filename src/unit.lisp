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
(defclass bias-unit (unit)
  ((output-value :initform 1)))

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
    (loop for i in (butlast (cdr num-of-units))
          do (let (hidden-units)
               (push (make-instance 'bias-unit) hidden-units)
               (loop repeat i
                     do (push (make-instance 'hidden-unit) hidden-units))
               (push (nreverse hidden-units) hidden-unit-set)))
    (loop repeat (car (last num-of-units))
          do (push (make-instance 'output-unit) output-units))
    (append (list (nreverse input-units))
            (nreverse hidden-unit-set)
            (list (nreverse output-units)))))

@export
(defun input-units (units)
  (cdr (car units)))

@export
(defun hidden-unit-set (units)
  (mapcar #'cdr (butlast (cdr units))))

@export
(defun output-units (units)
  (car (last units)))
