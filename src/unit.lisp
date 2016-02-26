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
(defclass bias-unit (unit)
  ((output-value :initform 1)))
