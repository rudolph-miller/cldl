(in-package :cl-user)
(defpackage cldl.connection
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.unit
                #:unit))
(in-package :cldl.connection)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Class of connection"
(defclass connection ()
  ((left-unit :initform nil
              :type (or null unit)
              :initarg :left-unit
              :accessor connection-left-unit)
   (right-unit :initform nil
               :type (or null unit)
               :initarg :right-unit
               :accessor connection-right-unit)
   (weight :initform 1
           :type number
           :initarg :weight
           :accessor connection-weight)
   (weight-diff :initform 0
                :type number
                :initarg :weight-diff
                :accessor connection-weight-diff)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (with-slots (weight weight-diff) connection
      (format stream
              ":WEIGHT ~a :WEIGHT-DIFF ~a"
              weight
              weight-diff))))
