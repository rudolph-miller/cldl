(in-package :cl-user)
(defpackage cldl.connection
  (:use #:cl
        #:annot.doc
        #:annot.class)
  (:import-from #:cldl.util
                #:normal-random)
  (:import-from #:cldl.unit
                #:unit
                #:bias-unit
                #:unit-left-connections
                #:unit-right-connections))
(in-package :cldl.connection)

(syntax:use-syntax :annot)

@export
(defvar +DEFAULT-WEIGHT+ 0)

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

@export
@doc
"Connect given units and return connection-set"
(defun connect-units (left-units right-units)
  (flet ((%connect (left-unit right-unit)
           (let* ((weight (if (typep left-unit 'bias-unit)
                              +DEFAULT-WEIGHT+
                              (+ +DEFAULT-WEIGHT+ (normal-random 0 1))))
                  (connection (make-instance 'connection
                                             :weight weight
                                             :left-unit left-unit
                                             :right-unit right-unit)))
             (push connection (unit-left-connections right-unit))
             (push connection (unit-right-connections left-unit))
             connection)))
    (mapcar #'(lambda (left-unit)
                (mapcar #'(lambda (right-unit)
                            (%connect left-unit right-unit))
                        right-units))
            left-units)))
