(in-package :cl-user)
(defpackage cldl.dnn
  (:use #:cl
        #:annot.doc
        #:annot.class))
(in-package :cldl.dnn)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Class of Deep Neural Network."
(defclass dnn ()
  ((units :initform nil
          :type list
          :initarg :units
          :accessor dnn-units)
   (connections :initform nil
                :type list
                :initarg :connections
                :accessor dnn-connections)
   (learning-coefficient :initform 0.01
                         :type number
                         :initarg :learning-coefficient
                         :accessor dnn-learning-coefficient)
   (mini-batch-size :initform 10)))

(defmethod print-object ((dnn dnn) stream)
  (print-unreadable-object (dnn stream :type t :identity t)
    (with-slots (learning-coefficient mini-batch-size) dnn
      (format stream
              ":LEARING-COEFFICIENT ~a :MINI-BATCH-SIZE ~a"
              learning-coefficient
              mini-batch-size))))

(defun predict (dnn input)
  (declare (ignore dnn input)))
