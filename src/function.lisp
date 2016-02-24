(in-package :cl-user)
(defpackage cldl.function
  (:use #:cl
        #:annot.doc
        #:annot.class))
(in-package :cldl.function)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass function-set ()
  ((name :initform ""
         :type string
         :initarg :name
         :accessor function-set-name)
   (activation-function :initform nil
                        :type (or null function)
                        :initarg :activation-function
                        :accessor function-set-activation-function)
   (multiple-values :initform nil
                    :type boolean
                    :initarg :multiple-values
                    :accessor function-set-multiple-values)))

@export
@export-accessors
(defclass hidden-function-set (function-set)
  ((diff-of-activation-function :initform nil
                                :type (or null function)
                                :initarg :diff-of-activation-function
                                :accessor function-set-diff-of-activation-function
                                :documentation "Differential of activation function.")))

@export
@export-accessors
(defclass output-function-set (function-set)
  ((error-function :initform nil
                   :type (or null function)
                   :initarg :error-function
                   :accessor function-set-error-function)
   (delta-function :initform nil
                   :type (or null function)
                   :initarg :delta-function
                   :accessor function-set-delta-function)))

(defmethod print-object ((function-set function-set) stream)
  (print-unreadable-object (function-set stream :type t :identity t)
    (with-slots (name multiple-values) function-set
      (format stream
              ":NAME ~a :MULTIPLE-VALUES ~a"
              name
              multiple-values))))

(defvar *function-sets* nil)

(defparameter *function-set-name-aliases*
  '((:activation . :activation-function)
    (:a . :activation-function)
    (:diff-of-activation . :diff-of-activation-function)
    (:diff-of-a . :diff-of-activation-function)
    (:error . :error-function)
    (:e . :error)
    (:delta . :delta-function)
    (:d . :delta-functio)))

@export
(defmacro def-function-set (name (type &key multiple-values) &body definitions)
  `(push (make-instance ',(ecase type
                            (:hidden 'hidden-function-set)
                            (:output 'output-function-set))
                        :name ',name
                        ,@(loop for definition in definitions
                                nconc
                                (destructuring-bind (label lambda)
                                    definition
                                  (list (or (cdr (assoc label *function-set-name-aliases*))
                                            label)
                                        lambda)))
                        :multiple-values ,multiple-values)
         *function-sets*))

@export
(defun find-function-set (name)
  (find name *function-sets*
        :test #'eql
        :key #'(lambda (function-set)
                 (function-set-name function-set))))
