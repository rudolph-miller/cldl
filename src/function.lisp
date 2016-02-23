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
   (function :initform nil
             :type (or null function)
             :initarg :function
             :accessor function-set-function)
   (differentiation :initform nil
                    :type (or null function)
                    :initarg :differentiation
                    :accessor function-set-differentiation)
   (error :initform nil
          :type (or null function)
          :initarg :error
          :accessor function-set-error)
   (multiple-values :initform nil
                    :type boolean
                    :initarg :multiple-values
                    :accessor function-set-multiple-values)))

(defmethod print-object ((function-set function-set) stream)
  (print-unreadable-object (function-set stream :type t :identity t)
    (with-slots (name multiple-values) function-set
      (format stream
              ":NAME ~a :MULTIPLE-VALUES ~a"
              name
              multiple-values))))

(defvar *function-sets* nil)

@export
(defmacro def-function-set (name (&key multiple-values) &body definitions)
  `(push (make-instance 'function-set
                        :name ',name
                        ,@(loop for definition in definitions
                                nconc
                                (destructuring-bind (label lambda-list &body body)
                                    definition
                                  (list (ecase label
                                          (:function :function)
                                          (:fn :function)
                                          (:differentiation :differentiation)
                                          (:diff :differentiation)
                                          (:error :error)
                                          (:e :error))
                                        `(lambda ,lambda-list ,@body))))
                        :multiple-values ,multiple-values)
         *function-sets*))

@export
(defun find-function-set (name)
  (find name *function-sets*
        :test #'eql
        :key #'(lambda (function-set)
                 (function-set-name function-set))))
