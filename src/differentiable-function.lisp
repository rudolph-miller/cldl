(in-package :cl-user)
(defpackage cldl.differentiable-function
  (:use #:cl
        #:annot.class)
  (:import-from #:c2mop
                #:funcallable-standard-class
                #:set-funcallable-instance-function))
(in-package :cldl.differentiable-function)

(syntax:use-syntax :annot)

(defvar *d-functions* nil)

@export
@export-accessors
@doc
"Differentiable Function"
(defclass d-function ()
  ((name :initform nil
         :type symbol
         :initarg :name
         :accessor d-function-name)
   (fn :initform nil
       :type (or null function)
       :initarg :fn
       :accessor d-function-fn)
   (diff :initform nil
         :type (or null function)
         :initarg :diff
         :accessor d-function-diff)
   (take-value-set :initform nil
                   :type boolean
                   :initarg :take-value-set
                   :accessor d-function-take-value-set))
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((d-function d-function) &key)
  (push d-function *d-functions*)
  (with-slots (fn) d-function
    (set-funcallable-instance-function
     d-function
     #'(lambda (&rest args)
         (apply fn args)))))

(defmethod print-object ((d-function d-function) stream)
  (print-unreadable-object (d-function stream :type t :identity t)
    (with-slots (name) d-function
      (format stream "~a" name))))

@export
(defun diff-funcall (d-function &rest args)
  (let ((diff (d-function-diff d-function)))
    (apply diff args)))

@export
(defun find-d-function (name)
  (find name *d-functions* :key #'d-function-name))

@export
(defmacro def-d-function (name-and-options args &body definitions)
  (flet ((make-fn (type)
           (let ((definition (cdr (find type definitions :key #'car))))
             (when definition
               `(lambda ,args ,(cons 'progn definition))))))
    (let ((name (if (consp name-and-options)
                    (car name-and-options)
                    name-and-options))
          (options (when (consp name-and-options)
                     (cdr name-and-options)))
          (fn (make-fn :fn))
          (diff (make-fn :diff)))
      `(make-instance 'd-function
                      :name ',name
                      :fn ,fn
                      :diff ,diff
                      ,@options))))
