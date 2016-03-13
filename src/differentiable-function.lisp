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

(defun funcall-or-mapcar (d-function type value &optional (expected nil expected-given))
  (let ((fn (ecase type
              (:fn (d-function-fn d-function))
              (:diff (d-function-diff d-function)))))
    (if (d-function-take-value-set d-function)
        (if expected-given
            (funcall fn value expected)
            (funcall fn value))
        (if expected-given
            (mapcar fn value expected)
            (mapcar fn value)))))

(defmethod initialize-instance :after ((d-function d-function) &key)
  (push d-function *d-functions*)
  (set-funcallable-instance-function
   d-function
   #'(lambda (value &optional (expected nil expected-given))
       (if expected-given
           (funcall-or-mapcar d-function :fn value expected)
           (funcall-or-mapcar d-function :fn value)))))

(defmethod print-object ((d-function d-function) stream)
  (print-unreadable-object (d-function stream :type t :identity t)
    (with-slots (name) d-function
      (format stream "~a" name))))

@export
(defun diff-funcall (d-function value &optional (expected nil expected-given))
  (let ((diff (d-function-diff d-function)))
    (if expected-given
        (funcall diff value expected)
        (funcall diff value))))

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
