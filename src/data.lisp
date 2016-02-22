(in-package :cl-user)
(defpackage cldl.data
  (:use #:cl
        #:annot.doc
        #:annot.class))
(in-package :cldl.data)

(syntax:use-syntax :annot)

@export
@export-accessors
@doc
"Class of data"
(defclass data ()
  ((expected :initform nil
             :type (or null number)
             :initarg :expected
             :accessor data-expected)
   (input :initform nil
          :type (or null list)
          :initarg :input
          :accessor data-input)))
