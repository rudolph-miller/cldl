(in-package :cl-user)
(defpackage cldl.dnn
  (:use #:cl
        #:annot.doc))
(in-package :cldl.dnn)

(syntax:use-syntax :annot)

(defun predict (dnn input)
  (declare (ignore dnn input)))
