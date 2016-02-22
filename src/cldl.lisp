(in-package :cl-user)
(defpackage cldl
  (:use #:cl
        #:annot.doc)
  (:import-from #:cldl.dnn
                #:predict)
  (:export #:predict))
(in-package :cldl)

(syntax:use-syntax :annot)
