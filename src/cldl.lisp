(in-package :cl-user)
(defpackage cldl
  (:use #:cl
        #:annot.doc)
  (:import-from #:cldl.functions
                #:softmax
                #:rectified-linear-unit)
  (:import-from #:cldl.layer
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:make-layers)
  (:import-from #:cldl.connection
                #:connect)
  (:import-from #:cldl.data
                #:make-data-set
                #:data-input
                #:data-expected)
  (:import-from #:cldl.dnn
                #:dnn
                #:predict
                #:test
                #:train)
  (:export #:def-function-set ;; function
           #:find-function-set

           #:softmax ;; functions
           #:rectified-linear-unit

           #:input-layer ;; layer
           #:hidden-layer
           #:output-layer
           #:make-layers

           #:connect ;; connection

           #:make-data-set ;; data
           #:data-input
           #:data-expected

           #:dnn ;; dnn
           #:predict
           #:test
           #:train))
(in-package :cldl)
