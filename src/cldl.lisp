(in-package :cl-user)
(defpackage cldl
  (:use #:cl
        #:annot.doc)
  (:import-from #:cldl.differentiable-function
                #:find-d-function)
  (:import-from #:cldl.functions
                #:softmax
                #:multi-class-cross-entropy
                #:rectified-linear-unit)
  (:import-from #:cldl.layer
                #:input-layer
                #:hidden-layer
                #:output-layer
                #:make-layers
                #:layer-should-connect-units
                #:connect-layers)
  (:import-from #:cldl.data
                #:make-data-set
                #:data-input
                #:data-expected)
  (:import-from #:cldl.dnn
                #:dnn
                #:predict
                #:test
                #:train
                #:run)
  (:export #:find-d-function ;; differentiable-function
           #:def-d-function

           #:softmax ;; functions
           #:multi-class-cross-entropy
           #:rectified-linear-unit

           #:input-layer ;; layer
           #:hidden-layer
           #:output-layer
           #:make-layers
           #:layer-should-connect-units
           #:connect-layers

           #:make-data-set ;; data
           #:data-input
           #:data-expected

           #:dnn ;; dnn
           #:predict
           #:test
           #:train
           #:run))
(in-package :cldl)
