#|
  This file is a part of cldl project.
  Copyright (c) 2016 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage cldl-asd
  (:use :cl :asdf))
(in-package :cldl-asd)

(defsystem cldl
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/cldl"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :closer-mop)
  :components ((:module "src"
                :components
                ((:file "util")
                 (:file "unit")
                 (:file "layer")
                 (:file "connection")
                 (:file "data")
                 (:file "dnn")
                 (:file "cldl"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cldl-test))))
