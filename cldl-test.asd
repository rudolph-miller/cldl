#|
  This file is a part of cldl project.
  Copyright (c) 2016 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage cldl-test-asd
  (:use :cl :asdf))
(in-package :cldl-test-asd)

(defsystem cldl-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/cldl"
  :depends-on (:cldl
               :prove
               :split-sequence)
  :components ((:module "t"
                :serial t
                :components
                ((:static-file "data.txt")
                 (:test-file "cldl"))))
  :description "Test system for cldl."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
