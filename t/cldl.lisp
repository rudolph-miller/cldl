(in-package :cl-user)
(defpackage cldl-test
  (:use :cl
        :cldl
        :prove))
(in-package :cldl-test)

(plan nil)

(defun data-path ()
  (flet ((component-by-name (name components)
           (find name
                 components
                 :key #'asdf/component:component-name
                 :test #'equal)))
    (let* ((t-module (component-by-name "t"
                                        (asdf/component:module-components
                                         (asdf:find-system :cldl-test))))
           (data (component-by-name "data.txt"
                                    (asdf/component:module-components t-module))))
      (when data
        (asdf/component:component-pathname data)))))

(defun data ()
  (with-open-file (in (data-path) :direction :input)
    (loop with result = nil
          for line = (read-line in nil nil)
          while line
          do (push line result)
          finally (return result))))

(finalize)
