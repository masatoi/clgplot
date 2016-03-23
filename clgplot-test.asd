#|
  This file is a part of clgplot project.
|#

(in-package :cl-user)
(defpackage clgplot-test-asd
  (:use :cl :asdf))
(in-package :clgplot-test-asd)

(defsystem clgplot-test
  :author ""
  :license ""
  :depends-on (:clgplot
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clgplot"))))
  :description "Test system for clgplot"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
