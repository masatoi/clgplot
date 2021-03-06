#|
  This file is a part of clgplot project.
|#

(in-package :cl-user)
(defpackage clgplot-asd
  (:use :cl :asdf))
(in-package :clgplot-asd)

(defsystem clgplot
  :version "0.2"
  :author "Satoshi Imai"
  :license "MIT Licence"
  :depends-on (:uiop :iterate)
  :components ((:module "src"
                :components
                ((:file "clgplot"))))
  :description "A Gnuplot front-end for Common lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clgplot-test))))
