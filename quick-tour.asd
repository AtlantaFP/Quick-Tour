#|
  This file is a part of quick-tour project.
  Copyright (c) 2018 Jud Taylor (jud.taylor@gmail.com)
|#

#|
  Project/github repo pair.

  Author: Jud Taylor (jud.taylor@gmail.com)
|#

(in-package :cl-user)
(defpackage quick-tour-asd
  (:use :cl :asdf))
(in-package :quick-tour-asd)

(defsystem quick-tour
  :version "0.1"
  :author "Jud Taylor"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "quick-tour"))))
  :description "Project/github repo pair."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op quick-tour-test))))
