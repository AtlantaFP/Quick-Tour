#|
  This file is a part of quick-tour project.
  Copyright (c) 2018 Jud Taylor (jud.taylor@gmail.com)
|#

(in-package :cl-user)
(defpackage quick-tour-test-asd
  (:use :cl :asdf))
(in-package :quick-tour-test-asd)

(defsystem quick-tour-test
  :author "Jud Taylor"
  :license "LLGPL"
  :depends-on (:quick-tour
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "quick-tour"))))
  :description "Test system for quick-tour"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
