(in-package :cl-user)
(defpackage quick-tour-test
  (:use :cl
        :quick-tour
        :prove))
(in-package :quick-tour-test)

;; NOTE: To run this test file, execute `(asdf:test-system :quick-tour)' in your Lisp.

(plan nil)

;; This is in /home/gt/quicklisp/dists/quicklisp/software/cl-project-20160531-git/skeleton/t/skeleton.lisp

(finalize)
