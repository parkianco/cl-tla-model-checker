;;;; cl-tla-model-checker.asd
;;;; ASDF system definition for TLA+ Model Checker

(defsystem #:cl-tla-model-checker
  :name "cl-tla-model-checker"
  :version "1.0.0"
  :description "Pure Common Lisp TLA+ temporal logic model checker"
  :author "Parkian Company LLC"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "util")
     (:file "spec")
     (:file "state")
     (:file "temporal")
     (:file "checker"))))
  :in-order-to ((test-op (test-op #:cl-tla-model-checker/test))))

(defsystem #:cl-tla-model-checker/test
  :name "cl-tla-model-checker-test"
  :version "1.0.0"
  :description "Tests for cl-tla-model-checker"
  :depends-on (#:cl-tla-model-checker)
  :components
  ((:module "test"
    :components
    ((:file "test-tla"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-tla-model-checker.test :run-tests)))
