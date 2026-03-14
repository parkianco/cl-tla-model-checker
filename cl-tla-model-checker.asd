;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-tla-model-checker.asd
;;;; ASDF system definition for TLA+ Model Checker

(asdf:defsystem #:cl-tla-model-checker
  :name "cl-tla-model-checker"
  :version "0.1.0"
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
  :in-order-to ((asdf:test-op (test-op #:cl-tla-model-checker/test))))

(asdf:defsystem #:cl-tla-model-checker/test
  :name "cl-tla-model-checker-test"
  :version "0.1.0"
  :description "Tests for cl-tla-model-checker"
  :depends-on (#:cl-tla-model-checker)
  :components
  ((:module "test"
    :components
    ((:file "test-tla"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-tla-model-checker.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
