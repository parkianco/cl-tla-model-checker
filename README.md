# cl-tla-model-checker

Pure Common Lisp implementation of Tla Model Checker

## Overview
This library provides a robust, zero-dependency implementation of Tla Model Checker for the Common Lisp ecosystem. It is designed to be highly portable, performant, and easy to integrate into any SBCL/CCL/ECL environment.

## Getting Started

Load the system using ASDF:

```lisp
(asdf:load-system #:cl-tla-model-checker)
```

## Usage Example

```lisp
;; Initialize the environment
(let ((ctx (cl-tla-model-checker:initialize-tla-model-checker :initial-id 42)))
  ;; Perform batch processing using the built-in standard toolkit
  (multiple-value-bind (results errors)
      (cl-tla-model-checker:tla-model-checker-batch-process '(1 2 3) #'identity)
    (format t "Processed ~A items with ~A errors.~%" (length results) (length errors))))
```

## License
Apache-2.0
