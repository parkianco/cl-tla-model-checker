;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-tla-model-checker)

(define-condition cl-tla-model-checker-error (error)
  ((message :initarg :message :reader cl-tla-model-checker-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-tla-model-checker error: ~A" (cl-tla-model-checker-error-message condition)))))
