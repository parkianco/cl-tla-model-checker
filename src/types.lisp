;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-tla-model-checker)

;;; Core types for cl-tla-model-checker
(deftype cl-tla-model-checker-id () '(unsigned-byte 64))
(deftype cl-tla-model-checker-status () '(member :ready :active :error :shutdown))
