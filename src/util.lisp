;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; util.lisp
;;;; Utility functions for TLA+ Model Checker

(in-package #:cl-tla-model-checker)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defparameter *tla-output-dir* #P"/tmp/tla-specs/"
  "Default output directory for generated TLA+ specifications.")

(defparameter *tla-indent* 2
  "Indentation width for generated TLA+ code.")

(defparameter *tla-line-width* 80
  "Maximum line width for generated TLA+ code.")

(defparameter *tlc-path* nil
  "Path to TLC model checker JAR file.")

(defparameter *tla-debug* nil
  "Enable debug output for TLA+ generation.")

;;; ============================================================================
;;; Indentation and Formatting
;;; ============================================================================

(defun make-indent (n)
  "Create an indentation string of N spaces."
  (make-string n :initial-element #\Space))

(defun emit-value (value)
  "Emit a simple TLA+ value."
  (cond
    ((null value) "{}")
    ((eq value t) "TRUE")
    ((numberp value) (format nil "~A" value))
    ((stringp value) (format nil "\"~A\"" value))
    ((symbolp value) (symbol-name value))
    ((listp value) (format nil "{~{~A~^, ~}}" (mapcar #'emit-value value)))
    (t (format nil "~S" value))))

(defun format-tla-value (value)
  "Format a Lisp value for TLC configuration."
  (cond
    ((null value) "<<>>")
    ((eq value t) "TRUE")
    ((numberp value) (format nil "~A" value))
    ((stringp value) (format nil "\"~A\"" value))
    ((symbolp value) (symbol-name value))
    ((listp value) (format nil "{~{~A~^, ~}}" (mapcar #'format-tla-value value)))
    (t (format nil "~S" value))))

;;; ============================================================================
;;; String Utilities
;;; ============================================================================

(defun parse-integer-from (string position)
  "Parse an integer from STRING starting at POSITION."
  (let ((start (position-if #'digit-char-p string :start position)))
    (when start
      (let ((end (position-if-not #'digit-char-p string :start start)))
        (parse-integer (subseq string start (or end (length string))))))))

(defun extract-error (output-string)
  "Extract error message from TLC output."
  (let ((start (search "Error:" output-string)))
    (when start
      (let ((end (or (position #\Newline output-string :start start)
                     (length output-string))))
        (subseq output-string start end)))))

;;; ============================================================================
;;; File I/O
;;; ============================================================================

(defun emit-to-file (object file-path)
  "Emit a TLA+ specification to a file."
  (ensure-directories-exist file-path)
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    (write-string (generate-tla-spec object) out))
  file-path)
