;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; package.lisp
;;;; Package definitions for TLA+ Model Checker

(in-package #:cl-user)

(defpackage #:cl-tla-model-checker
  (:nicknames #:tla #:tla-mc)
  (:use #:cl)
  (:documentation "Pure Common Lisp TLA+ temporal logic model checker.
Provides state space exploration, temporal property verification,
and refinement checking for distributed systems specifications.")

  ;; AST Types
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:tla-ast-node
   #:make-tla-ast-node
   #:tla-ast-node-p
   #:tla-ast-kind
   #:tla-ast-value
   #:tla-ast-children
   #:tla-ast-metadata
   #:tla-ast-source-loc
   #:make-ast
   #:ast-kind-p
   #:ast-walk
   #:ast-transform)

  ;; Module Types
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:tla-module
   #:make-tla-module
   #:tla-module-p
   #:tla-module-name
   #:tla-module-extends
   #:tla-module-constants
   #:tla-module-variables
   #:tla-module-definitions
   #:tla-module-theorems

   #:tla-definition
   #:make-tla-definition
   #:tla-definition-p
   #:tla-definition-name
   #:tla-definition-params
   #:tla-definition-body
   #:tla-definition-local-p

   #:tla-theorem
   #:make-tla-theorem
   #:tla-theorem-p
   #:tla-theorem-name
   #:tla-theorem-formula

   #:tla-constant
   #:make-tla-constant
   #:tla-constant-p
   #:tla-constant-name

   #:tla-var
   #:make-tla-var
   #:tla-var-p
   #:tla-var-name)

  ;; State Machine Types
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:state-machine
   #:make-state-machine
   #:state-machine-p
   #:state-machine-name
   #:state-machine-variables
   #:state-machine-init
   #:state-machine-next
   #:state-machine-actions
   #:state-machine-fairness
   #:state-machine-spec
   #:create-state-machine

   #:sm-variable
   #:make-sm-variable
   #:sm-variable-p
   #:sm-variable-name
   #:sm-variable-type
   #:sm-variable-initial

   #:sm-action
   #:make-sm-action
   #:sm-action-p
   #:sm-action-name
   #:sm-action-params
   #:sm-action-guard
   #:sm-action-effect
   #:sm-action-fairness)

  ;; Temporal Operators
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:temporal-always
   #:temporal-eventually
   #:temporal-until
   #:temporal-leads-to
   #:temporal-weak-until
   #:temporal-release
   #:temporal-next
   #:temporal-stuttering
   #:temporal-enabled
   #:temporal-primed
   #:always-implies
   #:always-eventually
   #:eventually-always
   #:response
   #:precedence
   #:absence
   #:existence
   #:bounded-response)

  ;; Fairness
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:fairness-constraint
   #:make-fairness-constraint
   #:fairness-constraint-p
   #:fairness-constraint-kind
   #:fairness-constraint-action
   #:fairness-constraint-vars
   #:weak-fair
   #:strong-fair
   #:fairness-formula
   #:compose-fairness)

  ;; Refinement
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:refinement-mapping
   #:make-refinement-mapping
   #:refinement-mapping-p
   #:refinement-mapping-source
   #:refinement-mapping-target
   #:refinement-mapping-state-map
   #:refinement-mapping-action-map
   #:make-refinement
   #:check-refinement
   #:stuttering-simulation
   #:trace-containment)

  ;; Model Checking
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:model-config
   #:make-model-config
   #:model-config-p
   #:model-config-spec
   #:model-config-invariants
   #:model-config-properties
   #:model-config-constants
   #:model-config-symmetry
   #:model-config-constraint
   #:tlc-result
   #:tlc-result-p
   #:tlc-result-success-p
   #:tlc-result-states-found
   #:tlc-result-counterexample
   #:generate-cfg
   #:run-model-check
   #:parse-tlc-output
   #:analyze-counterexample)

  ;; State Space Exploration
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:state-space
   #:make-state-space
   #:state-space-p
   #:explore-states
   #:check-invariant
   #:check-property
   #:find-counterexample
   #:state-graph
   #:reachable-states)

  ;; Spec Generation
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:generate-tla-spec
   #:lisp-to-tla
   #:defspec
   #:with-spec-context
   #:emit-module
   #:emit-formula
   #:emit-to-file)

  ;; Configuration
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:*tla-output-dir*
   #:*tla-indent*
   #:*tla-line-width*
   #:*tlc-path*
   #:*tla-debug*))

(defpackage #:cl-tla-model-checker.test
  (:use #:cl #:cl-tla-model-checker)
  (:export
   #:with-tla-model-checker-timing
   #:tla-model-checker-batch-process
   #:tla-model-checker-health-check#:run-tests))
