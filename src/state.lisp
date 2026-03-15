;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; state.lisp
;;;; State space exploration and representation

(in-package #:cl-tla-model-checker)

;;; ============================================================================
;;; State Representation
;;; ============================================================================

(defstruct (model-state (:conc-name mst-))
  "A state in the model's state space."
  (id 0 :type integer)
  (values nil :type list)      ; ((var . value) ...)
  (hash 0 :type integer)
  (depth 0 :type integer)
  (predecessor nil)            ; Previous state (or nil for initial)
  (action nil))                ; Action that led to this state

(defun state-hash (state-values)
  "Compute a hash for state values."
  (sxhash state-values))

(defun states-equal-p (s1 s2)
  "Check if two states have equal variable bindings."
  (equal (mst-values s1) (mst-values s2)))

(defun state-get (state var)
  "Get the value of VAR in STATE."
  (cdr (assoc var (mst-values state) :test #'equal)))

(defun state-with (state var value)
  "Create a new state with VAR set to VALUE."
  (let ((new-values (cons (cons var value)
                          (remove var (mst-values state)
                                  :key #'car :test #'equal))))
    (make-model-state
     :id (1+ (mst-id state))
     :values new-values
     :hash (state-hash new-values)
     :depth (1+ (mst-depth state))
     :predecessor state)))

;;; ============================================================================
;;; State Space Structure
;;; ============================================================================

(defstruct (state-space (:conc-name ss-))
  "The explored state space of a model."
  (initial-states nil :type list)
  (all-states (make-hash-table :test #'equal))
  (transitions (make-hash-table :test #'equal))
  (state-count 0 :type integer)
  (transition-count 0 :type integer)
  (max-depth 0 :type integer)
  (deadlocks nil :type list)
  (spec nil))

(defun add-state (space state)
  "Add STATE to the state space if not already present.
   Returns (values state new-p)."
  (let ((key (mst-values state)))
    (if (gethash key (ss-all-states space))
        (values (gethash key (ss-all-states space)) nil)
        (progn
          (setf (gethash key (ss-all-states space)) state)
          (incf (ss-state-count space))
          (setf (ss-max-depth space)
                (max (ss-max-depth space) (mst-depth state)))
          (values state t)))))

(defun add-transition (space from-state action to-state)
  "Add a transition to the state space."
  (let ((from-key (mst-values from-state)))
    (push (cons action to-state)
          (gethash from-key (ss-transitions space)))
    (incf (ss-transition-count space))))

(defun get-successors (space state)
  "Get all successor states of STATE."
  (mapcar #'cdr (gethash (mst-values state) (ss-transitions space))))

;;; ============================================================================
;;; State Space Exploration
;;; ============================================================================

(defstruct (exploration-config (:conc-name ec-))
  "Configuration for state space exploration."
  (mode :bfs :type (member :bfs :dfs :random))
  (max-states nil :type (or null integer))
  (max-depth nil :type (or null integer))
  (symmetry nil :type list)
  (invariants nil :type list)
  (properties nil :type list)
  (on-state nil :type (or null function))
  (on-violation nil :type (or null function)))

(defun explore-states (spec &key (mode :bfs) max-states max-depth
                                 invariants properties on-state on-violation)
  "Explore the state space of SPEC.
   Returns a STATE-SPACE structure with all explored states."
  (let ((space (make-state-space :spec spec))
        (config (make-exploration-config
                 :mode mode
                 :max-states max-states
                 :max-depth max-depth
                 :invariants invariants
                 :properties properties
                 :on-state on-state
                 :on-violation on-violation))
        (frontier nil)
        (visited (make-hash-table :test #'equal)))
    ;; Initialize with initial states
    (let ((init-states (compute-initial-states spec)))
      (setf (ss-initial-states space) init-states)
      (dolist (s init-states)
        (add-state space s)
        (push s frontier)))
    ;; Explore
    (loop while frontier
          for state = (pop-frontier frontier mode)
          for depth = (mst-depth state)
          do
             ;; Check limits
             (when (and max-states (>= (ss-state-count space) max-states))
               (return))
             (when (and max-depth (> depth max-depth))
               (loop-finish))
             ;; Check invariants
             (when invariants
               (dolist (inv invariants)
                 (unless (check-invariant-at inv state)
                   (when on-violation
                     (funcall on-violation :invariant inv state))
                   (return-from explore-states
                     (values space (make-counterexample state inv))))))
             ;; User callback
             (when on-state
               (funcall on-state state))
             ;; Expand successors
             (let ((successors (compute-successors spec state)))
               (if (null successors)
                   ;; Deadlock state
                   (push state (ss-deadlocks space))
                   ;; Process successors
                   (dolist (succ-pair successors)
                     (destructuring-bind (action . succ) succ-pair
                       (multiple-value-bind (canonical new-p)
                           (add-state space succ)
                         (add-transition space state action canonical)
                         (when new-p
                           (push canonical frontier))))))))
    space))

(defun pop-frontier (frontier mode)
  "Pop next state from frontier based on exploration mode."
  (ecase mode
    (:bfs (let ((s (car (last frontier))))
            (setf frontier (butlast frontier))
            s))
    (:dfs (pop frontier))
    (:random (let* ((idx (random (length frontier)))
                    (s (nth idx frontier)))
               (setf frontier (remove s frontier))
               s))))

(defun compute-initial-states (spec)
  "Compute the initial states from SPEC."
  (cond
    ((state-machine-p spec)
     (let ((init-formula (sm-init spec))
           (vars (sm-variables spec)))
       ;; For simple specs, enumerate initial states
       ;; This is a simplified implementation
       (list (make-model-state
              :id 0
              :values (mapcar (lambda (v)
                                (cons (smv-name v) (smv-initial v)))
                              vars)
              :depth 0))))
    ((tla-module-p spec)
     ;; Extract Init definition and evaluate
     (list (make-model-state :id 0 :values nil :depth 0)))
    (t
     (error "Unknown spec type: ~A" (type-of spec)))))

(defun compute-successors (spec state)
  "Compute successor states of STATE according to SPEC.
   Returns list of (action . state) pairs."
  (cond
    ((state-machine-p spec)
     (let ((result nil))
       (dolist (action (sm-actions spec))
         (when (action-enabled-p action state)
           (let ((succ (apply-action action state)))
             (push (cons (sma-name action) succ) result))))
       result))
    ((tla-module-p spec)
     ;; Extract Next definition and evaluate
     nil)
    (t nil)))

(defun action-enabled-p (action state)
  "Check if ACTION is enabled in STATE."
  (let ((guard (sma-guard action)))
    (or (null guard)
        (eval-formula guard state))))

(defun apply-action (action state)
  "Apply ACTION to STATE, returning the successor state."
  (let ((effect (sma-effect action))
        (new-values (copy-alist (mst-values state))))
    ;; Apply effect to produce new state
    ;; This is a simplified implementation
    (make-model-state
     :id (1+ (mst-id state))
     :values new-values
     :hash (state-hash new-values)
     :depth (1+ (mst-depth state))
     :predecessor state
     :action (sma-name action))))

(defun eval-formula (formula state)
  "Evaluate a state formula in STATE.
   Returns T if formula holds, NIL otherwise."
  (cond
    ((null formula) t)
    ((eq formula t) t)
    ((eq formula :true) t)
    ((eq formula :false) nil)
    ((symbolp formula)
     (state-get state formula))
    ((atom formula) formula)
    ((listp formula)
     (let ((op (car formula))
           (args (cdr formula)))
       (case op
         (:and (every (lambda (f) (eval-formula f state)) args))
         (:or (some (lambda (f) (eval-formula f state)) args))
         (:not (not (eval-formula (first args) state)))
         (:implies (or (not (eval-formula (first args) state))
                       (eval-formula (second args) state)))
         (:= (equal (eval-formula (first args) state)
                    (eval-formula (second args) state)))
         (:/= (not (equal (eval-formula (first args) state)
                          (eval-formula (second args) state))))
         (:< (< (eval-formula (first args) state)
                (eval-formula (second args) state)))
         (:> (> (eval-formula (first args) state)
                (eval-formula (second args) state)))
         (:<= (<= (eval-formula (first args) state)
                  (eval-formula (second args) state)))
         (:>= (>= (eval-formula (first args) state)
                  (eval-formula (second args) state)))
         (:in (member (eval-formula (first args) state)
                      (eval-formula (second args) state)
                      :test #'equal))
         (t (error "Unknown formula operator: ~A" op)))))
    (t (error "Cannot evaluate formula: ~A" formula))))

;;; ============================================================================
;;; Invariant and Property Checking
;;; ============================================================================

(defun check-invariant (invariant space)
  "Check that INVARIANT holds in all states of SPACE.
   Returns (values success-p counterexample)."
  (maphash (lambda (key state)
             (declare (ignore key))
             (unless (check-invariant-at invariant state)
               (return-from check-invariant
                 (values nil (make-counterexample state invariant)))))
           (ss-all-states space))
  (values t nil))

(defun check-invariant-at (invariant state)
  "Check that INVARIANT holds at STATE."
  (eval-formula invariant state))

(defun check-property (property space)
  "Check a temporal property over the state space.
   Returns (values success-p counterexample)."
  (let ((kind (if (tla-ast-node-p property)
                  (tla-ast-kind property)
                  (car property))))
    (case kind
      (:box ;; Always property - check invariant
       (let ((inner (if (tla-ast-node-p property)
                        (tla-ast-value property)
                        (second property))))
         (check-invariant inner space)))
      (:diamond ;; Eventually property - check reachability
       (let ((inner (if (tla-ast-node-p property)
                        (tla-ast-value property)
                        (second property))))
         (find-reachable-satisfying inner space)))
      (t
       (warn "Unsupported property type: ~A" kind)
       (values nil nil)))))

(defun find-reachable-satisfying (formula space)
  "Find a state satisfying FORMULA, or return failure.
   Returns (values success-p witness-or-counterexample)."
  (maphash (lambda (key state)
             (declare (ignore key))
             (when (eval-formula formula state)
               (return-from find-reachable-satisfying
                 (values t (make-witness-trace state)))))
           (ss-all-states space))
  (values nil nil))

;;; ============================================================================
;;; Counterexample and Witness Generation
;;; ============================================================================

(defstruct (counterexample (:conc-name cex-)
                           (:constructor %make-counterexample))
  "A counterexample trace."
  (property nil)
  (trace nil :type list)
  (loop-start nil)
  (type :safety))

(defun make-counterexample (state property)
  "Build a counterexample trace leading to STATE."
  (%make-counterexample
   :property property
   :trace (build-trace state)
   :type :safety))

(defun make-witness-trace (state)
  "Build a witness trace leading to STATE."
  (build-trace state))

(defun build-trace (state)
  "Build the trace from initial state to STATE."
  (let ((trace nil))
    (loop for s = state then (mst-predecessor s)
          while s
          do (push (cons (mst-action s) (mst-values s)) trace))
    trace))

(defun find-counterexample (property space)
  "Find a counterexample for PROPERTY in SPACE.
   Returns NIL if property holds."
  (multiple-value-bind (success-p cex) (check-property property space)
    (unless success-p cex)))

;;; ============================================================================
;;; State Graph Analysis
;;; ============================================================================

(defun state-graph (space)
  "Return the state graph as an adjacency list.
   Each entry is (state . successors)."
  (let ((graph nil))
    (maphash (lambda (key state)
               (declare (ignore key))
               (push (cons state (get-successors space state)) graph))
             (ss-all-states space))
    graph))

(defun reachable-states (space &optional from-state)
  "Return all states reachable from FROM-STATE (or initial states if nil)."
  (let ((visited (make-hash-table :test #'equal))
        (result nil))
    (labels ((visit (state)
               (unless (gethash (mst-values state) visited)
                 (setf (gethash (mst-values state) visited) t)
                 (push state result)
                 (dolist (succ (get-successors space state))
                   (visit succ)))))
      (if from-state
          (visit from-state)
          (dolist (init (ss-initial-states space))
            (visit init))))
    result))
