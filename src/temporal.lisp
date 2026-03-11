;;;; temporal.lisp
;;;; Temporal operators and fairness constraints

(in-package #:cl-tla-model-checker)

;;; ============================================================================
;;; Temporal Operators
;;; ============================================================================

(defun temporal-always (formula)
  "Create an Always/Globally temporal formula: []P"
  (make-ast :box formula nil '(:temporal :ltl)))

(defun temporal-eventually (formula)
  "Create an Eventually/Finally temporal formula: <>P"
  (make-ast :diamond formula nil '(:temporal :ltl)))

(defun temporal-until (p q)
  "Create an Until temporal formula: P U Q"
  (make-ast :until nil (list p q) '(:temporal :ltl)))

(defun temporal-leads-to (p q)
  "Create a Leads-To temporal formula: P ~> Q (equivalent to [](P => <>Q))"
  (make-ast :leads-to nil (list p q) '(:temporal :ltl)))

(defun temporal-weak-until (p q)
  "Create a Weak Until temporal formula: P W Q"
  (make-ast :weak-until nil (list p q) '(:temporal :ltl)))

(defun temporal-release (p q)
  "Create a Release temporal formula: P R Q (dual of Until)"
  (make-ast :release nil (list p q) '(:temporal :ltl)))

(defun temporal-next (formula)
  "Create a Next temporal formula: X P (or O P in TLA+)"
  (make-ast :next formula nil '(:temporal :ltl)))

(defun temporal-stuttering (action vars)
  "Create a stuttering step: action \\/ UNCHANGED vars"
  (make-ast :stuttering nil (list action vars) '(:temporal :action)))

(defun temporal-enabled (action)
  "Create an ENABLED operator: ENABLED action"
  (make-ast :enabled action nil '(:temporal :action)))

(defun temporal-primed (var)
  "Create a primed variable: var'"
  (make-ast :prime var nil '(:temporal :action)))

;;; ============================================================================
;;; Temporal Formula Builders
;;; ============================================================================

(defun always-implies (p q)
  "[](P => Q)"
  (temporal-always `(:implies ,p ,q)))

(defun always-eventually (p)
  "[]<>P (infinitely often)"
  (temporal-always (temporal-eventually p)))

(defun eventually-always (p)
  "<>[]P (persistence/stability)"
  (temporal-eventually (temporal-always p)))

(defun response (p q)
  "P ~> Q (whenever P, eventually Q)"
  (temporal-leads-to p q))

(defun precedence (p q)
  "Q must hold before P (P => once Q held)"
  `(:not (:until (:not ,q) ,p)))

(defun absence (p)
  "P never holds"
  (temporal-always `(:not ,p)))

(defun existence (p)
  "P eventually holds"
  (temporal-eventually p))

(defun bounded-response (p q &key (bound 10))
  "P leads to Q within BOUND steps"
  (let ((bounded-formula
          (loop for i from 0 below bound
                collect (temporal-next q))))
    `(:implies ,p (:or ,@bounded-formula))))

;;; ============================================================================
;;; Fairness Constraints
;;; ============================================================================

(defstruct (fairness-constraint (:conc-name fc-))
  "A fairness constraint for liveness properties."
  (kind :weak :type (member :weak :strong :unconditional))
  (action nil)
  (vars nil)
  (condition nil)
  (description nil :type (or null string)))

;; Aliases for API consistency
(defun fairness-constraint-kind (fc) (fc-kind fc))
(defun fairness-constraint-action (fc) (fc-action fc))
(defun fairness-constraint-vars (fc) (fc-vars fc))

(defun weak-fair (action vars &key condition)
  "Create a weak fairness constraint: WF_vars(action).
   If action is continuously enabled, it must eventually occur."
  (make-fairness-constraint
   :kind :weak
   :action action
   :vars vars
   :condition condition))

(defun strong-fair (action vars &key condition)
  "Create a strong fairness constraint: SF_vars(action).
   If action is repeatedly enabled, it must eventually occur."
  (make-fairness-constraint
   :kind :strong
   :action action
   :vars vars
   :condition condition))

(defun compose-fairness (&rest constraints)
  "Compose multiple fairness constraints into a conjunction."
  (make-ast :and nil (mapcar #'fairness-formula constraints)))

(defun fairness-formula (constraint)
  "Generate the TLA+ formula for a fairness constraint."
  (let ((kind (fc-kind constraint))
        (action (fc-action constraint))
        (vars (fc-vars constraint)))
    (ecase kind
      (:weak `(:wf ,vars ,action))
      (:strong `(:sf ,vars ,action))
      (:unconditional `(:enabled ,action)))))

;;; ============================================================================
;;; Refinement Mapping
;;; ============================================================================

(defstruct (refinement-mapping (:conc-name rm-))
  "A refinement mapping from a concrete spec to an abstract spec."
  (source nil :type (or null state-machine tla-module))
  (target nil :type (or null state-machine tla-module))
  (state-map nil :type list)    ; ((abstract-var . concrete-expr) ...)
  (action-map nil :type list)   ; ((abstract-action . concrete-action) ...)
  (invariant nil)               ; Coupling invariant
  (auxiliary nil :type list)    ; Auxiliary variables for prophecy/history
  (verified-p nil :type boolean))

;; Aliases for API consistency
(defun refinement-mapping-source (rm) (rm-source rm))
(defun refinement-mapping-target (rm) (rm-target rm))
(defun refinement-mapping-state-map (rm) (rm-state-map rm))
(defun refinement-mapping-action-map (rm) (rm-action-map rm))

(defun make-refinement (source target state-mappings
                        &key action-mappings invariant auxiliary)
  "Create a refinement mapping from SOURCE to TARGET."
  (make-refinement-mapping
   :source source
   :target target
   :state-map state-mappings
   :action-map (or action-mappings (infer-action-mappings source target))
   :invariant invariant
   :auxiliary auxiliary))

(defun infer-action-mappings (source target)
  "Attempt to infer action mappings from source to target."
  (let ((source-actions (if (state-machine-p source)
                            (sm-actions source)
                            nil))
        (target-actions (if (state-machine-p target)
                            (sm-actions target)
                            nil)))
    ;; Simple name-based matching
    (loop for sa in source-actions
          for match = (find (sma-name sa) target-actions
                            :key #'sma-name :test #'equal)
          when match collect (cons (sma-name match) (sma-name sa)))))

(defun check-refinement (mapping)
  "Check that the refinement mapping is valid.
   Returns (values success-p errors)."
  (let ((errors nil))
    ;; Check state mapping completeness
    (when (state-machine-p (rm-target mapping))
      (let ((target-vars (mapcar #'smv-name (sm-variables (rm-target mapping))))
            (mapped-vars (mapcar #'car (rm-state-map mapping))))
        (dolist (tv target-vars)
          (unless (member tv mapped-vars :test #'equal)
            (push (format nil "Target variable ~A not mapped" tv) errors)))))
    ;; Check simulation relation (placeholder)
    (unless (rm-invariant mapping)
      (push "No coupling invariant specified" errors))
    ;; Check action refinement
    (when (and (rm-action-map mapping) (null (rm-action-map mapping)))
      (push "No action mappings defined" errors))
    (values (null errors) (nreverse errors))))

(defun stuttering-simulation (source target mapping)
  "Generate a stuttering simulation proof obligation."
  (let ((source-init (sm-init source))
        (source-next (sm-next source))
        (target-init (sm-init target))
        (target-next (sm-next target))
        (inv (rm-invariant mapping))
        (state-map (rm-state-map mapping)))
    `(:and
      ;; Init refinement: Init_C => (bar(Init_A) after substitution)
      (:theorem "InitRefines"
                (:implies ,source-init
                          ,(apply-state-map target-init state-map)))
      ;; Step refinement: [Next_C]_vc => [bar(Next_A)]_bar(va)
      (:theorem "StepRefines"
                (:implies (:and ,inv ,source-next)
                          (:or ,(apply-state-map target-next state-map)
                               (:unchanged ,(mapcar #'car state-map))))))))

(defun apply-state-map (formula state-map)
  "Apply state variable substitutions to a formula."
  (cond
    ((null formula) nil)
    ((symbolp formula)
     (or (cdr (assoc formula state-map :test #'eq))
         formula))
    ((atom formula) formula)
    (t (mapcar (lambda (f) (apply-state-map f state-map)) formula))))

(defun trace-containment (source target)
  "Check trace containment: traces(source) \\subseteq traces(target)."
  (make-ast :trace-containment nil (list source target)))
