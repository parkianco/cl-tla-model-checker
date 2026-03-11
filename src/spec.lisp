;;;; spec.lisp
;;;; TLA+ specification representation (AST, modules, definitions)

(in-package #:cl-tla-model-checker)

;;; ============================================================================
;;; TLA+ AST Node Structure
;;; ============================================================================

(defstruct (tla-ast-node (:conc-name tla-ast-))
  "Base structure for TLA+ Abstract Syntax Tree nodes."
  (kind nil :type keyword)
  (value nil)
  (children nil :type list)
  (metadata nil :type list)
  (source-loc nil))

(defun make-ast (kind value &optional children metadata)
  "Convenience constructor for AST nodes."
  (make-tla-ast-node
   :kind kind
   :value value
   :children children
   :metadata metadata))

(defun ast-kind-p (node kind)
  "Check if NODE is an AST node of the given KIND."
  (and (tla-ast-node-p node)
       (eq (tla-ast-kind node) kind)))

(defun ast-walk (node fn)
  "Walk the AST, applying FN to each node. FN receives node and depth."
  (labels ((walk-impl (n depth)
             (funcall fn n depth)
             (dolist (child (tla-ast-children n))
               (when (tla-ast-node-p child)
                 (walk-impl child (1+ depth))))))
    (when (tla-ast-node-p node)
      (walk-impl node 0))))

(defun ast-transform (node fn)
  "Transform AST by applying FN to each node. FN returns replacement node."
  (if (tla-ast-node-p node)
      (let ((transformed (funcall fn node)))
        (if (tla-ast-node-p transformed)
            (make-tla-ast-node
             :kind (tla-ast-kind transformed)
             :value (tla-ast-value transformed)
             :children (mapcar (lambda (c) (ast-transform c fn))
                               (tla-ast-children transformed))
             :metadata (tla-ast-metadata transformed)
             :source-loc (tla-ast-source-loc transformed))
            transformed))
      node))

;;; ============================================================================
;;; TLA+ Module Structure
;;; ============================================================================

(defstruct (tla-module (:conc-name tla-mod-))
  "Represents a complete TLA+ module."
  (name nil :type (or symbol string))
  (extends nil :type list)
  (constants nil :type list)
  (variables nil :type list)
  (definitions nil :type list)
  (theorems nil :type list)
  (assumptions nil :type list)
  (properties nil :type list)
  (metadata nil :type list))

;; Aliases for API consistency
(defun tla-module-name (m) (tla-mod-name m))
(defun tla-module-extends (m) (tla-mod-extends m))
(defun tla-module-constants (m) (tla-mod-constants m))
(defun tla-module-variables (m) (tla-mod-variables m))
(defun tla-module-definitions (m) (tla-mod-definitions m))
(defun tla-module-theorems (m) (tla-mod-theorems m))

(defstruct (tla-definition (:conc-name tla-def-))
  "A TLA+ operator or definition."
  (name nil :type (or symbol string))
  (params nil :type list)
  (body nil)
  (local-p nil :type boolean)
  (recursive-p nil :type boolean)
  (doc nil :type (or null string)))

;; Aliases for API consistency
(defun tla-definition-name (d) (tla-def-name d))
(defun tla-definition-params (d) (tla-def-params d))
(defun tla-definition-body (d) (tla-def-body d))
(defun tla-definition-local-p (d) (tla-def-local-p d))

(defstruct (tla-theorem (:conc-name tla-thm-))
  "A TLA+ theorem declaration."
  (name nil :type (or symbol string))
  (formula nil)
  (proof nil)
  (assumes nil :type list))

;; Aliases for API consistency
(defun tla-theorem-name (th) (tla-thm-name th))
(defun tla-theorem-formula (th) (tla-thm-formula th))

(defstruct (tla-constant (:conc-name tla-const-))
  "A TLA+ CONSTANT declaration."
  (name nil :type (or symbol string))
  (type nil)
  (model-value nil)
  (symmetric-p nil :type boolean))

;; Aliases for API consistency
(defun tla-constant-name (c) (tla-const-name c))

(defstruct (tla-var (:conc-name tlav-))
  "A TLA+ VARIABLE declaration."
  (name nil :type (or symbol string))
  (type nil)
  (description nil :type (or null string)))

;; Aliases for API consistency
(defun tla-var-name (v) (tlav-name v))

;;; ============================================================================
;;; State Machine Representation
;;; ============================================================================

(defstruct (state-machine (:conc-name sm-))
  "A state machine specification."
  (name nil :type (or symbol string))
  (variables nil :type list)
  (init nil)
  (next nil)
  (actions nil :type list)
  (fairness nil :type list)
  (spec nil)
  (invariants nil :type list)
  (properties nil :type list)
  (metadata nil :type list))

;; Aliases for API consistency
(defun state-machine-name (sm) (sm-name sm))
(defun state-machine-variables (sm) (sm-variables sm))
(defun state-machine-init (sm) (sm-init sm))
(defun state-machine-next (sm) (sm-next sm))
(defun state-machine-actions (sm) (sm-actions sm))
(defun state-machine-fairness (sm) (sm-fairness sm))
(defun state-machine-spec (sm) (sm-spec sm))

(defstruct (sm-variable (:conc-name smv-))
  "A state machine variable."
  (name nil :type (or symbol string))
  (type nil)
  (initial nil)
  (constraints nil :type list))

;; Aliases for API consistency
(defun sm-variable-name (v) (smv-name v))
(defun sm-variable-type (v) (smv-type v))
(defun sm-variable-initial (v) (smv-initial v))

(defstruct (sm-action (:conc-name sma-))
  "A state machine action (transition)."
  (name nil :type (or symbol string))
  (params nil :type list)
  (guard nil)
  (effect nil)
  (fairness nil :type (member nil :weak :strong))
  (modifies nil :type list)
  (description nil :type (or null string)))

;; Aliases for API consistency
(defun sm-action-name (a) (sma-name a))
(defun sm-action-params (a) (sma-params a))
(defun sm-action-guard (a) (sma-guard a))
(defun sm-action-effect (a) (sma-effect a))
(defun sm-action-fairness (a) (sma-fairness a))

(defun create-state-machine (name variables init-expr next-expr
                             &key actions fairness invariants properties)
  "Create a state machine with the given components."
  (make-state-machine
   :name name
   :variables variables
   :init init-expr
   :next next-expr
   :actions actions
   :fairness fairness
   :invariants invariants
   :properties properties
   :spec (generate-spec-formula name variables next-expr fairness)))

(defun generate-spec-formula (name vars next-expr fairness)
  "Generate the Spec == Init /\\ [][Next]_vars /\\ Fairness formula."
  (declare (ignore next-expr))
  (let* ((init-name (format nil "Init~A" name))
         (next-name (format nil "Next~A" name))
         (vars-tuple (if (= (length vars) 1)
                         (smv-name (first vars))
                         `(:tuple ,@(mapcar #'smv-name vars))))
         (base-spec `(:and (:ref ,init-name)
                           (:box (:subscript ,next-name ,vars-tuple)))))
    (if fairness
        `(:and ,base-spec ,@(mapcar #'fairness-to-formula fairness))
        base-spec)))

(defun fairness-to-formula (fairness-constraint)
  "Generate a fairness formula from a constraint."
  (let ((kind (fc-kind fairness-constraint))
        (action (fc-action fairness-constraint))
        (vars (fc-vars fairness-constraint)))
    (ecase kind
      (:weak `(:wf ,vars ,action))
      (:strong `(:sf ,vars ,action)))))

;;; ============================================================================
;;; Specification DSL Macros
;;; ============================================================================

(defvar *spec-context* nil
  "Current specification context for DSL macros.")

(defmacro with-spec-context ((name) &body body)
  "Execute BODY with a specification context bound."
  `(let ((*spec-context* (make-tla-module :name ',name)))
     ,@body
     *spec-context*))

(defmacro defspec (name (&rest options) &body clauses)
  "Define a TLA+ specification using a DSL."
  (declare (ignore options))
  `(let ((module (make-tla-module :name ',name)))
     ,@(mapcar (lambda (clause)
                 (expand-spec-clause 'module clause))
               clauses)
     module))

(defun expand-spec-clause (module-var clause)
  "Expand a specification clause into code that modifies MODULE-VAR."
  (let ((kind (car clause))
        (args (cdr clause)))
    (case kind
      (:extends
       `(setf (tla-mod-extends ,module-var)
              (append (tla-mod-extends ,module-var) ',args)))
      (:constant
       `(push (make-tla-constant :name ',(first args)
                                 :type ',(second args))
              (tla-mod-constants ,module-var)))
      (:variable
       `(push (make-tla-var :name ',(first args)
                            :type ',(second args))
              (tla-mod-variables ,module-var)))
      (:define
       `(push (make-tla-definition :name ',(first args)
                                   :params ',(second args)
                                   :body ',(third args))
              (tla-mod-definitions ,module-var)))
      (:theorem
       `(push (make-tla-theorem :name ',(first args)
                                :formula ',(second args))
              (tla-mod-theorems ,module-var)))
      (t `(warn "Unknown specification clause: ~A" ',kind)))))
