;;;; test-tla.lisp
;;;; Tests for TLA+ Model Checker

(in-package #:cl-tla-model-checker.test)

;;; ============================================================================
;;; Test Infrastructure
;;; ============================================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  "Define a test case."
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "  PASS: ~A~%" ',name))
       (error (e)
         (incf *fail-count*)
         (format t "  FAIL: ~A - ~A~%" ',name e)))))

(defmacro assert-true (form &optional message)
  "Assert that FORM evaluates to true."
  `(unless ,form
     (error "Assertion failed~@[: ~A~]" ,message)))

(defmacro assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL."
  `(unless (equal ,expected ,actual)
     (error "Expected ~S but got ~S~@[: ~A~]" ,expected ,actual ,message)))

(defmacro assert-not-nil (form &optional message)
  "Assert that FORM is not nil."
  `(unless ,form
     (error "Expected non-nil value~@[: ~A~]" ,message)))

;;; ============================================================================
;;; AST Tests
;;; ============================================================================

(deftest test-make-ast
  (let ((node (tla:make-ast :box 'P)))
    (assert-true (tla:tla-ast-node-p node))
    (assert-equal :box (tla:tla-ast-kind node))
    (assert-equal 'P (tla:tla-ast-value node))))

(deftest test-ast-kind-p
  (let ((node (tla:make-ast :diamond 'Q)))
    (assert-true (tla:ast-kind-p node :diamond))
    (assert-true (not (tla:ast-kind-p node :box)))))

(deftest test-ast-walk
  (let* ((child1 (tla:make-ast :var 'x))
         (child2 (tla:make-ast :var 'y))
         (root (tla:make-ast :and nil (list child1 child2)))
         (visited nil))
    (tla:ast-walk root (lambda (n d)
                         (push (cons (tla:tla-ast-kind n) d) visited)))
    (assert-equal 3 (length visited))))

;;; ============================================================================
;;; Module Tests
;;; ============================================================================

(deftest test-make-module
  (let ((mod (tla:make-tla-module :name "TestModule")))
    (assert-true (tla:tla-module-p mod))
    (assert-equal "TestModule" (tla:tla-module-name mod))))

(deftest test-make-definition
  (let ((def (tla:make-tla-definition
              :name 'Init
              :body '(:= x 0))))
    (assert-true (tla:tla-definition-p def))
    (assert-equal 'Init (tla:tla-definition-name def))))

;;; ============================================================================
;;; State Machine Tests
;;; ============================================================================

(deftest test-make-state-machine
  (let* ((var (tla:make-sm-variable :name 'x :type 'nat :initial 0))
         (sm (tla:make-state-machine
              :name "Counter"
              :variables (list var)
              :init '(:= x 0)
              :next '(:prime x (:+ x 1)))))
    (assert-true (tla:state-machine-p sm))
    (assert-equal "Counter" (tla:state-machine-name sm))))

(deftest test-sm-variable
  (let ((var (tla:make-sm-variable :name 'count :type 'integer :initial 0)))
    (assert-true (tla:sm-variable-p var))
    (assert-equal 'count (tla:sm-variable-name var))
    (assert-equal 0 (tla:sm-variable-initial var))))

(deftest test-sm-action
  (let ((action (tla:make-sm-action
                 :name 'Increment
                 :guard '(:< x 10)
                 :effect '(:prime x (:+ x 1)))))
    (assert-true (tla:sm-action-p action))
    (assert-equal 'Increment (tla:sm-action-name action))))

;;; ============================================================================
;;; Temporal Operator Tests
;;; ============================================================================

(deftest test-temporal-always
  (let ((formula (tla:temporal-always '(safe))))
    (assert-true (tla:tla-ast-node-p formula))
    (assert-equal :box (tla:tla-ast-kind formula))))

(deftest test-temporal-eventually
  (let ((formula (tla:temporal-eventually '(done))))
    (assert-equal :diamond (tla:tla-ast-kind formula))))

(deftest test-temporal-until
  (let ((formula (tla:temporal-until 'P 'Q)))
    (assert-equal :until (tla:tla-ast-kind formula))
    (assert-equal 2 (length (tla:tla-ast-children formula)))))

(deftest test-temporal-leads-to
  (let ((formula (tla:temporal-leads-to 'request 'response)))
    (assert-equal :leads-to (tla:tla-ast-kind formula))))

(deftest test-always-implies
  (let ((formula (tla:always-implies 'P 'Q)))
    (assert-true (tla:tla-ast-node-p formula))
    (assert-equal :box (tla:tla-ast-kind formula))))

(deftest test-always-eventually
  (let ((formula (tla:always-eventually 'progress)))
    (assert-equal :box (tla:tla-ast-kind formula))
    (assert-true (tla:tla-ast-node-p (tla:tla-ast-value formula)))))

;;; ============================================================================
;;; Fairness Tests
;;; ============================================================================

(deftest test-weak-fairness
  (let ((fc (tla:weak-fair 'action 'vars)))
    (assert-true (tla:fairness-constraint-p fc))
    (assert-equal :weak (tla:fairness-constraint-kind fc))))

(deftest test-strong-fairness
  (let ((fc (tla:strong-fair 'action 'vars)))
    (assert-equal :strong (tla:fairness-constraint-kind fc))))

(deftest test-fairness-formula
  (let* ((fc (tla:weak-fair 'Send '(network)))
         (formula (tla:fairness-formula fc)))
    (assert-equal :wf (car formula))))

;;; ============================================================================
;;; Refinement Tests
;;; ============================================================================

(deftest test-make-refinement
  (let* ((source (tla:make-state-machine :name "Concrete"))
         (target (tla:make-state-machine :name "Abstract"))
         (mapping (tla:make-refinement source target '((x . y)))))
    (assert-true (tla:refinement-mapping-p mapping))
    (assert-equal source (tla:refinement-mapping-source mapping))))

;;; ============================================================================
;;; State Space Tests
;;; ============================================================================

(deftest test-state-space-creation
  (let ((space (tla:make-state-space)))
    (assert-true (tla:state-space-p space))
    (assert-equal 0 (tla::ss-state-count space))))

;;; ============================================================================
;;; Code Generation Tests
;;; ============================================================================

(deftest test-emit-formula-logic
  (assert-equal "(TRUE /\\ TRUE)" (tla:emit-formula '(:and :true :true) 0))
  (assert-equal "(TRUE \\/ FALSE)" (tla:emit-formula '(:or :true :false) 0))
  (assert-equal "(P) => (Q)" (tla:emit-formula '(:implies P Q) 0)))

(deftest test-emit-formula-quantifiers
  (let ((formula (tla:emit-formula '(:forall n Nodes (:in n alive)) 0)))
    (assert-true (search "\\A" formula))
    (assert-true (search "\\in" formula))))

(deftest test-emit-formula-temporal
  (let ((always (tla:emit-formula '(:box safe) 0))
        (eventually (tla:emit-formula '(:diamond done) 0)))
    (assert-true (search "[]" always))
    (assert-true (search "<>" eventually))))

(deftest test-emit-formula-sets
  (assert-true (search "\\in" (tla:emit-formula '(:in x S) 0)))
  (assert-true (search "\\cup" (tla:emit-formula '(:union A B) 0)))
  (assert-true (search "\\cap" (tla:emit-formula '(:inter A B) 0))))

(deftest test-emit-module
  (let* ((mod (tla:make-tla-module
               :name "TestSpec"
               :extends '("Naturals")
               :variables (list (tla:make-tla-var :name 'x))
               :definitions (list (tla:make-tla-definition
                                   :name 'Init
                                   :body '(:= x 0)))))
         (output (tla:emit-module mod 0)))
    (assert-true (search "MODULE TestSpec" output))
    (assert-true (search "EXTENDS Naturals" output))
    (assert-true (search "VARIABLES" output))))

(deftest test-lisp-to-tla
  (assert-equal "TRUE" (tla:lisp-to-tla t))
  (assert-equal "FALSE" (tla:lisp-to-tla :false))
  (assert-equal "42" (tla:lisp-to-tla 42)))

;;; ============================================================================
;;; Model Config Tests
;;; ============================================================================

(deftest test-model-config
  (let ((config (tla:make-model-config
                 :invariants '(TypeInvariant SafetyInvariant)
                 :properties '(Liveness))))
    (assert-true (tla:model-config-p config))
    (assert-equal '(TypeInvariant SafetyInvariant)
                  (tla:model-config-invariants config))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(deftest test-simple-counter-spec
  "Test creating a complete simple counter specification."
  (let* ((x-var (tla:make-sm-variable :name 'x :type 'nat :initial 0))
         (inc-action (tla:make-sm-action
                      :name 'Increment
                      :guard '(:< x 10)
                      :effect '(:prime x (:+ x 1))))
         (sm (tla:create-state-machine
              "Counter"
              (list x-var)
              '(:= x 0)
              '(:or (Increment))
              :actions (list inc-action)
              :invariants '((Range . (:and (:<= 0 x) (:<= x 10)))))))
    (assert-true (tla:state-machine-p sm))
    (let ((tla-output (tla:generate-tla-spec sm)))
      (assert-true (stringp tla-output))
      (assert-true (search "VARIABLES" tla-output)))))

(deftest test-mutex-spec
  "Test creating a mutual exclusion specification."
  (let ((mod (tla:make-tla-module
              :name "Mutex"
              :extends '("Naturals" "FiniteSets")
              :constants (list (tla:make-tla-constant :name 'Procs))
              :variables (list (tla:make-tla-var :name 'pc)
                               (tla:make-tla-var :name 'waiting))
              :theorems (list (tla:make-tla-theorem
                               :name "MutualExclusion"
                               :formula (tla:temporal-always
                                         '(:forall p1 Procs
                                           (:forall p2 Procs
                                            (:implies (:and (:= pc[p1] "cs")
                                                            (:= pc[p2] "cs"))
                                                      (:= p1 p2))))))))))
    (let ((output (tla:generate-tla-spec mod)))
      (assert-true (search "MODULE Mutex" output))
      (assert-true (search "THEOREM MutualExclusion" output)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)
  (format t "~%Running TLA+ Model Checker Tests~%")
  (format t "================================~%~%")

  ;; AST tests
  (format t "AST Tests:~%")
  (test-make-ast)
  (test-ast-kind-p)
  (test-ast-walk)

  ;; Module tests
  (format t "~%Module Tests:~%")
  (test-make-module)
  (test-make-definition)

  ;; State machine tests
  (format t "~%State Machine Tests:~%")
  (test-make-state-machine)
  (test-sm-variable)
  (test-sm-action)

  ;; Temporal tests
  (format t "~%Temporal Operator Tests:~%")
  (test-temporal-always)
  (test-temporal-eventually)
  (test-temporal-until)
  (test-temporal-leads-to)
  (test-always-implies)
  (test-always-eventually)

  ;; Fairness tests
  (format t "~%Fairness Tests:~%")
  (test-weak-fairness)
  (test-strong-fairness)
  (test-fairness-formula)

  ;; Refinement tests
  (format t "~%Refinement Tests:~%")
  (test-make-refinement)

  ;; State space tests
  (format t "~%State Space Tests:~%")
  (test-state-space-creation)

  ;; Code generation tests
  (format t "~%Code Generation Tests:~%")
  (test-emit-formula-logic)
  (test-emit-formula-quantifiers)
  (test-emit-formula-temporal)
  (test-emit-formula-sets)
  (test-emit-module)
  (test-lisp-to-tla)

  ;; Model config tests
  (format t "~%Model Config Tests:~%")
  (test-model-config)

  ;; Integration tests
  (format t "~%Integration Tests:~%")
  (test-simple-counter-spec)
  (test-mutex-spec)

  ;; Summary
  (format t "~%================================~%")
  (format t "Results: ~A/~A passed" *pass-count* *test-count*)
  (when (> *fail-count* 0)
    (format t ", ~A failed" *fail-count*))
  (format t "~%")

  (values (zerop *fail-count*) *pass-count* *fail-count*))
