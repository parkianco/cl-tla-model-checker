;;;; checker.lisp
;;;; Model checking and TLA+ code generation

(in-package #:cl-tla-model-checker)

;;; ============================================================================
;;; Model Checking Configuration
;;; ============================================================================

(defstruct (model-config (:conc-name mc-))
  "Configuration for TLC model checking."
  (spec nil :type (or null tla-module state-machine))
  (invariants nil :type list)
  (properties nil :type list)
  (constants nil :type list)     ; ((name . value) ...)
  (symmetry nil :type list)      ; Symmetric model values
  (constraint nil)               ; State constraint
  (action-constraint nil)        ; Action constraint
  (workers 4 :type integer)
  (memory 4096 :type integer)    ; MB
  (depth nil :type (or null integer))
  (seed nil :type (or null integer))
  (mode :bfs :type (member :bfs :dfs :random :simulation)))

;; Aliases for API consistency
(defun model-config-spec (mc) (mc-spec mc))
(defun model-config-invariants (mc) (mc-invariants mc))
(defun model-config-properties (mc) (mc-properties mc))
(defun model-config-constants (mc) (mc-constants mc))
(defun model-config-symmetry (mc) (mc-symmetry mc))
(defun model-config-constraint (mc) (mc-constraint mc))

(defun generate-cfg (config output-file)
  "Generate a TLC configuration file."
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    ;; Specification
    (format out "SPECIFICATION Spec~%~%")
    ;; Constants
    (when (mc-constants config)
      (format out "CONSTANTS~%")
      (dolist (const (mc-constants config))
        (format out "  ~A = ~A~%" (car const) (format-tla-value (cdr const))))
      (format out "~%"))
    ;; Invariants
    (when (mc-invariants config)
      (dolist (inv (mc-invariants config))
        (format out "INVARIANT ~A~%" (if (symbolp inv) inv (first inv)))))
    ;; Properties
    (when (mc-properties config)
      (dolist (prop (mc-properties config))
        (format out "PROPERTY ~A~%" (if (symbolp prop) prop (first prop)))))
    ;; Symmetry
    (when (mc-symmetry config)
      (format out "~%SYMMETRY ~{~A~^, ~}~%" (mc-symmetry config)))
    ;; Constraint
    (when (mc-constraint config)
      (format out "~%CONSTRAINT ~A~%" (mc-constraint config)))
    output-file))

;;; ============================================================================
;;; TLC Execution and Result Parsing
;;; ============================================================================

(defstruct (tlc-result (:conc-name tlcr-))
  "Result of TLC model checking."
  (success-p nil :type boolean)
  (states-found 0 :type integer)
  (distinct-states 0 :type integer)
  (depth 0 :type integer)
  (time 0.0 :type number)
  (errors nil :type list)
  (warnings nil :type list)
  (counterexample nil)
  (coverage nil))

;; Aliases for API consistency
(defun tlc-result-success-p (r) (tlcr-success-p r))
(defun tlc-result-states-found (r) (tlcr-states-found r))
(defun tlc-result-counterexample (r) (tlcr-counterexample r))

(defun run-model-check (config &key (output-dir *tla-output-dir*))
  "Run TLC on the given model configuration.
   Returns a TLC-RESULT structure."
  (ensure-directories-exist output-dir)
  (let* ((spec-name (if (state-machine-p (mc-spec config))
                        (sm-name (mc-spec config))
                        (tla-mod-name (mc-spec config))))
         (spec-file (merge-pathnames (format nil "~A.tla" spec-name) output-dir))
         (cfg-file (merge-pathnames (format nil "~A.cfg" spec-name) output-dir)))
    ;; Generate files
    (emit-to-file (mc-spec config) spec-file)
    (generate-cfg config cfg-file)
    ;; Run TLC (mock for now - would shell out to java -jar tla2tools.jar)
    (if *tlc-path*
        (run-tlc-process spec-file cfg-file config)
        (make-tlc-result
         :success-p nil
         :errors (list "TLC not configured. Set *tlc-path*.")))))

(defun run-tlc-process (spec-file cfg-file config)
  "Actually run the TLC process. Returns TLC-RESULT."
  ;; Placeholder - would use sb-ext:run-program in actual implementation
  (declare (ignore cfg-file config))
  (make-tlc-result
   :success-p t
   :states-found 0
   :distinct-states 0
   :depth 0
   :time 0.0
   :errors (list (format nil "Mock TLC run on ~A" spec-file))))

(defun parse-tlc-output (output-string)
  "Parse TLC output into a TLC-RESULT."
  (let ((result (make-tlc-result)))
    ;; Parse state counts
    (let ((states-match (search "states found" output-string)))
      (when states-match
        (setf (tlcr-states-found result)
              (parse-integer-from output-string states-match))))
    ;; Parse errors
    (when (search "Error:" output-string)
      (setf (tlcr-success-p result) nil)
      (push (extract-error output-string) (tlcr-errors result)))
    ;; Parse counterexample if present
    (when (search "Error trace" output-string)
      (setf (tlcr-counterexample result)
            (parse-counterexample-trace output-string)))
    result))

(defun parse-counterexample-trace (output-string)
  "Parse a counterexample trace from TLC output."
  (declare (ignore output-string))
  ;; Placeholder - would parse the actual trace format
  nil)

(defun analyze-counterexample (result)
  "Analyze a counterexample to identify the root cause."
  (let ((trace (tlcr-counterexample result)))
    (when trace
      ;; Return analysis summary
      (list :trace-length (length trace)
            :loop-p (detect-loop trace)
            :violated-property (first (tlcr-errors result))))))

(defun detect-loop (trace)
  "Detect if a trace contains a loop (for liveness counterexamples)."
  (let ((states (mapcar #'car trace)))
    (loop for i from 0 below (length states)
          when (member (nth i states) (nthcdr (1+ i) states) :test #'equal)
            return t
          finally (return nil))))

;;; ============================================================================
;;; TLA+ Specification Generation
;;; ============================================================================

(defun generate-tla-spec (object &key (indent 0))
  "Generate TLA+ specification text from a Lisp object."
  (cond
    ((tla-module-p object) (emit-module object indent))
    ((state-machine-p object) (emit-state-machine object indent))
    ((tla-definition-p object) (emit-definition object indent))
    ((tla-ast-node-p object) (emit-ast-node object indent))
    ((listp object) (emit-formula object indent))
    (t (emit-value object))))

(defun emit-module (module indent)
  "Emit a TLA+ module."
  (with-output-to-string (out)
    (let ((ind (make-indent indent)))
      ;; Module header
      (format out "~A---- MODULE ~A ----~%"
              ind (tla-mod-name module))
      ;; EXTENDS
      (when (tla-mod-extends module)
        (format out "~AEXTENDS ~{~A~^, ~}~%~%"
                ind (tla-mod-extends module)))
      ;; CONSTANTS
      (when (tla-mod-constants module)
        (format out "~ACONSTANTS~%" ind)
        (dolist (const (tla-mod-constants module))
          (format out "~A  ~A~%" ind (tla-const-name const)))
        (format out "~%"))
      ;; VARIABLES
      (when (tla-mod-variables module)
        (format out "~AVARIABLES~%" ind)
        (dolist (var (tla-mod-variables module))
          (format out "~A  ~A~%" ind (tla-var-name var)))
        (format out "~%"))
      ;; Definitions
      (dolist (def (tla-mod-definitions module))
        (format out "~A~%~%" (emit-definition def indent)))
      ;; Theorems
      (dolist (thm (tla-mod-theorems module))
        (format out "~ATHEOREM ~A == ~A~%~%"
                ind (tla-thm-name thm) (emit-formula (tla-thm-formula thm) 0)))
      ;; Module footer
      (format out "~A====~%" ind))))

(defun emit-state-machine (sm indent)
  "Emit a state machine as TLA+ definitions."
  (with-output-to-string (out)
    (let ((ind (make-indent indent)))
      ;; Variables declaration
      (format out "~AVARIABLES ~{~A~^, ~}~%~%"
              ind (mapcar #'smv-name (sm-variables sm)))
      ;; Variable tuple
      (let ((vars (mapcar #'smv-name (sm-variables sm))))
        (format out "~Avars == <<~{~A~^, ~}>>~%~%"
                ind vars))
      ;; Init predicate
      (format out "~AInit ==~%" ind)
      (format out "~A  ~A~%~%" ind (emit-formula (sm-init sm) (+ indent 2)))
      ;; Individual actions
      (dolist (action (sm-actions sm))
        (format out "~A~A~%~%" ind (emit-action action indent)))
      ;; Next state relation
      (format out "~ANext ==~%" ind)
      (format out "~A  ~A~%~%" ind (emit-formula (sm-next sm) (+ indent 2)))
      ;; Fairness constraints
      (when (sm-fairness sm)
        (format out "~AFairness ==~%" ind)
        (format out "~A  ~A~%~%" ind
                (emit-formula (apply #'compose-fairness (sm-fairness sm)) (+ indent 2))))
      ;; Spec formula
      (format out "~ASpec == Init /\\ [][Next]_vars"
              ind)
      (when (sm-fairness sm)
        (format out " /\\ Fairness"))
      (format out "~%~%")
      ;; Invariants
      (dolist (inv (sm-invariants sm))
        (format out "~AInv~A == ~A~%"
                ind (car inv) (emit-formula (cdr inv) 0))))))

(defun emit-action (action indent)
  "Emit a state machine action as a TLA+ operator."
  (let ((ind (make-indent indent)))
    (declare (ignore ind))
    (format nil "~A(~{~A~^, ~}) ==~%  /\\ ~A~%  /\\ ~A"
            (sma-name action)
            (sma-params action)
            (emit-formula (sma-guard action) (+ indent 2))
            (emit-formula (sma-effect action) (+ indent 2)))))

(defun emit-definition (def indent)
  "Emit a TLA+ definition."
  (let ((ind (make-indent indent)))
    (format nil "~A~A~A == ~A"
            ind
            (if (tla-def-local-p def) "LOCAL " "")
            (if (tla-def-params def)
                (format nil "~A(~{~A~^, ~})"
                        (tla-def-name def)
                        (tla-def-params def))
                (tla-def-name def))
            (emit-formula (tla-def-body def) (+ indent 2)))))

(defun emit-ast-node (node indent)
  "Emit a TLA+ AST node."
  (let ((kind (tla-ast-kind node))
        (value (tla-ast-value node))
        (children (tla-ast-children node)))
    (case kind
      ;; Temporal operators
      (:box (format nil "[](~A)" (emit-formula value 0)))
      (:diamond (format nil "<>(~A)" (emit-formula value 0)))
      (:until (format nil "(~A) U (~A)"
                      (emit-formula (first children) 0)
                      (emit-formula (second children) 0)))
      (:leads-to (format nil "(~A) ~~> (~A)"
                         (emit-formula (first children) 0)
                         (emit-formula (second children) 0)))
      (:next (format nil "O(~A)" (emit-formula value 0)))
      (:prime (format nil "~A'" (emit-value value)))
      (:enabled (format nil "ENABLED ~A" (emit-formula value 0)))
      (:wf (format nil "WF_~A(~A)"
                   (emit-formula (first children) 0)
                   (emit-formula (second children) 0)))
      (:sf (format nil "SF_~A(~A)"
                   (emit-formula (first children) 0)
                   (emit-formula (second children) 0)))
      (:stuttering (format nil "(~A) \\/ UNCHANGED ~A"
                           (emit-formula (first children) 0)
                           (emit-formula (second children) 0)))
      (:subscript (format nil "[~A]_~A"
                          (emit-formula (first children) 0)
                          (emit-formula (second children) 0)))
      (:and (format nil "(~{~A~^ /\\ ~})"
                    (mapcar (lambda (c) (emit-formula c indent)) children)))
      ;; Default
      (t (format nil "~A(~{~A~^, ~})"
                 kind
                 (mapcar (lambda (c) (emit-formula c indent)) children))))))

(defun emit-formula (formula indent)
  "Emit a TLA+ formula from a Lisp expression."
  (cond
    ((null formula) "TRUE")
    ((eq formula t) "TRUE")
    ((eq formula :true) "TRUE")
    ((eq formula :false) "FALSE")
    ((tla-ast-node-p formula) (emit-ast-node formula indent))
    ((symbolp formula) (symbol-name formula))
    ((numberp formula) (format nil "~A" formula))
    ((stringp formula) (format nil "\"~A\"" formula))
    ((atom formula) (format nil "~A" formula))
    ;; Compound formulas
    (t (emit-compound-formula formula indent))))

(defun emit-compound-formula (formula indent)
  "Emit a compound TLA+ formula."
  (let ((op (car formula))
        (args (cdr formula)))
    (case op
      ;; Logic
      (:and (format nil "(~{~A~^ /\\ ~})"
                    (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:or (format nil "(~{~A~^ \\/ ~})"
                   (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:not (format nil "~~(~A)" (emit-formula (first args) indent)))
      (:implies (format nil "(~A) => (~A)"
                        (emit-formula (first args) indent)
                        (emit-formula (second args) indent)))
      (:iff (format nil "(~A) <=> (~A)"
                    (emit-formula (first args) indent)
                    (emit-formula (second args) indent)))
      ;; Quantifiers
      (:forall (format nil "\\A ~A \\in ~A: ~A"
                       (first args)
                       (emit-formula (second args) indent)
                       (emit-formula (third args) indent)))
      (:exists (format nil "\\E ~A \\in ~A: ~A"
                       (first args)
                       (emit-formula (second args) indent)
                       (emit-formula (third args) indent)))
      ;; Temporal
      (:box (format nil "[](~A)" (emit-formula (first args) indent)))
      (:diamond (format nil "<>(~A)" (emit-formula (first args) indent)))
      (:wf (format nil "WF_~A(~A)"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      (:sf (format nil "SF_~A(~A)"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      ;; Sets
      (:in (format nil "~A \\in ~A"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      (:notin (format nil "~A \\notin ~A"
                      (emit-formula (first args) indent)
                      (emit-formula (second args) indent)))
      (:union (format nil "(~{~A~^ \\cup ~})"
                      (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:inter (format nil "(~{~A~^ \\cap ~})"
                      (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:diff (format nil "(~A) \\ (~A)"
                     (emit-formula (first args) indent)
                     (emit-formula (second args) indent)))
      (:subset (format nil "(~A) \\subseteq (~A)"
                       (emit-formula (first args) indent)
                       (emit-formula (second args) indent)))
      (:cardinality (format nil "Cardinality(~A)"
                            (emit-formula (first args) indent)))
      ;; Sequences
      (:head (format nil "Head(~A)" (emit-formula (first args) indent)))
      (:tail (format nil "Tail(~A)" (emit-formula (first args) indent)))
      (:append (format nil "Append(~A, ~A)"
                       (emit-formula (first args) indent)
                       (emit-formula (second args) indent)))
      (:len (format nil "Len(~A)" (emit-formula (first args) indent)))
      (:seq (format nil "<<~{~A~^, ~}>>"
                    (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:concat (format nil "(~A) \\o (~A)"
                       (emit-formula (first args) indent)
                       (emit-formula (second args) indent)))
      ;; Records
      (:record (format nil "[~{~A |-> ~A~^, ~}]"
                       (mapcan (lambda (pair)
                                 (list (car pair)
                                       (emit-formula (cdr pair) indent)))
                               args)))
      (:field (format nil "~A.~A"
                      (emit-formula (first args) indent)
                      (second args)))
      ;; Functions
      (:func (format nil "[~A \\in ~A |-> ~A]"
                     (first args)
                     (emit-formula (second args) indent)
                     (emit-formula (third args) indent)))
      (:apply (format nil "~A[~A]"
                      (emit-formula (first args) indent)
                      (emit-formula (second args) indent)))
      (:domain (format nil "DOMAIN ~A" (emit-formula (first args) indent)))
      (:except (format nil "[~A EXCEPT ![~A] = ~A]"
                       (emit-formula (first args) indent)
                       (emit-formula (second args) indent)
                       (emit-formula (third args) indent)))
      ;; Arithmetic
      (:+ (format nil "(~{~A~^ + ~})"
                  (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:- (if (= (length args) 1)
              (format nil "-~A" (emit-formula (first args) indent))
              (format nil "(~A) - (~A)"
                      (emit-formula (first args) indent)
                      (emit-formula (second args) indent))))
      (:* (format nil "(~{~A~^ * ~})"
                  (mapcar (lambda (a) (emit-formula a indent)) args)))
      (:div (format nil "(~A) \\div (~A)"
                    (emit-formula (first args) indent)
                    (emit-formula (second args) indent)))
      (:mod (format nil "(~A) % (~A)"
                    (emit-formula (first args) indent)
                    (emit-formula (second args) indent)))
      (:range (format nil "~A..~A"
                      (emit-formula (first args) indent)
                      (emit-formula (second args) indent)))
      ;; Comparisons
      (:= (format nil "(~A) = (~A)"
                  (emit-formula (first args) indent)
                  (emit-formula (second args) indent)))
      (:/= (format nil "(~A) # (~A)"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      (:< (format nil "(~A) < (~A)"
                  (emit-formula (first args) indent)
                  (emit-formula (second args) indent)))
      (:> (format nil "(~A) > (~A)"
                  (emit-formula (first args) indent)
                  (emit-formula (second args) indent)))
      (:<= (format nil "(~A) =< (~A)"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      (:>= (format nil "(~A) >= (~A)"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)))
      ;; Primed variables
      (:prime (format nil "~A'" (emit-formula (first args) indent)))
      (:unchanged (format nil "UNCHANGED ~A" (emit-formula (first args) indent)))
      (:tuple (format nil "<<~{~A~^, ~}>>"
                      (mapcar (lambda (a) (emit-formula a indent)) args)))
      ;; Conditionals
      (:if (format nil "IF ~A THEN ~A ELSE ~A"
                   (emit-formula (first args) indent)
                   (emit-formula (second args) indent)
                   (emit-formula (third args) indent)))
      (:case (format nil "CASE ~{~A -> ~A~^ [] ~}"
                     (mapcan (lambda (clause)
                               (list (emit-formula (car clause) indent)
                                     (emit-formula (cdr clause) indent)))
                             args)))
      (:let (format nil "LET ~{~A == ~A~^~%    ~} IN ~A"
                    (mapcan (lambda (b)
                              (list (first b)
                                    (emit-formula (second b) indent)))
                            (first args))
                    (emit-formula (second args) indent)))
      ;; References
      (:ref (symbol-name (first args)))
      ;; Default function application
      (t (format nil "~A(~{~A~^, ~})"
                 op
                 (mapcar (lambda (a) (emit-formula a indent)) args))))))

(defun lisp-to-tla (expr)
  "Convert a Lisp expression to TLA+ formula representation."
  (emit-formula expr 0))
