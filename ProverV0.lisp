;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refactored RelNet Weaver Core (Formula-Aware Prototype) - R Rules Only - ConR Axiom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structures (Minimal for Prototype) - Enhanced Node and Formulae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass relnet-node ()
  ((name :initarg :name :accessor relnet-node-name)
   (type :initarg :type :accessor relnet-node-type))
  (:documentation "Represents a node in the relational network, enhanced with a type.
This is a basic structure for elements within the prover's universe.

Slots:
  - NAME: The symbolic name of the node.
  - TYPE: The type of the node (e.g., 'formula', 'term')."))

(defvar *knowledge-base* nil
  "The global knowledge base for the prover. In this prototype, it is not
used beyond being passed to axiom and rule functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complexity Metrics - Global Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *axiom-applications-count* 0
  "Counts the total number of axiom applications within a single `run-prover` call.
This serves as a basic complexity metric and is reset by `initialize-knowledge-base`.")
(defvar *rule-applications-count* 0
  "Counts the total number of rule applications within a single `run-prover` call.
This serves as a basic complexity metric and is reset by `initialize-knowledge-base`.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formula Representation (WFF and RFF as Lisp Lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-con ()
  "Constructs a constant 'con' formula."
  '(con))
(defun make-dep (formula1 formula2)
  "Constructs a dependence formula '(dep F1 F2)'."
  `(dep ,formula1 ,formula2))
(defun make-ind (formula1 formula2)
  "Constructs an independence formula '(ind F1 F2)'."
  `(ind ,formula1 ,formula2))

(defun formula-type (formula)
  "Extracts the type (e.g., 'dep', 'ind', 'con') from a formula."
  (first formula))

(defun formula-arguments (formula)
  "Extracts the arguments from a formula."
  (rest formula))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms (ConR - Formula-Aware) - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-ConR (formula kb)
  "Implements the 'Consistency Right' (ConR) axiom for formulas of the form '(dep A A)'.

This axiom states that any formula is dependent on itself. If the input `formula`
matches the structure `(dep A A)`, the axiom applies, and the function returns A
to signify that the sub-formula is proven.

Parameters:
  - FORMULA: The formula to check against the axiom.
  - KB: The knowledge base (ignored in this implementation).

Returns:
  - The sub-formula `A` if the axiom applies.
  - `NIL` if the axiom does not apply.

Side Effects:
  - Increments `*axiom-applications-count*`."
  (declare (ignore kb))
  (incf *axiom-applications-count*)
  (format t "Proof Thread: Applying ConR axiom to formula: ~A~%" formula)
  (if (and (eq (formula-type formula) 'dep)
           (equal (second formula) (third formula))) ; Check if A == A in (dep A A)
      (let ((proven-formula (second formula)))       ; Extract A as the proven formula
        (format t "Proof Thread: Formula ~A is axiomatically proven (ConR).~%" formula)
        proven-formula)                              ; Return A itself to signify proof
      (progn
        (format t "Proof Thread: ConR axiom does not apply to formula ~A.~%" formula)
        nil)))                                       ; Axiom does not apply


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence - Formula-Aware & Threaded) - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-dependence-r (formula kb)
  "Implements the 'Dependence Right' (dependenceR) rule.

This rule applies to formulas of the form `(dep A B)`. It attempts to prove
the formula by recursively proving both sub-formulas `A` and `B`. This represents
a logical AND condition.

Parameters:
  - FORMULA: The formula to which the rule is applied.
  - KB: The knowledge base (ignored).

Returns:
  - The original `formula` if both sub-proofs succeed.
  - `NIL` if the rule does not apply or if either sub-proof fails.

Side Effects:
  - Increments `*rule-applications-count*`."
  (format t "Proof Thread: Attempting rule dependenceR (Dependence Right) on formula: ~A~%" formula)
  (incf *rule-applications-count*)
  (if (eq (formula-type formula) 'dep)
      (let ((formula1 (second formula))
            (formula2 (third formula)))
        (format t "Proof Thread: Applying dependenceR to sub-formulae ~A and ~A.~%" formula1 formula2)
        (let ((proof1-result (proof-function formula1))  ; Attempt to prove formula1 via proof-function
              (proof2-result (proof-function formula2)))  ; Attempt to prove formula2 via proof-function
          (if (and proof1-result proof2-result)         ; Check if both sub-proofs returned a formula (success)
              (progn
                (format t "Proof Thread: Rule dependenceR applied successfully - Proven (dependence A B).~%")
                formula)                                ; Rule applied, return original formula as proven
              (progn
                (format t "Proof Thread: Rule dependenceR failed to apply (premises not both proven).~%")
                nil))))                                 ; Rule failed, return nil
      (progn
        (format t "Proof Thread: Rule dependenceR does not apply to formula type ~A.~%" (formula-type formula))
        nil)))                                         ; Rule does not apply to formula type


(defun rule-independence-r (formula kb)
  "Implements the 'Independence Right' (independenceR) rule.

This rule applies to formulas of the form `(ind A B)`. It attempts to prove
the formula by recursively proving either sub-formula `A` or `B`. This represents
a logical OR condition. The prototype simulates this by trying one after the other.

Parameters:
  - FORMULA: The formula to which the rule is applied.
  - KB: The knowledge base (ignored).

Returns:
  - Two values:
    1. The original `formula` if either sub-proof succeeds, otherwise `NIL`.
    2. A keyword (`:premise1-satisfied` or `:premise2-satisfied`) indicating
       which branch succeeded, otherwise `NIL`.

Side Effects:
  - Increments `*rule-applications-count*`."
  (format t "Proof Thread: Attempting rule independenceR (Independence Right) - THREADED on formula: ~A~%" formula)
  (incf *rule-applications-count*)
  (if (eq (formula-type formula) 'ind)
      (let ((formula1 (second formula))
            (formula2 (third formula)))
        (format t "Proof Thread: Applying independenceR to sub-formulae ~A and ~A (in parallel).~%" formula1 formula2)
        (let ((premise1-result nil)
              (premise2-result nil))

          ;; Premise 1 for formula1 (alternative proof path 1)
          (setf premise1-result (proof-function formula1))

          ;; Premise 2 for formula2 (alternative proof path 2)
          (setf premise2-result (proof-function formula2))

          (format t "Proof Thread: Both premises of *IND*R evaluated in parallel for formula ~A.~%" formula)

          (if premise1-result                             ; If formula1 is proven, rule succeeds (OR)
              (progn
                (format t "Proof Thread: Rule independenceR applied successfully - Proven (independence A B) (via first premise).~%")
                (return-from rule-independence-r (values formula :premise1-satisfied))) ; Return original formula and premise info
              (if premise2-result                         ; If formula2 is proven, rule succeeds (OR)
                  (progn
                    (format t "Proof Thread: Rule independenceR applied successfully - Proven (independence A B) (via second premise).~%")
                    (return-from rule-independence-r (values formula :premise2-satisfied))) ; Return original formula and premise info
                  (progn
                    (format t "Proof Thread: Rule independenceR failed to apply (neither premise proven).~%")
                    (return-from rule-independence-r (values nil nil))))))) ; Rule fails if neither premise proven, return nil
      (progn
        (format t "Proof Thread: Rule independenceR does not apply to formula type ~A.~%" (formula-type formula))
        nil)))                                         ; Rule does not apply to formula type



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread Functions (Proof - Formula-Aware)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun proof-function (formula)
  "The main proof-seeking function that attempts to prove a given formula.

It orchestrates the proof search by sequentially applying a set of axioms and rules:
1. `axiom-ConR`
2. `rule-dependence-r`
3. `rule-independence-r`

The function returns as soon as one of these methods succeeds.

Parameters:
  - FORMULA: The formula to be proven.

Returns:
  - The proven formula itself on success.
  - `NIL` if no proof is found using the available axioms and rules."
  (format t "Proof Thread: Starting for formula ~A.~%" formula)

  ;; 1. Try axiom ConR
  (let ((axiom-ConR-result (axiom-ConR formula *knowledge-base*)))
    (when axiom-ConR-result                             ; Axiom returns formula itself if proven
      (format t "Proof Thread: Terminating (via ConR axiom for formula ~A).~%" formula)
      (return-from proof-function axiom-ConR-result)))   ; Return the proven formula

  ;; 2. Try rule *DEP*R
  (let ((rule-dep-r-result (rule-dependence-r formula *knowledge-base*)))
    (when rule-dep-r-result                              ; Rule returns formula itself if applied
      (format t "Proof Thread: Terminating (via rule dependenceR for formula ~A).~%" formula)
      (return-from proof-function rule-dep-r-result)))    ; Return the proven formula

  ;; 3. Try rule *IND*R
  (multiple-value-bind (rule-ind-r-result premise-satisfied)
      (rule-independence-r formula *knowledge-base*)
    (declare (ignore premise-satisfied))
    (when rule-ind-r-result                              ; Rule returns formula itself if applied
      (format t "Proof Thread: Terminating (via rule independenceR for formula ~A).~%" formula)
      (return-from proof-function rule-ind-r-result)))    ; Return the proven formula


  ;; 4. No proof found in this iteration (Exhausted rules)
  (format t "Proof Thread: No proof found for formula ~A in this iteration.~%" formula)
  (format t "Proof Thread: Terminating (no proof for formula ~A).~%" formula)
  nil)                                                  ; Return nil for no proof



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parallel Interface and Orchestration (Formula-Aware Prototype) - COMPLEXITY REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-knowledge-base ()
  "Initializes the prover state by resetting the knowledge base and complexity counters."
  (setf *knowledge-base* nil)
  (reset-complexity-counters))

(defun reset-complexity-counters ()
  "Resets the global complexity counters to zero."
  (setf *axiom-applications-count* 0)
  (setf *rule-applications-count* 0))


(defun run-prover (formula)
  "Top-level function to run the theorem prover on a single formula.

This function initializes the prover's state and then invokes the main
`proof-function` to attempt to prove the given formula. It serves as the
primary entry point for a single proof attempt.

Parameters:
  - FORMULA: The formula to be proven.

Returns:
  - The proven formula on success.
  - `NIL` if the formula cannot be proven."
  (format t "Prover: Initializing Knowledge Base.~%")
  (initialize-knowledge-base)

  (format t "Prover: Starting Proof Thread for formula ~A.~%" formula)
  (let ((proof-result (proof-function formula)))


    (if proof-result                                     ; If proof-result is not nil (i.e., a formula)
        (progn
          (format t "Prover: Proof Thread terminated. Result: Proven.~%")
          proof-result)                                  ; Return the proven formula
        (progn
          (format t "Prover: Proof Thread terminated. Result: Unproven.~%")
          nil))))                                       ; Return nil for unproven


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run Prover with Formula - COMPLEXITY REPORTING and TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  "The main entry point for demonstrating and testing the prover.

This function constructs several example formulas and runs the prover on each
one, printing the results and complexity metrics to standard output. It serves
as a test harness for the prover's logic.

Parameters:
  - None.

Returns:
  - Nothing."
  (format t "Starting Formula-Aware Theorem Prover Prototype (R-Rules Only - ConR Axiom).~%")

  ;; Example Formula Construction
  (let* ((con-formula (make-con))
         (dep-formula (make-dep con-formula con-formula))
         (ind-formula (make-ind con-formula con-formula))
         (self-dep-formula (make-dep con-formula con-formula)) ; (dep A A) where A is (con)
         (complex-formula (make-dep dep-formula ind-formula))
         (self-ref-formula (make-dep con-formula con-formula))) ; another (dep A A) for testing ConR

    (format t "~%--- Testing with con formula ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover con-formula)))
      (format t "~%Prover Result for formula ~A: ~A~%" con-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))

    (format t "~%--- Testing with (dep con con) formula ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover dep-formula)))
      (format t "~%Prover Result for formula ~A: ~A~%" dep-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))

    (format t "~%--- Testing with (ind con con) formula ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover ind-formula)))
      (format t "~%Prover Result for formula ~A: ~A~%" ind-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))

    (format t "~%--- Testing with (dep con con) - Self Dependence Axiom ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover self-dep-formula))) ; Testing (dep con con) which fits ConR
      (format t "~%Prover Result for formula ~A: ~A~%" self-dep-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))

    (format t "~%--- Testing with (dep (dep con con) (ind con con)) formula ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover complex-formula)))
      (format t "~%Prover Result for formula ~A: ~A~%" complex-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))

    (format t "~%--- Testing with another (dep con con) - Self Ref Axiom ---~%")
    (reset-complexity-counters)
    (let ((prover-result (run-prover self-ref-formula))) ; Another test for (dep con con) and ConR
      (format t "~%Prover Result for formula ~A: ~A~%" self-ref-formula prover-result)
      (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
      (format t "Rule Applications:  ~A~%" *rule-applications-count*))


    (format t "~%Formula-Aware Theorem Prover Prototype Finished (R-Rules Only - ConR Axiom).~%")))

(main)