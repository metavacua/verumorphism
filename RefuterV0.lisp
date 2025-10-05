;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refactored RelNet Weaver Core (Formula-Aware Prototype) - L Rules Only - Operator-Specific Rules - REFUTER (Concise Output) - REFINED INCON/DUAL-INCON
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
  "The global knowledge base for the refuter. In this prototype, it is not
used beyond being passed to rule functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complexity Metrics - Global Counters (Kept but not Printed in Concise Output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refutation-axiom-applications-count* 0
  "Counts axiom applications during a refutation attempt. Reset by `initialize-refutation-knowledge-base`.")
(defvar *refutation-rule-applications-count* 0
  "Counts rule applications during a refutation attempt. Reset by `initialize-refutation-knowledge-base`.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formula Representation (WFF and RFF as Lisp Lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-incon ()
  "Constructs a constant 'incon' (inconsistency) formula."
  '(incon))
(defun make-dep (formula1 formula2)
  "Constructs a dependence formula '(dep F1 F2)'."
  `(dep ,formula1 ,formula2))
(defun make-ind (formula1 formula2)
  "Constructs an independence formula '(ind F1 F2)'."
  `(ind ,formula1 ,formula2))
(defun make-dual (formula)
  "Constructs a dual formula '(dual F)'."
  `(dual ,formula))

(defun formula-type (formula)
  "Extracts the type (e.g., 'dep', 'ind', 'con') from a formula."
  (first formula))

(defun formula-arguments (formula)
  "Extracts the arguments from a formula."
  (rest formula))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms - COMPLEXITY COUNTING - InconL Axiom for IndL(A,A) & Incon Base Case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; InconL Axiom: IndL(A, A) is refuted immediately
;; Incon Base Case: (incon) is refuted immediately

(defun axiom-inconl (formula)
  "Implements the 'Inconsistency Left' (InconL) axiom.

This axiom serves as the base case for refutation. It applies to two forms:
1. The basic inconsistency formula `(incon)`.
2. Any independence formula of the form `(ind A A)`.

Parameters:
  - FORMULA: The formula to check against the axiom.

Returns:
  - The `formula` itself if the axiom applies.
  - `NIL` otherwise.

Side Effects:
  - Increments `*refutation-axiom-applications-count*`."
  (incf *refutation-axiom-applications-count*)
  (cond
    ((eq (formula-type formula) 'incon) formula)              ; Base case: (incon) is refuted
    ((and (eq (formula-type formula) 'ind)
          (equal (second formula) (third formula))) formula) ; Axiom: (ind A A) is refuted
    (t nil)))                                                 ; Axiom does not apply, return nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence & Dual - Formula-Aware & Threaded) - COMPLEXITY COUNTING - OPERATOR-SPECIFIC RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-duald-dual-l (formula kb)
  "Implements the 'Dual-Dual Left' rule, which simplifies `(dual (dual A))` to `A`.

This is a simplification rule that unwraps a doubly-nested dual. The resulting
formula `A` is then passed to the refuter for further processing.

Parameters:
  - FORMULA: The formula to apply the rule to.
  - KB: The knowledge base (ignored).

Returns:
  - The simplified inner formula `A` if the rule applies.
  - `NIL` otherwise.

Side Effects:
  - Increments `*refutation-rule-applications-count*`."
  (declare (ignore kb))
  (incf *refutation-rule-applications-count*)
  (if (and (eq (formula-type formula) 'dual)
           (eq (formula-type (second formula)) 'dual)) ; Check if it's (dual (dual A))
      (second (second formula))                         ; Rule applied, return simplified formula A
      nil))                                          ; Rule does not apply, return nil


(defun rule-dual-incon (formula kb)
  "Implements a specific rule for `(dual incon)`, which is not refutable.

This rule acts as a specific base case, preventing the refutation of `(dual incon)`.
If the formula is not `(dual incon)`, it passes control to `rule-duald-dual-l`.

Parameters:
  - FORMULA: The formula to apply the rule to.
  - KB: The knowledge base.

Returns:
  - `NIL` if the formula is `(dual incon)`.
  - Otherwise, the result of calling `rule-duald-dual-l`.

Side Effects:
  - Increments `*refutation-rule-applications-count*`."
  (declare (ignore kb))
  (incf *refutation-rule-applications-count*)
  (if (and (eq (formula-type formula) 'dual)
           (eq (formula-type (second formula)) 'incon)) ; Check if it's (dual incon)
      nil                                               ; (dual incon) is NOT refuted, return NIL
      (rule-duald-dual-l formula kb)))                 ; Try dualdL rule next if not (dual incon)


(defun rule-dual-l (formula kb)
  "Implements the 'Dual Left' (dualL) rule.

This rule applies to formulas of the form `(dual A)`. It attempts to refute the
formula by recursively trying to refute the inner formula `A`.

Parameters:
  - FORMULA: The formula to apply the rule to.
  - KB: The knowledge base.

Returns:
  - The original `formula` if the sub-refutation of `A` succeeds.
  - Otherwise, the result of `rule-dual-incon` (for base cases like `(dual incon)`).

Side Effects:
  - Increments `*refutation-rule-applications-count*`."
  (declare (ignore kb))
  (incf *refutation-rule-applications-count*)
  (if (eq (formula-type formula) 'dual)
      (let ((formulaA (second formula)))
        (let ((refutation-result (run-refuter formulaA))) ; Recursive call to refuter for formulaA
          (if refutation-result
              formula                                     ; Rule applied, return original formula as refuted
              nil)))                                     ; Rule failed, return nil
      (rule-dual-incon formula kb)))                    ; Try rule-dual-incon next (and dualdL within it)


(defun rule-independence-l (formula kb)
  "Implements the 'Independence Left' (independenceL) rule.

This rule applies to `(ind A B)` formulas. It attempts to refute the formula by
recursively refuting *either* `A` or `B` (a logical OR). The implementation
first checks if the `axiom-inconl` applies (for the `(ind A A)` case).

Parameters:
  - FORMULA: The formula to apply the rule to.
  - KB: The knowledge base.

Returns:
  - The original `formula` if it is an `ind` formula and either sub-refutation succeeds.
  - The result of `rule-dependence-l` if the formula is not of type `ind`.

Side Effects:
  - Increments `*refutation-rule-applications-count*`."
  (declare (ignore kb))
  (incf *refutation-rule-applications-count*)

  ;; Axiom Check First: InconL - for (ind A A) - moved to axiom-inconl
  (let ((axiom-result (axiom-inconl formula)))         ; Check if InconL axiom applies (for ind A A and incon)
    (if axiom-result
        (return-from rule-independence-l axiom-result))) ; If axiom applies, return result immediately

  (if (eq (formula-type formula) 'ind)
      (let ((formula1 (second formula))
            (formula2 (third formula)))
        (let ((premise1-result nil)
              (premise2-result nil))

          ;; Premise 1 for formula1 (alternative refutation path 1)
          (setf premise1-result (run-refuter formula1)) ; Recursive call to refuter for formula1

          ;; Premise 2 for formula2 (alternative refutation path 2)
          (setf premise2-result (run-refuter formula2)) ; Recursive call to refuter for formula2


          (if premise1-result                                       ; If formula1 is refuted, rule succeeds (OR)
              (return-from rule-independence-l formula)         ; Return original formula as refuted
              (if premise2-result                                       ; If formula2 is refuted, rule succeeds (OR)
                  (return-from rule-independence-l formula)         ; Return original formula as refuted
                  (return-from rule-independence-l nil)))))       ; Rule fails if neither premise refuted, return nil
      (rule-dependence-l formula kb)))                    ; Try rule-dependence-l next if not (ind formula)


(defun rule-dependence-l (formula kb)
  "Implements the 'Dependence Left' (dependenceL) rule.

This rule applies to `(dep A B)` formulas. It attempts to refute the formula by
recursively refuting *both* `A` and `B` (a logical AND).

Parameters:
  - FORMULA: The formula to apply the rule to.
  - KB: The knowledge base.

Returns:
  - The original `formula` if it is a `dep` formula and both sub-refutations succeed.
  - The result of `rule-dual-l` if the formula is not of type `dep`.

Side Effects:
  - Increments `*refutation-rule-applications-count*`."
  (declare (ignore kb))
  (incf *refutation-rule-applications-count*)

  (if (eq (formula-type formula) 'dep)
      (let ((formula1 (second formula))
            (formula2 (third formula)))
        (let ((refutation1-result (run-refuter formula1))  ; Recursive call to refuter for formula1
              (refutation2-result (run-refuter formula2)))  ; Recursive call to refuter for formula2
          (if (and refutation1-result refutation2-result)        ; Check if both sub-refutations returned a formula (success - AND)
              formula                                          ; Rule applied, return original formula as refuted
              nil)))                                          ; Rule failed, return nil
      (rule-dual-l formula kb)))                             ; Try rule-dual-l next if not (dep formula) - Dual is last resort


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread Functions (Refutation - Formula-Aware) -  REMOVED refutation-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - refutation-function is now embodied within run-refuter (and rule dispatch)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parallel Interface and Orchestration (Formula-Aware Prototype) - COMPLEXITY REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-refutation-knowledge-base ()
  "Initializes the refuter state by resetting the knowledge base and complexity counters."
  (setf *knowledge-base* nil)
  (reset-refutation-complexity-counters))

(defun reset-refutation-complexity-counters ()
  "Resets the global refutation complexity counters to zero."
  (setf *refutation-axiom-applications-count* 0)
  (setf *refutation-rule-applications-count* 0))


(defun run-refuter (formula)
  "Top-level function to run the theorem refuter on a single formula.

This function serves as the main entry point for a refutation attempt. It
initializes the state and then dispatches the formula to the appropriate
axiom or rule based on its primary operator (`incon`, `ind`, `dep`, `dual`).

Parameters:
  - FORMULA: The formula to be refuted.

Returns:
  - The refuted formula on success.
  - `NIL` if the formula cannot be refuted."
  (initialize-refutation-knowledge-base)
  (cond
    ((eq (formula-type formula) 'incon) (axiom-inconl formula))       ; Base case: incon - axiom check
    ((eq (formula-type formula) 'ind) (rule-independence-l formula *knowledge-base*)) ; Rule for ind
    ((eq (formula-type formula) 'dep) (rule-dependence-l formula *knowledge-base*))    ; Rule for dep
    ((eq (formula-type formula) 'dual) (rule-dual-l formula *knowledge-base*))        ; Rule for dual
    (t nil)))                                                        ; No rule applies

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run Refuter with Formula - COMPLEXITY REPORTING and TESTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  "The main entry point for demonstrating and testing the refuter.

This function constructs several example formulas and runs the refuter on each
one, printing the results to standard output. It serves as a test harness for
the refuter's logic.

Parameters:
  - None.

Returns:
  - Nothing."
  (format t "Starting Formula-Aware Theorem Refuter Prototype (L-Rules Only - Operator-Specific Rules - Refined Incon/Dual-Incon).~%")

  ;; Example Formula Construction
  (let* ((incon-formula (make-incon))
         (dep-formula (make-dep incon-formula incon-formula))
         (ind-formula (make-ind incon-formula incon-formula))
         (self-ind-formula (make-ind incon-formula incon-formula)) ; (ind A A) where A is (incon) - for InconL axiom test
         (complex-formula (make-ind dep-formula ind-formula))
         (self-ref-formula (make-ind incon-formula incon-formula)) ; another (ind A A) for testing InconL axiom
         (dual-incon-formula (make-dual incon-formula))         ; (dual incon) for dualL rule test - now dual-incon rule
         (dual-dep-formula (make-dual dep-formula))           ; (dual (dep incon incon)) for dualL rule test
         (duald-dual-incon-formula (make-dual (make-dual incon-formula)))) ; (dual (dual incon)) for dualdL rule test


    (format t "~%--- Testing with incon formula ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter incon-formula)))
      (format t "~%Refuter Result for formula ~A: ~A~%" incon-formula refuter-result)) ; EXPECTED: (INCON)

    (format t "~%--- Testing with (dep incon incon) formula ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter dep-formula)))
      (format t "~%Refuter Result for formula ~A: ~A~%" dep-formula refuter-result)) ; EXPECTED: NIL

    (format t "~%--- Testing with (ind incon incon) formula ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter ind-formula)))
      (format t "~%Refuter Result for formula ~A: ~A~%" ind-formula refuter-result)) ; EXPECTED: (IND (INCON) (INCON))

    (format t "~%--- Testing with (ind incon incon) - Self Independence Axiom (InconL) ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter self-ind-formula))) ; Testing (ind incon incon) - InconL axiom
      (format t "~%Refuter Result for formula ~A: ~A~%" self-ind-formula refuter-result)) ; EXPECTED: (IND (INCON) (INCON))

    (format t "~%--- Testing with (ind (dep incon incon) (ind incon incon)) formula ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter complex-formula)))
      (format t "~%Refuter Result for formula ~A: ~A~%" complex-formula refuter-result)) ; EXPECTED: (IND (DEP (INCON) (INCON)) (IND (INCON) (INCON)))

    (format t "~%--- Testing with another (ind incon incon) - Self Ref Axiom (InconL) ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter self-ref-formula))) ; Another test for (ind incon incon) - InconL axiom
      (format t "~%Refuter Result for formula ~A: ~A~%" self-ref-formula refuter-result)) ; EXPECTED: (IND (INCON) (INCON))

    (format t "~%--- Testing with (dual incon) formula - dual-incon Rule ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter dual-incon-formula))) ; Testing (dual incon) - dual-incon rule
      (format t "~%Refuter Result for formula ~A: ~A~%" dual-incon-formula refuter-result)) ; EXPECTED: NIL

    (format t "~%--- Testing with (dual (dep incon incon)) formula - dualL Rule ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter dual-dep-formula))) ; Testing (dual (dep incon incon)) - dualL rule
      (format t "~%Refuter Result for formula ~A: ~A~%" dual-dep-formula refuter-result)) ; EXPECTED: NIL

    (format t "~%--- Testing with (dual (dual incon)) formula - dualdL Rule ---~%")
    (reset-refutation-complexity-counters)
    (let ((refuter-result (run-refuter duald-dual-incon-formula))) ; Testing (dual (dual incon)) - dualdL rule
      (format t "~%Refuter Result for formula ~A: ~A~%" duald-dual-incon-formula refuter-result)) ; EXPECTED: NIL


    (format t "~%Formula-Aware Theorem Refuter Prototype Finished (L-Rules Only - Operator-Specific Rules - Refined Incon/Dual-Incon).~%")))

(main)