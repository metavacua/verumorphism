(defstruct rule
  "Represents an axiom or inference rule in the hypersequent calculus.

Slots:
  - NAME: The symbolic name of the rule (e.g., 'con_R).
  - TYPE: The type of rule, e.g., :axiom, :sequent-rule, :hypersequent-rule.
  - CALCULUS_TYPE: The specific calculus this rule belongs to.
  - OPERATOR: The logical operator this rule applies to (e.g., :con, :dep).
  - DIRECTION: The direction of the rule application (:left or :right), if applicable.
  - INPUT-SEQUENT: The pattern for the input sequent(s) that this rule matches.
  - OUTPUT-SEQUENT: The pattern for the sequent produced by this rule.
  - PREMISE_CONDITION_FN: A function that checks if the premises for the rule hold.
    It receives a list of bindings from the pattern match.
  - DESCRIPTION: A human-readable description of the rule."
  name
  type
  calculus_type
  operator
  direction
  input-sequent
  output-sequent
  premise_condition_fn ; Changed from premise-condition to premise_condition_fn
  description)

;; Function to check if a symbol is a variable (starts with '?')
(defun variablep (symbol)
  "Checks if a given symbol is a variable.
Variables are denoted by a '?' prefix, e.g., ?A."
  (and (symbolp symbol)
       (char= (char (symbol-name symbol) 0) #\?)))

(defun sequent-equal (sequent1 sequent2)
  "Compares two sequents for structural equality."
  (equalp sequent1 sequent2))

;; Function to substitute bindings into a template (basic)
(defun substitute-bindings (template bindings)
  "Recursively substitutes variables in a template with their values from a bindings list.

Parameters:
  - TEMPLATE: A list structure (potentially containing variables) to be filled.
  - BINDINGS: An association list mapping variables to their values (e.g., '((?A . foo))').

Returns:
  - A new list structure with all variables replaced by their bound values."
  (cond ((variablep template) (cdr (assoc template bindings)))
        ((consp template) (cons (substitute-bindings (car template) bindings)
                                 (substitute-bindings (cdr template) bindings)))
        (t template)))

;; Function to match patterns (revised to return NIL on no match, and bindings (possibly empty list) on match)
(defun pattern-match (pattern input bindings)
  "Matches a pattern against an input, returning a list of variable bindings.
This is a core utility for the rule-based prover to determine if a rule can be
applied to a given sequent.

Parameters:
  - PATTERN: The pattern to match against, may contain variables (e.g., '(?A b c)').
  - INPUT: The concrete data to be matched (e.g., '(a b c)').
  - BINDINGS: An initial association list of bindings.

Returns:
  - An association list of bindings if the match is successful.
  - NIL if the match fails."
  (format t "  pattern-match: pattern=~S, input=~S, bindings=~S~%" pattern input bindings)
  (cond ((equal pattern input) (if bindings bindings '())) ; Return '() for exact match if no initial bindings, otherwise return bindings
        ((variablep pattern) (if (assoc pattern bindings)
                                   (if (equal (cdr (assoc pattern bindings)) input) bindings nil)
                                   (acons pattern input bindings)))
        ((and (consp pattern) (consp input))
         (let ((car-bindings (pattern-match (car pattern) (car input) bindings)))
           (if car-bindings
               (pattern-match (cdr pattern) (cdr input) car-bindings)
               nil)))
        (t nil)))


(defparameter con_r-rule
  (make-rule
   :name 'con_R
   :type :axiom
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :con
   :output-sequent '(() con)
   :premise_condition_fn nil ; No premise condition for axioms
   :description "Axiom: Provability of consistency (con). Output is minimal sequent (entails con), represented as (() con)."))

(defparameter incon_l-rule
  (make-rule
   :name 'incon_L
   :type :axiom
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :incon
   :output-sequent '((incon) ())
   :premise_condition_fn nil ; No premise condition for axioms
   :description "Axiom: Refutability of inconsistency (incon). Output is minimal sequent (incon entails), represented as ((incon) ())."))

(defparameter duality_r_dualR-rule
  (make-rule
   :name 'NonContextual_duality_R_dualR
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :duality
   :direction :right
   :input-sequent '((?A) ())
   :output-sequent '(() (dual ?A))
   :premise_condition_fn nil ; No premise condition for duality rules in this direction
   :description "Duality Right Rule (dualR): Transforms minimal sequent (A entails), represented as ((?A) ()), to minimal sequent (entails dual A), represented as (() (dual ?A)). Uses 'dual' for duality operator in code."))

(defparameter duality_l_dualL-rule
  (make-rule
   :name 'NonContextual_duality_L_dualL
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :duality
   :direction :left
   :input-sequent '(() (dual ?A))
   :output-sequent '((?A) ())
   :premise_condition_fn nil ; No premise condition for duality rules in this direction
   :description "Duality Left Rule (dualL): Transforms minimal sequent (entails dual A), represented as (() (dual ?A)), to minimal sequent (A entails), represented as ((?A) ()). Uses 'dual' for duality operator in code."))

(defparameter hypersequent_independence_r_indepR-rule
  (make-rule
   :name 'Hypersequent_independence_R_indepR
   :type :hypersequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :independence
   :direction :right
   :input-sequent '(() (indep ?A ?B)) ; Input sequent for indepR rule
   :output-sequent '(() (indep ?A ?B)) ; Output sequent is the same as input in indepR - corrected input and output
   :premise_condition_fn #'(lambda (bindings) ; Premise condition as a function
                                           (format t "  indepR premise_condition_fn called with bindings: ~S~%" bindings)
                                           (let ((sequent-a (substitute-bindings '(() ?A) bindings))
                                                 (sequent-b (substitute-bindings '(() ?B) bindings)))
                                             (format t "  indepR checking premise A sequent: ~S~%" sequent-a)
                                             (format t "  indepR checking premise B sequent: ~S~%" sequent-b)
                                             (or (entails sequent-a)
                                                 (entails sequent-b))))
   :description "Hypersequent Independence Right Rule (indepR): From premise (entails A) or (entails B), infer minimal sequent (entails indep A B). Uses 'indep' for independence operator in code."))

(defparameter hypersequent_independence_l_indepL-rule
  (make-rule
   :name 'Hypersequent_independence_L_indepL
   :type :hypersequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :independence
   :direction :left
   :input-sequent '(((indep ?A ?B) ())) ; Input sequent for indepL rule
   :output-sequent '(((indep ?A ?B) ())) ; Output sequent same as input in indepL - corrected input and output
   :premise_condition_fn #'(lambda (bindings) ; Premise condition as a function
                                           (format t "  indepL premise_condition_fn called with bindings: ~S~%" bindings)
                                           (let ((sequent-a (substitute-bindings '((?A) ()) bindings))
                                                 (sequent-b (substitute-bindings '((?B) ()) bindings)))
                                             (format t "  indepL checking premise A sequent: ~S~%" sequent-a)
                                             (format t "  indepL checking premise B sequent: ~S~%" sequent-b)
                                             (or (entails sequent-a)
                                                 (entails sequent-b))))
   :description "Hypersequent Independence Left Rule (indepL): From premise (A entails) or (B entails), infer minimal sequent (indep A B entails). Uses 'indep' for independence operator in code."))

(defparameter dependence_r_depR-rule
  (make-rule
   :name 'Dependence_R_depR
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :dependence
   :direction :right
   :input-sequent '(() (dep ?A ?B)) ; Input sequent for depR rule
   :output-sequent '(() (dep ?A ?B)) ; Output sequent same as input for depR - corrected input and output
   :premise_condition_fn #'(lambda (bindings) ; Premise condition as a function
                                           (format t "  depR premise_condition_fn called with bindings: ~S~%" bindings)
                                           (let ((sequent-a (substitute-bindings '(() ?A) bindings))
                                                 (sequent-b (substitute-bindings '(() ?B) bindings)))
                                             (format t "  depR checking premise A sequent: ~S~%" sequent-a)
                                             (format t "  depR checking premise B sequent: ~S~%" sequent-b)
                                             (and (entails sequent-a)
                                                  (entails sequent-b))))
   :description "Dependence Right Rule (depR): From premise (entails A) and (entails B), infer minimal sequent (entails dep A B). Uses 'dep' for dependence operator in code."))

(defparameter dependence_l_depL-rule
  (make-rule
   :name 'Dependence_L_depL
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :dependence
   :direction :left
   :input-sequent '(((dep ?A ?B) ())) ; Input sequent for depL rule
   :output-sequent '(((dep ?A ?B) ())) ; Output sequent same as input for depL - corrected input and output
   :premise_condition_fn #'(lambda (bindings) ; Premise condition as a function
                                           (format t "  depL premise_condition_fn called with bindings: ~S~%" bindings)
                                           (let ((sequent-a (substitute-bindings '((?A) ()) bindings))
                                                 (sequent-b (substitute-bindings '((?B) ()) bindings)))
                                             (format t "  depL checking premise A sequent: ~S~%" sequent-a)
                                             (format t "  depL checking premise B sequent: ~S~%" sequent-b)
                                             (and (entails sequent-a)
                                                  (entails sequent-b))))
   :description "Dependence Left Rule (depL): From premise (A entails) and (entails B), infer minimal sequent (dep A B entails). Uses 'dep' for dependence operator in code."))


;; Revised 'entails' function with Independence and Dependence Rules, using premise_condition_fn
(defun entails (sequent)
  "The core proof search function of the theorem prover.
It determines if a given sequent is provable ('entailed') by the defined set of
axioms and inference rules. It works by recursively trying to match the sequent
against the output of axioms or the input of rules. For rules with premises,
it recursively calls itself to check if the premises hold.

Parameters:
  - SEQUENT: The sequent to be proven, represented as a list.

Returns:
  - T if the sequent is provable.
  - NIL otherwise."
  (format t "Entails called with sequent: ~S~%" sequent)   ; Debugging print

  ;; 1. Check Axioms (con_R and incon_l)
  (let ((bindings (pattern-match (rule-output-sequent con_r-rule) sequent nil))) ; Match sequent against axiom output
    (if bindings
        (progn
          (format t "  Matched axiom: con_r~%")    ; Debugging print
          (return-from entails t))))

  (let ((bindings (pattern-match (rule-output-sequent incon_l-rule) sequent nil))) ; Match sequent against axiom output
    (if bindings
        (progn
          (format t "  Matched axiom: incon_l~%")    ; Debugging print
          (return-from entails t))))

  ;; 2. Try Duality Rules (duality_r_dualR and duality_l_dualL)
  (let ((bindings (pattern-match (rule-input-sequent duality_r_dualR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: duality_r_dualR with bindings: ~S~%" bindings)      ; Debugging print
          (let ((output-sequent (substitute-bindings (rule-output-sequent duality_r_dualR-rule) bindings)))
            (format t "  Applying rule, new sequent: ~S~%" output-sequent)    ; Debugging print
            (if (entails output-sequent)      ; Recursive call
                (return-from entails t))))))

  (let ((bindings (pattern-match (rule-input-sequent duality_l_dualL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: duality_l_dualL with bindings: ~S~%" bindings)      ; Debugging print
          (let ((output-sequent (substitute-bindings (rule-output-sequent duality_l_dualL-rule) bindings)))
            (format t "  Applying rule, new sequent: ~S~%" output-sequent)    ; Debugging print
            (if (entails output-sequent)     ; Recursive call
                (return-from entails t))))))

  ;; 3. Try Independence Right Rule (indep_r_indepR)
  (let ((bindings (pattern-match (rule-input-sequent hypersequent_independence_r_indepR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: hypersequent_independence_r_indepR with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (funcall (rule-premise_condition_fn hypersequent_independence_r_indepR-rule) bindings))) ; Call premise_condition_fn
            (format t "  Premise condition for indep_r_indepR: ~S, Condition Value: ~S~%"
                    (rule-premise_condition_fn hypersequent_independence_r_indepR-rule) premise-condition-met)
            (if premise-condition-met
                (progn
                  (format t "  Premise condition met for indep_r_indepR - Returning T~%")
                  ;; Corrected: Return t directly if premise condition is met
                  (return-from entails t))))
          (format t "  indep_r_indepR - After conditional - Should not reach here if premise condition was met~%")))) ; Debugging print - Should not reach here if premise condition was met

  ;; 4. Try Independence Left Rule (indep_l_indepL)
  (let ((bindings (pattern-match (rule-input-sequent hypersequent_independence_l_indepL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: hypersequent_independence_l_indepL with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (funcall (rule-premise_condition_fn hypersequent_independence_l_indepL-rule) bindings))) ; Call premise_condition_fn
            (format t "  Premise condition for indep_l_indepL: ~S, Condition Value: ~S~%"
                    (rule-premise_condition_fn hypersequent_independence_l_indepL-rule) premise-condition-met)
            (if premise-condition-met
                (progn
                  (format t "  Premise condition met for indep_l_indepL - Returning T~%")
                  ;; Corrected: Return t directly if premise condition is met
                  (return-from entails t))))
          (format t "  indep_l_indepL - After conditional - Should not reach here if premise condition was met~%")))) ; Debugging print - Should not reach here if premise condition was met


  ;; 5. Try Dependence Right Rule (dep_r_depR)
  (let ((bindings (pattern-match (rule-input-sequent dependence_r_depR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: dependence_r_depR with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (funcall (rule-premise_condition_fn dependence_r_depR-rule) bindings))) ; Call premise_condition_fn
            (format t "  Premise condition for dep_r_depR: ~S, Condition Value: ~S~%"
                    (rule-premise_condition_fn dependence_r_depR-rule) premise-condition-met)
            (if premise-condition-met
                (progn
                  (format t "  Premise condition met for dep_r_depR - Returning T~%")
                  ;; Corrected: Return t directly if premise condition is met
                  (return-from entails t))))
          (format t "  dep_r_depR - After conditional - Should not reach here if premise condition was met~%")))) ; Debugging print - Should not reach here if premise condition was met


  ;; 6. Try Dependence Left Rule (dep_l_depL)
  (let ((bindings (pattern-match (rule-input-sequent dependence_l_depL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: dependence_l_depL with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (funcall (rule-premise_condition_fn dependence_l_depL-rule) bindings))) ; Call premise_condition_fn
            (format t "  Premise condition for dep_l_depL: ~S, Condition Value: ~S~%"
                    (rule-premise_condition_fn dependence_l_depL-rule) premise-condition-met)
            (if premise-condition-met
                (progn
                  (format t "  Premise condition met for dep_l_depL - Returning T~%")
                  ;; Corrected: Return t directly if premise condition is met
                  (return-from entails t))))
          (format t "  dep_l_depL - After conditional - Should not reach here if premise condition was met~%")))) ; Debugging print - Should not reach here if premise condition was met


  ;; 7. No rule applied or proof not found
  (format t "  No rule applied or proof not found for: ~S~%" sequent)   ; Debugging print
  nil)


;;;; Unit Tests for Pattern Matching and Substitution ;;;;

;;; 1. Unit Tests for pattern-match function ;;;

(defun run-pattern-match-tests ()
  "Runs a suite of unit tests for the `pattern-match` function."
  (test-description "Pattern Match Tests")
  (test-pattern-match-exact-match)
  (test-pattern-match-variable-binding)
  (test-pattern-match-variable-binding-existing)
  (test-pattern-match-recursive-match)
  (test-pattern-match-no-match)
  (test-pattern-match-mismatch-variable-binding)
  (test-pattern-match-variable-order-recursive))   ; Added test for variable order in recursive match

(defun test-description (description)
  "Prints a formatted header for a test section."
  (format t ";;;; ~A ;;;;~%" description))

(defun test-pattern-match-exact-match ()
  "Tests if `pattern-match` succeeds with an empty binding list for identical inputs."
  (let ((pattern '(a b c))
        (input '(a b c)))
    (assert-bindings-equal (pattern-match pattern input nil) '() "Exact Match Test Failed"))) ; Expect empty list now

(defun test-pattern-match-variable-binding ()
  "Tests basic variable binding."
  (let ((pattern '(?x b c))
        (input '(a b c))
        (expected-bindings '((?x . a))))
    (assert-bindings-equal (pattern-match pattern input nil) expected-bindings "Variable Binding Test Failed")))

(defun test-pattern-match-variable-binding-existing ()
  "Tests matching with a pre-existing, consistent variable binding."
  (let ((pattern '(?x b c))
        (input '(a b c))
        (initial-bindings '((?x . a)))
        (expected-bindings '((?x . a))))
    (assert-bindings-equal (pattern-match pattern input initial-bindings) expected-bindings "Existing Variable Binding Test Failed")))

(defun test-pattern-match-recursive-match ()
  "Tests pattern matching on nested list structures."
  (let ((pattern '((?x) b (?y)))
        (input '((a) b (c)))
        (expected-bindings '((?x . a) (?y . c))))
    (assert-bindings-equal (pattern-match pattern input nil) expected-bindings "Recursive Match Test Failed")))

(defun test-pattern-match-no-match ()
  "Tests a case where the pattern and input cannot match."
  (let ((pattern '(a b d))
        (input '(a b c)))
    (assert-equal (pattern-match pattern input nil) nil "No Match Test Failed")))

(defun test-pattern-match-mismatch-variable-binding ()
  "Tests a case where the input conflicts with a pre-existing binding."
  (let ((pattern '(?x b c))
        (input '(d b c))
        (initial-bindings '((?x . a))))
    (assert-equal (pattern-match pattern input initial-bindings) nil "Mismatch Variable Binding Test Failed")))

(defun test-pattern-match-variable-order-recursive ()
  "Tests that variable binding order in the pattern does not affect the result."
  (let ((pattern '((?y) b (?x)))    ; Variables in different order than input
        (input '((a) b (c)))
        (expected-bindings '((?y . a) (?x . c))))
    (assert-bindings-equal (pattern-match pattern input nil) expected-bindings "Variable Order Recursive Match Test Failed")))


;;; 2. Unit Tests for substitute-bindings function ;;;

(defun run-substitute-bindings-tests ()
  "Runs a suite of unit tests for the `substitute-bindings` function."
  (test-description "Substitute Bindings Tests")
  (test-substitute-bindings-variable-substitution)
  (test-substitute-bindings-recursive-substitution)
  (test-substitute-bindings-no-substitution)
  (test-substitute-bindings-mixed-substitution))

(defun test-substitute-bindings-variable-substitution ()
  "Tests basic substitution of a single variable."
  (let ((template '(?x b c))
        (bindings '((?x . a)))
        (expected-output '(a b c)))
    (assert-equal (substitute-bindings template bindings) expected-output "Variable Substitution Test Failed")))

(defun test-substitute-bindings-recursive-substitution ()
  "Tests substitution in a nested list structure."
  (let ((template '((?x) b (?y)))
        (bindings '((?x . a) (?y . c)))
        (expected-output '((a) b (c))))
    (assert-equal (substitute-bindings template bindings) expected-output "Recursive Substitution Test Failed")))

(defun test-substitute-bindings-no-substitution ()
  "Tests that the template is unchanged when no variables match the bindings."
  (let ((template '(a b c))
        (bindings '((?x . a)))
        (expected-output '(a b c)))
    (assert-equal (substitute-bindings template bindings) expected-output "No Substitution Test Failed")))

(defun test-substitute-bindings-mixed-substitution ()
  "Tests substitution in a template with both variables and literal atoms."
  (let ((template '((?x) b literal (?y)))
        (bindings '((?x . a) (?y . c)))
        (expected-output '((a) b literal (c))))
    (assert-equal (substitute-bindings template bindings) expected-output "Mixed Substitution Test Failed")))


;;; 3. Assertion Helper Functions ;;;

(defun assert-equal (actual expected test-name)
  "A simple assertion helper that checks for `equal`ity between two values."
  (if (equal actual expected)
      (format t "~A: PASS~%" test-name)
      (format t "~A: FAIL - Expected:~%  ~S~%Actual:~%  ~S~%" test-name expected actual)))

(defun assert-bindings-equal (actual expected test-name)
  "An assertion helper for comparing binding lists. It is order-insensitive."
  (flet ((bindings-to-set (bindings)
           (sort (copy-list bindings) #'string< :key #'car))) ; Sort bindings for order-insensitive comparison
    (let ((actual-set (bindings-to-set actual))
          (expected-set (bindings-to-set expected)))
      (if (equalp actual-set expected-set)
          (format t "~A: PASS~%" test-name)
          (format t "~A: FAIL - Expected Bindings:~%  ~S~%Actual Bindings:~%  ~S~%" test-name expected actual)))))


;;; 4. Run all tests ;;;
(run-pattern-match-tests)
(run-substitute-bindings-tests)

(print con_r-rule)
(print incon_l-rule)
(print duality_r_dualR-rule)
(print duality_l_dualL-rule)
(print hypersequent_independence_r_indepR-rule)
(print hypersequent_independence_l_indepL-rule)
(print dependence_r_depR-rule)
(print dependence_l_depL-rule)

;;;; Unit Tests for Entails Function ;;;;

(defun run-entails-tests ()
  "Runs a suite of unit tests for the `entails` function."
  (test-description "Entails Function Tests")
  (test-entails-axiom-con-r)
  (test-entails-axiom-incon-l)
  (test-entails-duality-r)
  (test-entails-duality-l)
  (test-entails-independence-r-premise-true-a)
  (test-entails-independence-r-premise-true-b)
  (test-entails-independence-l-premise-true-a)
  (test-entails-independence-l-premise-true-b)
  (test-entails-dependence-r-premise-true)
  (test-entails-dependence-l-premise-true)
  (test-entails-non-entailment)
  ;; New tests for variable substitution in Indep/Dep rules
  (test-entails-independence-r-variable-a)
  (test-entails-independence-r-variable-b)
  (test-entails-independence-l-variable-a)
  (test-entails-independence-l-variable-b)
  (test-entails-dependence-r-variable)
  (test-entails-dependence-l-variable))


(defun test-entails-axiom-con-r ()
  "Tests if `entails` correctly identifies the con_R axiom."
  (let ((sequent '(() con)))
    (assert-entails-equal (entails sequent) t "Entails Axiom con_R Test Failed")))

(defun test-entails-axiom-incon-l ()
  "Tests if `entails` correctly identifies the incon_L axiom."
  (let ((sequent '((incon) ())))
    (assert-entails-equal (entails sequent) t "Entails Axiom incon_L Test Failed")))

(defun test-entails-duality-r ()
  "Tests the dualR rule. Expects failure as there's no base case for 'foo'."
  (let ((sequent '((foo) ())))  ; Assume 'foo' is not provable/refutable by axioms alone
    (assert-entails-equal (entails sequent) nil "Entails Duality Right Rule (dualR) Test (Start) - Should be nil before rule application")
    (assert-entails-equal (entails '(() (dual foo))) nil "Entails Duality Right Rule (dualR) Test (End) -  Still nil as no base case for 'foo'"))) ; Should remain nil as we have no base case for 'foo'

(defun test-entails-duality-l ()
  "Tests the dualL rule. Expects failure as there's no base case for 'bar'."
  (let ((sequent '(() (dual bar)))) ; Assume 'bar' is not provable/refutable by axioms alone
    (assert-entails-equal (entails sequent) nil "Entails Duality Left Rule (dualL) Test (Start) - Should be nil before rule application")
    (assert-entails-equal (entails '((bar) ())) nil "Entails Duality Left Rule (dualL) Test (End) - Still nil as no base case for 'bar'"))) ; Should remain nil as we have no base case for 'bar'


(defun test-entails-independence-r-premise-true-a ()
  "Tests the indepR rule where the first premise is provable."
  (let ((sequent '(() (indep con baz))))  ; 'con' is provable
      (assert-entails-equal (entails sequent) nil "Entails Independence Right Rule (indepR) Test (Premise A - Start) - Should be nil before rule application")
      (assert-entails-equal (entails '(() con)) t "Premise (entails con) should be true")
      (assert-entails-equal (entails sequent) t "Entails Independence Right Rule (indepR) Test (Premise A - End)"))) ; Should now pass as indepR checks premise

(defun test-entails-independence-r-premise-true-b ()
  "Tests the indepR rule where the second premise is provable."
  (let ((sequent '(() (indep baz con))))  ; 'con' is provable, order swapped
      (assert-entails-equal (entails sequent) nil "Entails Independence Right Rule (indepR) Test (Premise B - Start) - Should be nil before rule application")
      (assert-entails-equal (entails '(() con)) t "Premise (entails con) should be true")
      (assert-entails-equal (entails sequent) t "Entails Independence Right Rule (indepR) Test (Premise B - End)"))) ; Should now pass

(defun test-entails-independence-l-premise-true-a ()
  "Tests the indepL rule where the first premise is refutable."
  (let ((sequent '(((indep incon baz) ()))))  ; 'incon' is refutable
      (assert-entails-equal (entails sequent) nil "Entails Independence Left Rule (indepL) Test (Premise A - Start) - Should be nil before rule application")
      (assert-entails-equal (entails '((incon) ())) t "Premise (incon entails) should be true")
      (assert-entails-equal (entails sequent) t "Entails Independence Left Rule (indepL) Test (Premise A - End)"))) ; Should now pass

(defun test-entails-independence-l-premise-true-b ()
  "Tests the indepL rule where the second premise is refutable."
  (let ((sequent '(((indep baz incon) ()))))  ; 'incon' is refutable, order swapped
      (assert-entails-equal (entails sequent) nil "Entails Independence Left Rule (indepL) Test (Premise B - Start) - Should be nil before rule application")
      (assert-entails-equal (entails '((incon) ())) t "Premise (incon entails) should be true")
      (assert-entails-equal (entails sequent) t "Entails Independence Left Rule (indepL) Test (Premise B - End)"))) ; Should now pass


(defun test-entails-dependence-r-premise-true ()
  "Tests the depR rule where both premises are provable."
  (let ((sequent '(() (dep con con)))) ; Both premises are 'con', which is provable
      (assert-entails-equal (entails sequent) nil "Entails Dependence Right Rule (depR) Test (Start) - Should be nil before rule application")
      (assert-entails-equal (entails '(() con)) t "Premise (entails con) should be true")
      (assert-entails-equal (entails '(() con)) t "Premise (entails con) should be true (again)") ; Added to ensure both premises are checked
      (assert-entails-equal (entails sequent) t "Entails Dependence Right Rule (depR) Test (End)"))) ; Should now pass

(defun test-entails-dependence-l-premise-true ()
  "Tests the depL rule where both premises are refutable."
  (let ((sequent '(((dep incon incon) ())))) ; Both premises are 'incon', which is refutable
      (assert-entails-equal (entails sequent) nil "Entails Dependence Left Rule (depL) Test (Start) - Should be nil before rule application")
      (assert-entails-equal (entails '((incon) ())) t "Premise (incon entails) should be true")
      (assert-entails-equal (entails '((incon) ())) t "Premise (incon entails) should be true (again)") ; Added to ensure both premises are checked
      (assert-entails-equal (entails sequent) t "Entails Dependence Left Rule (depL) Test (End)"))) ; Should now pass


(defun test-entails-non-entailment ()
  "Tests that an unprovable sequent correctly returns NIL."
  (let ((sequent '(() foo)))  ; 'foo' is not provable by axioms or rules
    (assert-entails-equal (entails sequent) nil "Entails Non-Entailment Test Failed")))


;;; 5. New tests for variable substitution in Indep/Dep rules ;;;

(defun test-entails-independence-r-variable-a ()
  "Tests the indepR rule with a variable in the first premise position."
  (let ((sequent '(() (indep ?x baz)))
        (bindings '((?x . con))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Independence Right Rule (indepR) Variable A Test")))

(defun test-entails-independence-r-variable-b ()
  "Tests the indepR rule with a variable in the second premise position."
  (let ((sequent '(() (indep baz ?y)))
        (bindings '((?y . con))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Independence Right Rule (indepR) Variable B Test")))

(defun test-entails-independence-l-variable-a ()
  "Tests the indepL rule with a variable in the first premise position."
  (let ((sequent '(((indep ?x baz) ())))
        (bindings '((?x . incon))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Independence Left Rule (indepL) Variable A Test")))

(defun test-entails-independence-l-variable-b ()
  "Tests the indepL rule with a variable in the second premise position."
  (let ((sequent '(((indep baz ?y) ())))
        (bindings '((?y . incon))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Independence Left Rule (indepL) Variable B Test")))

(defun test-entails-dependence-r-variable ()
  "Tests the depR rule with variables."
  (let ((sequent '(() (dep ?x ?x))) ; Testing with the same variable for both positions
        (bindings '((?x . con))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Dependence Right Rule (depR) Variable Test")))

(defun test-entails-dependence-l-variable ()
  "Tests the depL rule with variables."
  (let ((sequent '(((dep ?x ?x) ())))  ; Testing with the same variable for both positions
        (bindings '((?x . incon))))
    (assert-entails-equal (entails (substitute-bindings sequent bindings)) t "Entails Dependence Left Rule (depL) Variable Test")))


;;; 6. Assertion Helper Function for Entails Tests ;;;

(defun assert-entails-equal (actual expected test-name)
  "A simple assertion helper for the `entails` function tests."
  (if (equal actual expected)
      (format t "~A: PASS~%" test-name)
      (format t "~A: FAIL - Expected:~%  ~S~%Actual:~%  ~S~%" test-name expected actual)))


;;; 7. Run Entails Tests ;;;
(run-entails-tests)