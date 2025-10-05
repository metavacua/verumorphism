(defstruct rule
  "Represents an axiom or inference rule in the hypersequent calculus.

Slots:
  - NAME: The symbolic name of the rule (e.g., 'con_R).
  - TYPE: The type of rule, e.g., :axiom, :sequent-rule, :hypersequent-rule.
  - CALCULUS-TYPE: The specific calculus this rule belongs to.
  - OPERATOR: The logical operator this rule applies to (e.g., :con, :dep).
  - DIRECTION: The direction of the rule application (:left or :right), if applicable.
  - INPUT-SEQUENT: The pattern for the input sequent(s) that this rule matches.
  - OUTPUT-SEQUENT: The pattern for the sequent produced by this rule.
  - PREMISE-CONDITION: A lisp form that is evaluated to check if the premises for the rule hold.
  - DESCRIPTION: A human-readable description of the rule."
  name
  type
  calculus-type
  operator
  direction
  input-sequent
  output-sequent
  premise-condition
  description)

(defparameter con_r-rule
  (make-rule
   :name 'con_R
   :type :axiom
   :calculus-type :non-contextual-minimal-hypersequent-calculus
   :operator :con
   :output-sequent '(() con)
   :description "Axiom: Provability of consistency (con). Output is minimal sequent (entails con), represented as (() con)."))

(defparameter incon_l-rule
  (make-rule
   :name 'incon_L
   :type :axiom
   :calculus-type :non-contextual-minimal-hypersequent-calculus
   :operator :incon
   :output-sequent '((incon) ())
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
   :description "Duality Right Rule (dualR): Transforms minimal sequent (A entails), represented as ((?A) ()), to minimal sequent (entails dual A), represented as (() (dual ?A)). Correctly moves formula across turnstile. Uses 'dual' for duality operator in code."))

(defparameter duality_l_dualL-rule
  (make-rule
   :name 'NonContextual_duality_L_dualL
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :duality
   :direction :left
   :input-sequent '(() (dual ?A))
   :output-sequent '((?A) ())
   :description "Duality Left Rule (dualL): Transforms minimal sequent (entails dual A), represented as (() (dual ?A)), to minimal sequent (A entails), represented as ((?A) ()). Uses 'dual' for duality operator in code."))

(defparameter hypersequent_independence_r_indepR-rule
  (make-rule
   :name 'Hypersequent_independence_R_indepR
   :type :hypersequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :independence
   :direction :right
   :input-sequent '(() (indep ?A ?B))  ; Input sequent for indepR rule
   :output-sequent '(() (indep ?A ?B))  ; Output sequent is the same as input in indepR - corrected input and output
   :premise-condition '(or (entails (() ?A)) (entails (() ?B)))
   :description "Hypersequent Independence Right Rule (indepR): From premise (entails A) or (entails B), infer minimal sequent (entails indep A B). Uses 'indep' for independence operator in code."))

(defparameter hypersequent_independence_l_indepL-rule
  (make-rule
   :name 'Hypersequent_independence_L_indepL
   :type :hypersequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :independence
   :direction :left
   :input-sequent '(((indep ?A ?B) ()))  ; Input sequent for indepL rule
   :output-sequent '(((indep ?A ?B) ()))  ; Output sequent same as input in indepL - corrected input and output
   :premise-condition '(or (entails (?A ())) (entails (?B ())))
   :description "Hypersequent Independence Left Rule (indepL): From premise (A entails) or (B entails), infer minimal sequent (indep A B entails). Uses 'indep' for independence operator in code."))

(defparameter dependence_r_depR-rule
  (make-rule
   :name 'Dependence_R_depR
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :dependence
   :direction :right
   :input-sequent '(() (dep ?A ?B))  ; Input sequent for depR rule
   :output-sequent '(() (dep ?A ?B))  ; Output sequent same as input for depR - corrected input and output
   :premise-condition '(and (entails (() ?A)) (entails (() ?B)))
   :description "Dependence Right Rule (depR): From premise (entails A) and (entails B), infer minimal sequent (entails dep A B). Uses 'dep' for dependence operator in code."))

(defparameter dependence_l_depL-rule
  (make-rule
   :name 'Dependence_L_depL
   :type :sequent-rule
   :calculus_type :non-contextual-minimal-hypersequent-calculus
   :operator :dependence
   :direction :left
   :input-sequent '(((dep ?A ?B) ()))  ; Input sequent for depL rule
   :output-sequent '(((dep ?A ?B) ()))  ; Output sequent same as input for depL - corrected input and output
   :premise-condition '(and (entails (?A ())) (entails (?B ())))
   :description "Dependence Left Rule (depL): From premise (A entails) and (B entails), infer minimal sequent (dep A B entails). Uses 'dep' for dependence operator in code."))

(print con_r-rule)
(print incon_l-rule)
(print duality_r_dualR-rule)
(print duality_l_dualL-rule)
(print hypersequent_independence_r_indepR-rule)
(print hypersequent_independence_l_indepL-rule)
(print dependence_r_depR-rule)
(print dependence_l_depL-rule)

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
  (format t "Entails called with sequent: ~S~%" sequent)  ; Debugging print

  ;; 1. Check Axioms (con_R and incon_l)
  (if (sequent-equal sequent (rule-output-sequent con_r-rule))
      (progn
        (format t "  Matched axiom: con_r~%")  ; Debugging print
        (return-from entails t)))

  (if (sequent-equal sequent (rule-output-sequent incon_l-rule))
      (progn
        (format t "  Matched axiom: incon_l~%")  ; Debugging print
        (return-from entails t)))

  ;; 2. Try Duality Rules (duality_r_dualR and duality_l_dualL) - Right Rule First
  (let ((bindings (pattern-match (rule-input-sequent duality_r_dualR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: duality_r_dualR with bindings: ~S~%" bindings)  ; Debugging print
          (let ((A (substitute-bindings '?A bindings))) ; Extract A from bindings
            (format t "  Extracted A: ~S~%" A)  ; Debugging print
            (let ((premise-sequent (list (list A) nil))) ; Construct premise sequent ((A) ())
              (format t "  Premise sequent for duality_r_dualR: ~S~%" premise-sequent)  ; Debugging print
              (if (entails premise-sequent)  ; Recursive call with premise
                  (let ((output-sequent (substitute-bindings (rule-output-sequent duality_r_dualR-rule) bindings)))
                    (format t "  Applying rule, output sequent: ~S~%" output-sequent)  ; Debugging print
                    (return-from entails t))))))))

  ;; Duality Left Rule
  (let ((bindings (pattern-match (rule-input-sequent duality_l_dualL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: duality_l_dualL with bindings: ~S~%" bindings)  ; Debugging print
          (let ((A (substitute-bindings '?A bindings)))  ; Extract A from bindings
            (format t "  Extracted A: ~S~%" A)  ; Debugging print
            (let ((premise-sequent (list nil (list A))))  ; Construct premise sequent (() (A))
              (format t "  Premise sequent for duality_l_dualL: ~S~%" premise-sequent)  ; Debugging print
              (if (entails premise-sequent)  ; Recursive call with premise
                  (let ((output-sequent (substitute-bindings (rule-output-sequent duality_l_dualL-rule) bindings)))
                    (format t "  Applying rule, output sequent: ~S~%" output-sequent)  ; Debugging print
                    (return-from entails t))))))))

  ;; 3. Try Independence Right Rule (indep_r_indepR)
  (let ((bindings (pattern-match (rule-input-sequent hypersequent_independence_r_indepR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: hypersequent_independence_r_indepR with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (eval (substitute-bindings (rule-premise-condition hypersequent_independence_r_indepR-rule) bindings))))
            (format t "  Premise condition for indep_r_indepR: ~S, Condition Value: ~S~%"
                    (rule-premise-condition hypersequent_independence_r_indepR-rule) premise-condition-met)
            (if premise-condition-met
                (let ((output-sequent (substitute-bindings (rule-output-sequent hypersequent_independence_r_indepR-rule) bindings)))
                  (format t "  Applying rule, output sequent: ~S~%" output-sequent)
                  (return-from entails t)))))))

  ;; 4. Try Independence Left Rule (indep_l_indepL)
  (let ((bindings (pattern-match (rule-input-sequent hypersequent_independence_l_indepL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: hypersequent_independence_l_indepL with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (eval (substitute-bindings (rule-premise-condition hypersequent_independence_l_indepL-rule) bindings))))
            (format t "  Premise condition for indep_l_indepL: ~S, Condition Value: ~S~%"
                    (rule-premise-condition hypersequent_independence_l_indepL-rule) premise-condition-met)
            (if premise-condition-met
                (let ((output-sequent (substitute-bindings (rule-output-sequent hypersequent_independence_l_indepL-rule) bindings)))
                  (format t "  Applying rule, output sequent: ~S~%" output-sequent)
                  (return-from entails t)))))))

  ;; 5. Try Dependence Right Rule (dep_r_depR)
  (let ((bindings (pattern-match (rule-input-sequent dependence_r_depR-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: dependence_r_depR with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (eval (substitute-bindings (rule-premise-condition dependence_r_depR-rule) bindings))))
            (format t "  Premise condition for dep_r_depR: ~S, Condition Value: ~S~%"
                    (rule-premise-condition dependence_r_depR-rule) premise-condition-met)
            (if premise-condition-met
                (let ((output-sequent (substitute-bindings (rule-output-sequent dependence_r_depR-rule) bindings)))
                  (format t "  Applying rule, output sequent: ~S~%" output-sequent)
                  (return-from entails t)))))))


  ;; 6. Try Dependence Left Rule (dep_l_depL)
  (let ((bindings (pattern-match (rule-input-sequent dependence_l_depL-rule) sequent nil)))
    (if bindings
        (progn
          (format t "  Matched rule: dependence_l_depL with bindings: ~S~%" bindings)
          (let ((premise-condition-met
                 (eval (substitute-bindings (rule-premise-condition dependence_l_depL-rule) bindings))))
            (format t "  Premise condition for dep_l_depL: ~S, Condition Value: ~S~%"
                    (rule-premise-condition dependence_l_depL-rule) premise-condition-met)
            (if premise-condition-met
                (let ((output-sequent (substitute-bindings (rule-output-sequent dependence_l_depL-rule) bindings)))
                  (format t "  Applying rule, output sequent: ~S~%" output-sequent)
                  (if (entails output-sequent)  ; Recursive call
                      (return-from entails t))))))))


  ;; 7. No rule applied or proof not found
  (format t "  No rule applied or proof not found for: ~S~%" sequent)  ; Debugging print
  nil)


;; Function to check if a symbol is a variable (starts with '?') - No change
(defun variablep (symbol)
  "Checks if a given symbol is a variable.
Variables are denoted by a '?' prefix, e.g., ?A."
  (and (symbolp symbol)
       (char= (char (symbol-name symbol) 0) #\?)))

;; Function to match patterns (basic version - needs more robustness later) - No change
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
  (cond ((equal pattern input) bindings)
        ((variablep pattern) (if (assoc pattern bindings)
                                 (if (equal (cdr (assoc pattern bindings)) input) bindings nil)
                                 (acons pattern input bindings)))
        ((and (consp pattern) (consp input)
              (pattern-match (car pattern) (car input) bindings))
         (pattern-match (cdr pattern) (cdr input) (pattern-match (car pattern) (car input) bindings)))
        (t nil)))


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
  (test-pattern-match-variable-order-recursive))  ; Added test for variable order in recursive match

(defun test-description (description)
  "Prints a formatted header for a test section."
  (format t ";;;; ~A ;;;;~%" description))

(defun test-pattern-match-exact-match ()
  "Tests if `pattern-match` succeeds with an empty binding list for identical inputs."
  (let ((pattern '(a b c))
        (input '(a b c)))
    (assert-equal (pattern-match pattern input nil) nil "Exact Match Test Failed")))

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
  (let ((pattern '((?y) b (?x)))  ; Variables in different order than input
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
  (if (equalp actual expected) ; Using equalp for bindings comparison
      (format t "~A: PASS~%" test-name)
      (format t "~A: FAIL - Expected Bindings:~%  ~S~%Actual Bindings:~%  ~S~%" test-name expected actual)))


;;; 4. Run all tests ;;;
(run-pattern-match-tests)
(run-substitute-bindings-tests)