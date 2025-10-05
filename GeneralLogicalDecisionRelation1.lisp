(defpackage :non-classical-relations-methodology
  (:use #:cl)
  (:documentation "This package provides a conceptual framework and methodology for exploring non-classical logical relations. It uses examples from Bell's theorem and the Liar Paradox to demonstrate concepts like non-locality, context-dependence, and non-singular outcomes in logical systems.")
  (:export #:decision-procedure
           #:liar-paradox-relation
           #:bell-scenario-relation
           #:tarskian-relation
           #:bell-scenario-experiment
           #:liar-paradox-experiment
           #:tarskian-consequence-example
           #:non-classical-logical-relations-methodology
           #:refutation-bell-scenario-experiment
           #:refutation-liar-paradox-experiment
           #:main
           #:n-m-relation
           #:calculate-context-effect
           #:calculate-consistency
           #:calculate-dependence
           #:test-non-transitivity
           #:generalized-n-m-relation))

(in-package :non-classical-relations-methodology)

;; ------------------- Utility Functions -------------------

(defun check-reflexivity (relation elements interpretation &key (reflexive-threshold 1))
  "Checks if a given binary relation is reflexive for a set of elements.
A relation is reflexive if every element is related to itself. This function
can use a threshold for graded (non-binary) relations.

Parameters:
  - RELATION: A function representing the logical relation. It should accept
    two elements and an interpretation keyword.
  - ELEMENTS: A list of elements to check for reflexivity.
  - INTERPRETATION: A keyword symbol representing the context or interpretation
    under which the relation is being evaluated.
  - REFLEXIVE-THRESHOLD (Keyword, Optional): A numeric value. For a relation to
    be considered reflexive, its output must be a number greater than or equal
    to this threshold. Defaults to 1.

Returns:
  - T if the relation is reflexive for all elements in the set.
  - NIL if any element is not related to itself according to the threshold."
  (format t "Reflexive under ~A interpretation? (Threshold: ~A) ~%" interpretation reflexive-threshold)
  (loop for element in elements do
    (let ((relatedness (funcall relation element element interpretation)))
      (format t "~A is related to itself: ~A~%" element relatedness)
      (cond
        ((and (numberp relatedness) (< relatedness reflexive-threshold))
         (return-from check-reflexivity nil)) ; Non-reflexive if relatedness is below threshold
        ((and (not (numberp relatedness)) (null relatedness))
         (return-from check-reflexivity nil))  ; Non-reflexive if relatedness is NIL (non-numeric)
        (t ;; Otherwise, consider it reflexive
         ))))
  t) ; If loop completes without returning nil, relation is reflexive


;; ------------------- Logical Relations -------------------

(defun tarskian-relation (premise conclusion interpretation)
  "Represents a simplified Tarskian consequence relation.
In this model, a statement is always considered a consequence of itself,
returning a maximal degree of relatedness (1.0) to demonstrate reflexivity.
This serves as a baseline for a classical, local relation.

Parameters:
  - PREMISE: The premise statement (ignored).
  - CONCLUSION: The conclusion statement (ignored).
  - INTERPRETATION: The context for the relation (ignored).

Returns:
  - 1.0, representing a maximal degree of relatedness."
  (declare (ignore premise conclusion interpretation))
  1.0)


(defun bell-scenario-relation (event1 event2 interpretation)
  "Models a non-local relation inspired by Bell's theorem.
The relatedness of two events is dependent on the physical interpretation
(classical or quantum), demonstrating context-dependence.

Parameters:
  - EVENT1: The first event (ignored).
  - EVENT2: The second event (ignored).
  - INTERPRETATION: A keyword, either `:classical-interpretation` or
    `:quantum-interpretation`.

Returns:
  - 0.0 for the classical interpretation (non-reflexive).
  - 1.0 for the quantum interpretation (reflexive)."
  (declare (ignore event1 event2))
  (case interpretation
    (:classical-interpretation
     0.0) ; Non-reflexive in classical context
    (:quantum-interpretation
     1.0) ; Reflexive in quantum context
    (otherwise
     (error "Unknown interpretation: ~A. Use :classical-interpretation or :quantum-interpretation" interpretation))))


(defun liar-paradox-relation (statement1 statement2 interpretation)
  "Models a non-local, self-referential relation inspired by the Liar Paradox.
The degree of relatedness of a statement to itself depends on the chosen
logical interpretation (e.g., classical, non-classical, paraconsistent).
Some interpretations return vector results to represent context-dependent outcomes.

Parameters:
  - STATEMENT1: The first statement.
  - STATEMENT2: The second statement (ignored).
  - INTERPRETATION: A keyword specifying the logical framework.

Returns:
  - A numeric value or a vector representing the degree of relatedness."
  (declare (ignore statement1 statement2))
  (case interpretation
    (:classical-interpretation
     0.0) ; Contradiction in classical logic
    (:non-classical-interpretation
     0.8) ; High relatedness in non-classical context
    (:paraconsistent-interpretation
     1.0) ; Full relatedness in paraconsistent logic
    (:dialetheist-interpretation
     1.0) ; Full relatedness in dialetheist logic
    (:contextual-interpretation-1
     (if (eq statement1 :statement-p) (vector 0.7) (vector 0.0))) ; Contextual reflexivity for statement-p
    (:contextual-interpretation-2
     (if (eq statement1 :statement-q) (vector 0.7) (vector 0.0))) ; Contextual reflexivity for statement-q
    (otherwise
     (error "Unknown interpretation: ~A. Use :classical-interpretation, :non-classical-interpretation, :paraconsistent-interpretation, :dialetheist-interpretation, :contextual-interpretation-1, :contextual-interpretation-2" interpretation))))


;; ------------------- Generalized Decision Procedure -------------------

(defun decision-procedure (statement relation interpretations)
  "A generalized procedure to evaluate a statement's relation to itself.
This function iterates through multiple logical interpretations for a given
relation, demonstrating how the 'decision' or outcome for a statement can be
non-singular and vary with context.

Parameters:
  - STATEMENT: The statement being evaluated.
  - RELATION: The logical relation function to apply.
  - INTERPRETATIONS: A list of interpretation keywords to test.

Side Effects:
  - Prints the evaluation results for each interpretation to standard output."
  (format t "--- Generalized Decision Procedure ---~%")
  (format t "Statement: ~A~%" statement)
  (format t "Using Relation: ~A~%" relation)
  (format t "Evaluating under interpretations: ~A~%" interpretations)
  (loop for interpretation in interpretations do
    (let ((interpretation-result (funcall relation statement statement interpretation)))
      (format t "Decision under ~A Interpretation: ~A ~%" interpretation interpretation-result)))

  (format t "~%--- Non-Singular Outcome Analysis ---~%")
  (format t "Interpretation-Dependent Decisions: Decisions vary across interpretations.~%")
  (format t "Generalized logical relations offer interpretation-dependent outcomes.~%")
  )


;; ------------------- Experiment Functions -------------------

(defun bell-scenario-experiment ()
  "Conducts a computational experiment on the `bell-scenario-relation`.
It tests the hypotheses that the relation is non-reflexive under a classical
interpretation and reflexive under a quantum interpretation, printing the
outcomes.

Side Effects:
  - Prints the experimental setup, hypotheses, and outcomes."
  (format t "--- Bell Scenario Experiment (Confirmation) ---~%")
  (format t "Relation: Bell Scenario Relation (Non-Local)~%")
  (let ((events '(:event-a :event-b)))

    ;; Hypothesis: Non-Reflexivity under Classical Interpretation
    (format t "~%--- Hypothesis Test: H_Bell - Non-Reflexivity under Classical Interpretation ---~%")
    (format t "Hypothesis (H_Bell): Classical interpretation -> bell-scenario-relation is NOT reflexive (threshold 0.5).~%")
    (format t "Null Hypothesis (H_Bell'): Classical interpretation -> bell-scenario-relation IS reflexive (threshold 0.5).~%")
    (let ((is-reflexive-classical (check-reflexivity #'bell-scenario-relation events :classical-interpretation :reflexive-threshold 0.5)))
      (if is-reflexive-classical
          (format t "Outcome: Experiment REFUTES H_Bell, FAILS to refute H_Bell'. (Unexpected Classical Reflexivity)~%")
          (format t "Outcome: Experiment FAILS to refute H_Bell, REFUTES H_Bell'. (Expected Non-Reflexivity under Classical Interpretation)~%")))

    ;; Hypothesis: Reflexivity under Quantum Interpretation
    (format t "~%--- Hypothesis Test: H_Bell_Quantum - Reflexivity under Quantum Interpretation ---~%")
    (format t "Hypothesis (H_Bell_Quantum): Quantum interpretation -> bell-scenario-relation IS reflexive (threshold 0.5).~%")
    (format t "Null Hypothesis (H_Bell_Quantum'): Quantum interpretation -> bell-scenario-relation is NOT reflexive (threshold 0.5).~%")
    (let ((is-reflexive-quantum (check-reflexivity #'bell-scenario-relation events :quantum-interpretation :reflexive-threshold 0.5)))
      (if is-reflexive-quantum
          (format t "Outcome: Experiment FAILS to refute H_Bell_Quantum, REFUTES H_Bell_Quantum'. (Expected Reflexivity under Quantum Interpretation)~%")
          (format t "Outcome: Experiment REFUTES H_Bell_Quantum, FAILS to refute H_Bell_Quantum'. (Unexpected Non-Reflexivity under Quantum Interpretation)~%"))))

  (format t "--- End Bell Scenario Experiment (Confirmation) ---~%"))


(defun liar-paradox-experiment ()
  "Conducts a computational experiment on the `liar-paradox-relation`.
It tests the hypotheses that the relation is reflexive under a non-classical
interpretation and non-reflexive under a classical one.

Side Effects:
  - Prints the experimental setup, hypotheses, and outcomes."
  (format t "--- Liar Paradox Experiment (Confirmation) ---~%")
  (format t "Relation: Liar Paradox Relation (Non-Local, Self-Referential)~%")
  (let ((statements '(:statement-p :statement-q)))

    ;; Hypothesis: Reflexivity under Non-Classical Interpretation
    (format t "~%--- Hypothesis Test: H_Liar_NonClassical - Reflexivity under Non-Classical Interpretation ---~%")
    (format t "Hypothesis (H_Liar_NonClassical): Non-classical interpretation -> liar-paradox-relation IS reflexive (threshold 0.5).~%")
    (format t "Null Hypothesis (H_Liar_NonClassical'): Non-classical interpretation -> liar-paradox-relation is NOT reflexive (threshold 0.5).~%")
    (let ((is-reflexive-non-classical (check-reflexivity #'liar-paradox-relation statements :non-classical-interpretation :reflexive-threshold 0.5)))
      (if is-reflexive-non-classical
          (format t "Outcome: Experiment FAILS to refute H_Liar_NonClassical, REFUTES H_Liar_NonClassical'. (Expected Reflexivity under Non-Classical Interpretation)~%")
          (format t "Outcome: Experiment REFUTES H_Liar_NonClassical, FAILS to refute H_Liar_NonClassical'. (Unexpected Non-Reflexivity under Non-Classical Interpretation)~%")))

    ;; Hypothesis: Non-Reflexivity under Classical Interpretation
    (format t "~%--- Hypothesis Test: H_Liar_Classical - Non-Reflexivity under Classical Interpretation ---~%")
    (format t "Hypothesis (H_Liar_Classical): Classical interpretation -> liar-paradox-relation is NOT reflexive (threshold 0.5).~%")
    (format t "Null Hypothesis (H_Liar_Classical'): Classical interpretation -> liar-paradox-relation IS reflexive (threshold 0.5).~%")
    (let ((is-reflexive-classical (check-reflexivity #'liar-paradox-relation statements :classical-interpretation :reflexive-threshold 0.5)))
      (if is-reflexive-classical
          (format t "Outcome: Experiment REFUTES H_Liar_Classical, FAILS to refute H_Liar_Classical'. (Unexpected Classical Reflexivity)~%")
          (format t "Outcome: Experiment FAILS to refute H_Liar_Classical, REFUTES H_Liar_Classical'. (Expected Non-Reflexivity under Classical Interpretation)~%"))))

  (format t "--- End Liar Paradox Experiment (Confirmation) ---~%"))


(defun tarskian-consequence-example ()
  "Demonstrates the reflexive property of the `tarskian-relation`.
This serves as a baseline example of a local, classical logical relation.

Side Effects:
  - Prints the demonstration results to standard output."
  (format t "--- Tarskian Consequence Relation (Local Example) ---~%")
  (format t "Relation: Tarskian Consequence Relation (Local)~%")
  (format t "Reflexive under Classical Interpretation? (Threshold 0.5) ~%")
  (let ((propositions '(:p :q)))
    (loop for prop in propositions do
      (format t "~A is consequence of itself: ~A~%" prop (funcall #'tarskian-relation prop prop :classical-interpretation))))
  (format t "--- End Tarskian Consequence Relation (Local Example) ---~%"))



(defun refutation-bell-scenario-experiment ()
  "Conducts a refutation experiment on the Bell scenario.
It attempts to falsify the hypothesis that the `bell-scenario-relation` is
non-reflexive under classical interpretation by testing it against a modified
interpretation designed to force reflexivity.

Side Effects:
  - Prints the experimental setup, hypothesis, and outcome."
  (format t "--- Refutation Experiment: Bell Scenario ---~%")
  (format t "Attempting to refute: H_Bell - Non-Reflexivity of bell-scenario-relation under :classical-interpretation~%")
  (format t "Trying to find a :classical-interpretation where bell-scenario-relation *is* reflexive.~%")

  (format t "Relation: Bell Scenario Relation (Non-Local)~%")
  (format t "Checking for Reflexivity under a *modified* :classical-interpretation...~%")
  (let ((events '(:event-a :event-b)))
    ;; Modified Classical Interpretation: Force Reflexivity
    (let ((modified-classical-interpretation :refutation-classical-interpretation))
      (format t "Reflexive under ~A interpretation? ~%" modified-classical-interpretation)
      (let ((is-reflexive-refutation (check-reflexivity
                                        (lambda (event1 event2 interp)
                                          (declare (ignore event1 event2))
                                          (case interp
                                            (:refutation-classical-interpretation 1.0) ; Force reflexivity
                                            (:quantum-interpretation 1.0)
                                            (otherwise (bell-scenario-relation event1 event2 interp))))
                                        events modified-classical-interpretation :reflexive-threshold 0.5)))
        (if is-reflexive-refutation
            (format t "Outcome: Refutation Experiment *SUCCEEDS* in refuting H_Bell (under modified :classical-interpretation)! (Trivial refutation by interpretation modification)~%")
            (format t "Outcome: Refutation Experiment *FAILS* to refute H_Bell (under modified :classical-interpretation). (H_Bell remains robust)~%"))))))

  (format t "--- End Refutation Experiment: Bell Scenario ---~%")



(defun refutation-liar-paradox-experiment ()
  "Conducts a refutation experiment on the Liar Paradox.
It attempts to falsify the hypothesis that the `liar-paradox-relation` is
reflexive under non-classical interpretation by testing it against a modified
interpretation designed to force non-reflexivity.

Side Effects:
  - Prints the experimental setup, hypothesis, and outcome."
  (format t "--- Refutation Experiment: Liar Paradox ---~%")
  (format t "Attempting to refute: H_Liar_NonClassical - Reflexivity of liar-paradox-relation under :non-classical-interpretation~%")
  (format t "Trying to find a :non-classical-interpretation where liar-paradox-relation is *not* reflexive.~%")

  (format t "Relation: Liar Paradox Relation (Non-Local, Self-Referential)~%")
  (format t "Checking for Reflexivity under a *modified* :non-classical-interpretation...~%")
  (let ((statements '(:statement-p :statement-q)))
    ;; Modified Non-Classical Interpretation: Force Non-Reflexivity
    (let ((modified-non-classical-interpretation :refutation-non-classical-interpretation))
      (format t "Reflexive under ~A interpretation? ~%" modified-non-classical-interpretation)
      (let ((is-reflexive-refutation (check-reflexivity
                                        (lambda  (statement1 statement2 interp)
                                          (declare (ignore statement1 statement2))
                                          (case interp
                                            (:refutation-non-classical-interpretation 0.0) ; Force non-reflexivity
                                            (:non-classical-interpretation 0.8)
                                            (:classical-interpretation 0.0)
                                            (:paraconsistent-interpretation 1.0)
                                            (:dialetheist-interpretation 1.0)
                                            (:contextual-interpretation-1 (if (eq statement1 :statement-p) 0.7 0.0))
                                            (:contextual-interpretation-2 (if (eq statement1 :statement-q) 0.7 0.0))
                                            (otherwise (liar-paradox-relation statement1 statement2 interp))))
                                        statements modified-non-classical-interpretation :reflexive-threshold 0.5)))
        (if is-reflexive-refutation
            (format t "Outcome: Refutation Experiment *SUCCEEDS* in refuting H_Liar_NonClassical (under modified :non-classical-interpretation)! (Trivial refutation by interpretation modification)~%")
            (format t "Outcome: Refutation Experiment *FAILS* to refute H_Liar_NonClassical (under modified :non-classical-interpretation). (H_Liar_NonClassical remains robust)~%"))))))

  (format t "--- End Refutation Experiment: Liar Paradox ---~%")



(defun non-classical-logical-relations-methodology ()
  "Prints a summary of the proposed methodology for investigating non-classical
logical relations. This methodology involves hypothesizing, defining,
theoremizing, experimenting, analyzing, and refining theories about such
relations.

Side Effects:
  - Prints a multi-step methodology to standard output."
  (format t "--- Non-Classical Logical Relations Methodology ---~%")
  (format t "Methodology based on Generalized Logical Relations and Non-Locality:~%")
  (format t "- Hypothesize: Contrast classical vs. non-classical relations, graded relatedness, matrix/vector spaces.~%")
  (format t "- Define: Define local/non-local relations, reflexivity, symmetry, transitivity, context-dependence, graded relatedness, n-to-m relations, matrix/vector spaces.~%")
  (format t "- Theoremize: Derive theorems on non-locality impact on Tarskian properties, non-singular outcomes, algebraic properties.~%")
  (format t "- Experiment: Test hypotheses/theorems via paradoxes, decision procedures, refutation experiments, vector spaces, non-algebraic properties.~%")
  (format t "- Analyze Outcomes: Interpretation-dependent decisions, non-singular outcomes, refutation robustness, vector spaces, graded relatedness, algebraic properties.~%")
  (format t "- Refine Theory: Refine theory based on experiments, considering graded relatedness, refutation outcomes, mathematical representations, non-algebraic behavior.~%")
  (format t "--- End Non-Classical Logical Relations Methodology ---~%"))


;; ------------------- n-to-m Relation Functions -------------------

(defun n-m-relation (inputs interpretations)
  "A placeholder function modeling a generalized n-to-m relation.
It takes `n` inputs and produces `m` outputs (here, consistency and dependence),
with the outcome influenced by context effects derived from multiple
interpretations.

Parameters:
  - INPUTS: A list of input statements or entities.
  - INTERPRETATIONS: A list of interpretation keywords corresponding to the inputs.

Returns:
  - A vector of `m` output values, representing different dimensions of the
    relation's outcome (e.g., consistency, dependence)."
  (let ((n (length inputs))
        (m 2)) ; Fixed to m=2 outputs (consistency, dependence)
    (if (< n 1)
        (error "n-m-relation requires at least one input.")
        (let ((context-effects (loop for input in inputs
                                     for interpretation in interpretations ; Interpretations for each input
                                     collect (calculate-context-effect input interpretation))))

          (let ((consistency-output (calculate-consistency context-effects))
                (dependence-output (calculate-dependence context-effects)))
            (vector consistency-output dependence-output))))))


(defun calculate-context-effect (input interpretation)
  "A placeholder function to simulate the effect of a given interpretation on an input.
This is used by `n-m-relation` to model context-sensitivity.

Parameters:
  - INPUT: The input statement or entity.
  - INTERPRETATION: The context in which the input is being evaluated.

Returns:
  - A numeric value representing the 'effect' of the context on the input."
  (case interpretation
    (:context-a (if (eq input :statement-x) 0.9 0.1)) ; Context A: High effect for X
    (:context-b (if (eq input :statement-y) 0.9 0.1)) ; Context B: High effect for Y
    (:context-c (if (eq input :statement-z) 0.9 0.1)) ; Context C: High effect for Z
    (:context-ab (if (or (eq input :statement-x) (eq input :statement-y)) 0.5 0.1)) ; Context AB: Medium effect for X, Y
    (:context-bc (if (or (eq input :statement-y) (eq input :statement-z)) 0.5 0.1)) ; Context BC: Medium effect for Y, Z
    (:context-ac (if (or (eq input :statement-x) (eq input :statement-z)) 0.8 0.1)) ; Context AC: High effect for X, Z
    (otherwise 0.3))) ; Default context: Low effect


(defun calculate-consistency (context-effects)
  "A placeholder function that calculates a 'consistency' value from context effects.
It demonstrates non-monotonicity, where a lower average effect can lead to
higher consistency, challenging classical assumptions.

Parameters:
  - CONTEXT-EFFECTS: A list of numeric values representing context effects.

Returns:
  - A numeric value representing the calculated consistency."
  (let ((average-effect (/ (reduce #'+ context-effects) (length context-effects))))
    (if (< average-effect 0.6) ; Low average effect -> high consistency
        0.9
        0.3))) ; High average effect -> low consistency


(defun calculate-dependence (context-effects)
  "A placeholder function that calculates a 'dependence' value from context effects.
It demonstrates non-monotonicity, where a high sum of effects can lead to
low dependence.

Parameters:
  - CONTEXT-EFFECTS: A list of numeric values representing context effects.

Returns:
  - A numeric value representing the calculated dependence."
  (let ((sum-effect (reduce #'+ context-effects)))
    (if (> sum-effect 2.0) ; High sum effect -> low dependence
        0.1
        0.7))) ; Low sum effect -> high dependence


(defun test-non-transitivity ()
  "Conducts an experiment to test for non-transitivity in the `n-m-relation`.
It checks if high consistency between (A,B) and (B,C) necessarily implies
high consistency between (A,C).

Side Effects:
  - Prints the experimental setup, hypotheses, and outcome."
  (format t "--- Experiment: Testing Non-Transitivity of n-m-relation (Consistency) ---~%")

  (let* ((inputs_ab '(:statement-x :statement-y))
         (interpretations_ab '(:context-ab :context-ab))
         (inputs_bc '(:statement-y :statement-z))
         (interpretations_bc '(:context-bc :context-bc))
         (inputs_ac '(:statement-x :statement-z))
         (interpretations_ac '(:context-ac :context-ac))

         (relation_ab_output (n-m-relation inputs_ab interpretations_ab))
         (relation_bc_output (n-m-relation inputs_bc interpretations_bc))
         (relation_ac_output (n-m-relation inputs_ac interpretations_ac))

         (consistency_ab (aref relation_ab_output 0))
         (consistency_bc (aref relation_bc_output 0))
         (consistency_ac (aref relation_ac_output 0)))

    (format t "Relation Output AB (Inputs: ~A, Interpretations: ~A): ~A, Consistency: ~A~%" inputs_ab interpretations_ab relation_ab_output consistency_ab)
    (format t "Relation Output BC (Inputs: ~A, Interpretations: ~A): ~A, Consistency: ~A~%" inputs_bc interpretations_bc relation_bc_output consistency_bc)
    (format t "Relation Output AC (Inputs: ~A, Interpretations: ~A): ~A, Consistency: ~A~%" inputs_ac interpretations_ac relation_ac_output consistency_ac)

    (format t "~%--- Hypothesis Test: H_Transitivity - n-m-relation Consistency IS Transitive ---~%")
    (format t "Hypothesis (H_Transitivity): High consistency(AB) AND high consistency(BC) -> high consistency(AC) (threshold 0.7).~%")
    (format t "Null Hypothesis (H_Transitivity'): n-m-relation Consistency is NOT Transitive.~%")

    (let ((high-threshold 0.7))
      (if (and (>= consistency_ab high-threshold) (>= consistency_bc high-threshold) (< consistency_ac high-threshold))
          (format t "Outcome: Experiment FAILS to refute H_Transitivity', REFUTES H_Transitivity. (Demonstrates Non-Transitivity)~%")
          (format t "Outcome: Experiment FAILS to refute H_Transitivity, REFUTES H_Transitivity'. (Fails to demonstrate Non-Transitivity)~%"))))

  (format t "--- End Experiment: Testing Non-Transitivity of n-m-relation (Consistency) ---~%"))


(defun generalized-n-m-relation (inputs interpretations logical-relation)
  "A more abstract version of `n-m-relation` that accepts a logical relation.
This allows it to model the context effects of different logical systems by
taking the logical relation itself as an argument.

Parameters:
  - INPUTS: A list of input statements.
  - INTERPRETATIONS: A list of interpretation keywords.
  - LOGICAL-RELATION: A function representing the logical relation to be used.

Returns:
  - A vector of `m` output values (e.g., consistency, dependence)."
  (let ((n (length inputs))
        (m 2))                                      ; Still m=2 outputs (consistency, dependence)
    (if (< n 1)                                     ; Check for at least one input
        (error "generalized-n-m-relation requires at least one input.")
        (if (not (functionp logical-relation))     ; Check if logical-relation is a function
            (error "generalized-n-m-relation requires a logical-relation function as input.")
            (let ((context-effects (loop for input in inputs
                                         for interpretation in interpretations ; Interpretations for each input
                                         collect (calculate-context-effect-generalized input interpretation logical-relation))))

              (let ((consistency-output (calculate-consistency context-effects))
                    (dependence-output (calculate-dependence context-effects)))
                (vector consistency-output dependence-output)))))))


(defun calculate-context-effect-generalized (input interpretation logical-relation)
  "A helper for `generalized-n-m-relation` to calculate a context effect.
It modulates a base context effect with the result of applying the provided
`logical-relation`.

Parameters:
  - INPUT: The input statement.
  - INTERPRETATION: The context for the evaluation.
  - LOGICAL-RELATION: The logical relation function to apply.

Returns:
  - A numeric value for the calculated context effect."
  (let ((base-effect (calculate-context-effect input interpretation))) ; Base context effect
    (if (and input interpretation logical-relation)
        (* base-effect (funcall logical-relation input input :classical-interpretation)) ; Modulate by logical relation
        base-effect)))


;; ------------------- Main Function -------------------

(defun main ()
  "The main entry point for the script.
Runs a series of experiments and examples to demonstrate the concepts of
non-classical logical relations defined in this file.

Side Effects:
  - Prints the results of all experiments and demonstrations."
  (format t "--- Main Execution ---\n\n")

  (bell-scenario-experiment)
  (liar-paradox-experiment)
  (tarskian-consequence-example)
  (decision-procedure :liar-statement #'liar-paradox-relation '(:classical-interpretation :non-classical-interpretation :paraconsistent-interpretation :dialetheist-interpretation :contextual-interpretation-1 :contextual-interpretation-2))
  (refutation-bell-scenario-experiment)
  (refutation-liar-paradox-experiment)
  (non-classical-logical-relations-methodology)

  (format t "\n--- n-to-m Relation Design ---~%")
  (let ((inputs '(:statement-x :statement-y :statement-z))
        (interpretations '(:context-a :context-b :context-default)))
    (format t "Evaluating n-m-relation with inputs ~A and interpretations ~A:~%" inputs interpretations)
    (let ((relation-output (n-m-relation inputs interpretations)))
      (format t "n-m-relation output: ~A~%" relation-output)))
  (format t "--- End n-to-m Relation Design ---\n\n")

  (test-non-transitivity)

  ;; --- Example of using generalized-n-m-relation ---
  (format t "\n--- Generalized n-m-relation Example ---~%")
  (let ((inputs-gen '(:statement-x :statement-y))
        (interpretations-gen '(:context-a :context-b))
        (relation-input #'bell-scenario-relation)) ; Using bell-scenario-relation as input relation
    (format t "Evaluating generalized-n-m-relation with inputs ~A, interpretations ~A, and logical-relation ~A:~%" inputs-gen interpretations-gen relation-input)
    (let ((generalized-relation-output (generalized-n-m-relation inputs-gen interpretations-gen relation-input)))
      (format t "generalized-n-m-relation output: ~A~%" generalized-relation-output)))
  (format t "--- End Generalized n-m-relation Example ---\n\n")

  (format t "--- End Main Execution ---~%"))


(main)