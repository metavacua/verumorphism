(defpackage :weaver-prover
  (:use common-lisp)
  (:export
   :prove ; Note: prove function not defined in original code
   :parse-formula
   :print-formula
   :wff-p
   :axiom-p
   :get-arity
   :make-predicate ; Export make-predicate
   :substitute-formula ; Renamed
   :variables-of
   :evaluate
   :entailment
   :left-entailment
   :right-entailment
   :print-derivation
   :derive
   :tautology-p
   :run-prover ; Export example runner
   ))

(in-package :weaver-prover)

;;; ===============================
;;; Syntax
;;; ===============================

;;; Constants
(defconstant +constPone+ 'ONE_P)
(defconstant +constPzero+ 'ZERO_P)
(defconstant +unknown+ :UNKNOWN) ; Representing an unassigned or superposition state

;;; Define implication and non-implication operators
(defconstant +impliesP+ 'IMPLIES_P)
(defconstant +notImpliesP+ 'NOT_IMPLIES_P)

;;; Function to represent a predicate. Arity is stored as a property.
(defun make-predicate (name arity)
  "Defines a predicate symbol NAME with ARITY."
  (let ((symbol (intern (symbol-name name)))) ; Ensure symbol is in current package.
    (setf (get symbol 'arity) arity)
    symbol))

(defun get-arity (predicate-symbol)
  "Retrieves the arity of a predicate symbol. Returns NIL if not a predicate."
  (get predicate-symbol 'arity))

;;; Term Predicate (Moved Before WFF-P)
(defun term-p (term)
  "Checks if an expression is a term (variable or constant symbol)."
  ;; Adjusted to check for symbols that are not defined predicates
  (and (symbolp term)
       (not (get-arity term))
       ;; We might want stricter rules, e.g., allow only specific constants or variable patterns
       ))

;;; WFF Predicate (Well-Formed Formula)
(defun wff-p (formula)
  "Checks if a given expression is a well-formed formula (WFF) in Weaver."
  (cond
    ((symbolp formula) ; Atomic formula (0-arity predicate) or constant
     (or (eq formula +constPone+)
         (eq formula +constPzero+)
         (let ((arity (get-arity formula))) ; Check if it's a 0-arity predicate
           (and arity (= arity 0)))))
    ((and (listp formula) (consp formula)) ; Ensure it's a non-empty list
     (let ((operator (first formula))
           (args (rest formula)))
       (case operator
         ((AND_P OR_P IMPLIES_P NOT_IMPLIES_P) ; Binary connectives
          (and (= (length args) 2)
               (wff-p (first args))
               (wff-p (second args))))
         ((FORALL EXISTS) ; Quantifiers
          (and (= (length args) 2)
               (symbolp (first args)) ; Variable
               (not (member (first args) `(,+constPone+ ,+constPzero+))) ; Var shouldn't be a constant
               (term-p (first args)) ; Variable must also be a term
               (wff-p (second args))))
         (otherwise ; Predicate application
          (let ((arity (get-arity operator)))
            (and arity ; Check if operator is a defined predicate
                 (= arity (length args))
                 (every #'term-p args))))))) ; Arguments must be terms
    (t nil))) ; Use lowercase 't' for the default clause

;;; ===============================
;;; Formula Parsing (Overhauled Tokenizer)
;;; ===============================

(defun tokenize (input-string)
  "Splits input string into tokens: '(', ')', or symbol/operator names."
  ;; More robust tokenizer: handles multi-char names and separates parens.
  (loop with tokens = '()
        with current-token = (make-string-output-stream)
        for char across input-string
        do (case char
             ((#\( #\)) ; Delimiters that are also tokens
              (let ((token-str (get-output-stream-string current-token)))
                (unless (string= token-str "") (push token-str tokens)))
              (push (string char) tokens)
              (setf current-token (make-string-output-stream))) ; Reset stream
             ((#\Space #\Tab #\Newline #\,) ; Delimiters ignored unless inside token
              (let ((token-str (get-output-stream-string current-token)))
                (unless (string= token-str "") (push token-str tokens)))
              (setf current-token (make-string-output-stream))) ; Reset stream
             (otherwise ; Accumulate character
              (write-char char current-token)))
        finally ; Add any trailing token
                (let ((token-str (get-output-stream-string current-token)))
                  (unless (string= token-str "") (push token-str tokens)))
                (return (reverse tokens))))

(defun parse-formula (input-string)
  "Parses a string representation (using prefix notation like '(OP A B)') into a Weaver WFF list structure."
  (let ((tokens (tokenize input-string)))
    (multiple-value-bind (result remaining-tokens)
        (parse-expression tokens)
      (when remaining-tokens
        (warn "Unexpected tokens remaining after parse: ~A" remaining-tokens))
      result)))

(defun parse-expression (tokens)
  "Parses the next expression from the token list. Returns (values expression remaining-tokens)."
  (unless tokens (error "Unexpected end of input during parsing."))
  (let ((token (pop tokens)))
    (cond
      ((string= token "(") (parse-list-expression tokens))
      ((string= token ")") (error "Unexpected ')' encountered."))
      ((string= token "ONE_P") (values +constPone+ tokens))
      ((string= token "ZERO_P") (values +constPzero+ tokens))
      ((string= token "IMPLIES_P") (values +impliesP+ tokens))
      ((string= token "NOT_IMPLIES_P") (values +notImpliesP+ tokens))
      (t (values (intern token) tokens))))) ; Assume variable, constant, or 0-arity predicate name

(defun parse-list-expression (tokens)
  "Parses contents of a list after '(' until ')'. Returns (values expression remaining-tokens)."
  (when (null tokens) (error "Unclosed list expression: missing operator or ')'.") )
  (when (string= (first tokens) ")") (error "Empty list '()' found in input."))

  (multiple-value-bind (operator-expr remaining-after-op) (parse-expression tokens)
      (unless (symbolp operator-expr)
        (error "Expected an operator symbol after '(', got: ~A" operator-expr))

    (let ((operator operator-expr)
          (args nil)
          (remaining-tokens remaining-after-op))

      ;; Determine expected arity or parse until ')'
      (cond
        ((member operator '(AND_P OR_P IMPLIES_P NOT_IMPLIES_P))
         (multiple-value-bind (arg1 rem1) (parse-expression remaining-tokens)
           (multiple-value-bind (arg2 rem2) (parse-expression rem1)
             (setf args (list arg1 arg2))
             (setf remaining-tokens rem2))))
        ((member operator '(FORALL EXISTS))
         (multiple-value-bind (var-arg rem1) (parse-expression remaining-tokens) ; Variable
           (unless (symbolp var-arg) (error "Expected variable for quantifier ~A, got ~A" operator var-arg))
           (multiple-value-bind (formula-arg rem2) (parse-expression rem1) ; Formula
             (setf args (list var-arg formula-arg))
             (setf remaining-tokens rem2))))
        (t ;; Assume predicate
           (let ((arity (get-arity operator)))
             (unless arity (error "Unknown operator or undefined predicate: ~A" operator))
             (loop repeat arity
                   do (multiple-value-bind (arg rem) (parse-expression remaining-tokens)
                        (push arg args) ; Push and reverse later
                        (setf remaining-tokens rem)))
             (setf args (reverse args))))) ; Reverse args collected via push

      ;; Check for closing parenthesis
      (unless (and remaining-tokens (string= (first remaining-tokens) ")"))
        (error "Expected ')' after args for ~A, got: ~A" operator (first remaining-tokens)))

      (pop remaining-tokens) ; Consume ')'
      (values (cons operator args) remaining-tokens))))


;;; ===============================
;;; Formula Printing
;;; ===============================

(defun print-formula (formula)
  "Prints a WFF in a readable Lisp-like prefix format."
  (cond
    ((eq formula +constPone+) "ONE_P")
    ((eq formula +constPzero+) "ZERO_P")
    ((eq formula +unknown+) "UNKNOWN")
    ((symbolp formula) (symbol-name formula)) ; Variables or 0-arity Predicates
    ((listp formula)
     (let ((operator (first formula))
           (args (rest formula)))
       (case operator
         ;; Keep Lisp-like prefix notation for clarity and consistency with parser
         ((AND_P OR_P FORALL EXISTS IMPLIES_P NOT_IMPLIES_P)
          (format nil "(~a~{ ~a~})"
                  (symbol-name operator)
                  (mapcar #'print-formula args)))
         (otherwise ; Predicate
          (if (null args)
              (symbol-name operator) ; 0-arity predicate
              (format nil "(~a~{ ~a~})"
                      (symbol-name operator)
                      (mapcar #'print-formula args)))))))
    (t (format nil "~a" formula)))) ; Should not happen for WFFs


;;; ===============================
;;; Substitution (Renamed)
;;; ===============================

(defun substitute-formula (formula variable term) ; ****** RENAMED ******
  "Substitutes a term for all free occurrences of a variable in a formula."
  (cond
    ((eq formula variable) term)
    ((symbolp formula) formula) ; Constants, other variables, 0-arity predicates
    ((listp formula)
     (let ((operator (first formula))
           (args (rest formula)))
       (case operator
         ((AND_P OR_P IMPLIES_P NOT_IMPLIES_P)
          (list operator
                (substitute-formula (first args) variable term)   ; Recursive call renamed
                (substitute-formula (second args) variable term))) ; Recursive call renamed
         ((FORALL EXISTS)
          (if (eq (first args) variable) ; Bound variable, don't substitute inside
              formula
              ;; Avoid capture: If term contains the bound variable, need alpha-conversion (complex).
              ;; Simple version assumes no capture or that term doesn't contain first args.
              (list operator
                    (first args) ; Keep the bound variable
                    (substitute-formula (second args) variable term)))) ; Recursive call renamed
         (otherwise ; Predicate application
          (cons operator (mapcar #'(lambda (arg) (substitute-formula arg variable term)) args)))))) ; Recursive call renamed
    (t formula)))

;;; ===============================
;;; Free Variables
;;; ===============================
(defun variables-of (formula)
  "Returns a list of the free variables in a formula."
  (cond
    ((symbolp formula)
     ;; Check if it's a variable or a 0-arity predicate
     (if (and (not (member formula `(,+constPone+ ,+constPzero+)))
              (or (null (get-arity formula)) ; True variables
                  (= (get-arity formula) 0))) ; 0-arity predicates
         (list formula)
         nil))
    ((listp formula)
     (let ((operator (first formula))
           (args (rest formula)))
       (case operator
         ((AND_P OR_P IMPLIES_P NOT_IMPLIES_P)
          (union (variables-of (first args)) (variables-of (second args))))
         ((FORALL EXISTS)
          ;; Variables in the subformula minus the bound variable
          (set-difference (variables-of (second args)) (list (first args))))
         (otherwise ; Predicate
          ;; Variables in the arguments (assuming arguments are terms)
          (reduce #'union (mapcar #'variables-of args) :initial-value nil)))))
    (t nil)))

;;; ===============================
;;; Axiom Predicate
;;; ===============================
(defun axiom-p (formula)
  "Checks if a formula is an axiom (currently just 1P)."
  ;; Based on the rules provided earlier, only 1P seems to be a right-hand axiom.
  ;; 0P is a left-hand axiom. Derive only handles right-hand rules.
  (eq formula +constPone+))

;;; ===============================
;;; Evaluation (Strictly Propositional)
;;; ===============================

(defun evaluate (formula assignment)
  "Evaluates a PROPOSITIONAL formula given a variable assignment (hash table)."
  (cond
    ((eq formula +constPone+) +constPone+)
    ((eq formula +constPzero+) +constPzero+)
    ((symbolp formula) ; Variable or 0-arity predicate
     (multiple-value-bind (value foundp) (gethash formula assignment)
       (unless foundp
         ;; Treat as a propositional variable if not in assignment for now
         ;; In a more complete Weaver logic, this would be 'unknown'
         (return-from evaluate formula)) ; Or handle differently based on context
       value))
    ((listp formula)
     (let ((operator (first formula))
           (args (rest formula)))
       (case operator
         ((AND_P) (if (and (eq (evaluate (first args) assignment) +constPone+)
                         (eq (evaluate (second args) assignment) +constPone+))
                      +constPone+
                      +constPzero+))
         ((OR_P)  (if (or (eq (evaluate (first args) assignment) +constPone+)
                          (eq (evaluate (second args) assignment) +constPone+))
                     +constPone+
                     +constPzero+))
         ((IMPLIES_P) (if (eq (evaluate (first args) assignment) +constPzero+)
                          +constPone+
                          (evaluate (second args) assignment)))
         ((NOT_IMPLIES_P) (if (and (eq (evaluate (first args) assignment) +constPone+)
                                   (eq (evaluate (second args) assignment) +constPzero+))
                             +constPone+
                             +constPzero+))
         ((FORALL EXISTS)
          (error "Tautology evaluation cannot handle quantifiers: ~A" formula))
         (otherwise ; Predicate application or unknown operator
          (if (get-arity operator) ; Is it a known predicate?
              (error "Tautology evaluation cannot handle predicates: ~A" formula)
              (error "Unknown operator found during evaluation: ~A" operator))))))
    (t (error "Invalid formula in evaluation: ~A" formula))))

;;; ===============================
;;; Entailment (Left and Right) - Basic Lattice-like Check
;;; ===============================
;;; NOTE: These are simplistic checks, not full logical entailment for Weaver.

(defun entailment (left right)
  "Checks if the left-hand side entails the right-hand side (left |= right). Basic version."
  (left-entailment left right))

(defun left-entailment (left right)
  "Checks left-entailment. Basic version."
  (cond
    ((eq left +constPzero+) t) ; 0_P |= anything
    ((eq right +constPone+) t) ; anything |= 1_P
    ((equal left right) t) ; Use equal for structural comparison
    ((and (listp left) (eq (first left) 'AND_P))
     ;; This rule is non-standard for logical entailment.
     (and (left-entailment (second left) right) (left-entailment (third left) right)))
    ((and (listp right) (eq (first right) 'OR_P))
     ;; This rule is more standard: A |= B or C if A |= B or A |= C
     (or (left-entailment left (second right)) (left-entailment left (third right))))
    (t nil)))

(defun right-entailment (left right)
  "Checks right entailment (left =| right). Basic version."
  ;; Keeping same as left-entailment as definition is unclear.
  (cond
    ((eq left +constPzero+) t) ; 0_P |= anything
    ((eq right +constPone+) t) ; anything |= 1_P
    ((equal left right) t) ; Use equal
    ((and (listp right) (eq (first right) 'OR_P))
     (or (right-entailment left (second right)) (right-entailment left (third right))))
    ((and (listp left) (eq (first left) 'AND_P))
     (and (right-entailment (second left) right) (right-entailment (third left) right)))
    (t nil)))

;;; ===============================
;;; Derivation and Proof Checking (Right-Hand Rules Only)
;;; ===============================

(defstruct derivation-node
  formula
  rule
  premises)

(defun derive (formula &key (max-depth 10) (assumptions nil))
  "Attempts to derive a formula using backward proof search.
  Returns a derivation tree or NIL if it cannot be derived."
  (when (<= max-depth 0)
    (warn "Max derivation depth reached for: ~A" (print-formula formula))
    (return-from derive nil))

  ;; 1. Check if the formula is an axiom
  (or
   (and (axiom-p formula)
        (make-derivation-node :formula formula :rule 'AXIOM_1P_R :premises nil))
   (derive-axiom-implies formula)
   (derive-axiom-not-implies formula)
   ;; 2. Check if the formula is in the assumptions
   (when (member formula assumptions :test #'equal)
     (make-derivation-node :formula formula :rule 'HYPOTHESIS :premises nil))
   ;; 3. Try to apply rules backward
   (derive-and-backward formula :max-depth (1- max-depth) :assumptions assumptions)
   (derive-or-backward formula :max-depth (1- max-depth) :assumptions assumptions)
   (derive-exists-backward formula :max-depth (1- max-depth) :assumptions assumptions)
   (derive-implies-backward formula :max-depth (1- max-depth) :assumptions assumptions)
   (derive-not-implies-backward formula :max-depth (1- max-depth) :assumptions assumptions)))

(defun derive-and-backward (formula &key (max-depth 10) (assumptions nil))
  (when (and (listp formula) (eq (first formula) 'AND_P) (= (length (rest formula)) 2))
    (let ((left (second formula))
          (right (third formula)))
      (let ((left-deriv (derive left :max-depth (1- max-depth) :assumptions assumptions))
            (right-deriv (derive right :max-depth (1- max-depth) :assumptions assumptions)))
        (when (and left-deriv right-deriv)
          (make-derivation-node
           :formula formula
           :rule 'AND_P_R
           :premises (list left-deriv right-deriv)))))))

(defun derive-or-backward (formula &key (max-depth 10) (assumptions nil))
  (when (and (listp formula) (eq (first formula) 'OR_P) (= (length (rest formula)) 2))
    (let ((left (second formula))
          (right (third formula)))
      (or
       (let ((left-deriv (derive left :max-depth (1- max-depth) :assumptions assumptions)))
         (when left-deriv
           (make-derivation-node :formula formula :rule 'OR_P_R :premises (list left-deriv))))
       (let ((right-deriv (derive right :max-depth (1- max-depth) :assumptions assumptions)))
         (when right-deriv
           (make-derivation-node :formula formula :rule 'OR_P_R :premises (list right-deriv))))))))

(defun derive-exists-backward (formula &key (max-depth 10) (assumptions nil))
  (when (and (listp formula) (eq (first formula) 'EXISTS) (= (length (rest formula)) 2))
    (let ((var (second formula))
          (sub-formula (third formula)))
      ;; EXISTS R_P requires finding *one* term t such that P(t) is derivable.
      ;; Still using the placeholder for finding a suitable term.
      (let ((term (find-suitable-term sub-formula var)))
        (when term
          (let ((instantiated-formula (substitute-formula sub-formula var term)))
            (let ((premise-deriv (derive instantiated-formula :max-depth (1- max-depth) :assumptions assumptions)))
              (when premise-deriv
                (make-derivation-node :formula formula :rule 'EXISTS_R_P :premises (list premise-deriv))))))))))

(defun derive-implies-backward (formula &key (max-depth 10) (assumptions nil))
  (when (and (listp formula) (eq (first formula) +impliesP+) (= (length (rest formula)) 2))
    (let ((antecedent (second formula))
          (consequent (third formula)))
      ;; Try to derive the consequent
      (let ((consequent-deriv (derive consequent :max-depth (1- max-depth) :assumptions assumptions)))
        (when consequent-deriv
          (return-from derive-implies-backward
            (make-derivation-node :formula formula :rule 'IMPLIES_R :premises (list consequent-deriv)))))
      ;; Try to derive ONE_P with the antecedent as an assumption
      (let ((assumption-deriv (derive +constPone+ :max-depth (1- max-depth) :assumptions (cons antecedent assumptions))))
        (when assumption-deriv
          (return-from derive-implies-backward
            (make-derivation-node :formula formula :rule 'IMPLIES_R :premises (list assumption-deriv))))))))

(defun derive-not-implies-backward (formula &key (max-depth 10) (assumptions nil))
  (when (and (listp formula) (eq (first formula) +notImpliesP+) (= (length (rest formula)) 2))
    (let ((antecedent (second formula))
          (consequent (third formula)))
      ;; 1. Derive the antecedent A
      (let ((antecedent-deriv (derive antecedent :max-depth (1- max-depth) :assumptions assumptions)))
        (when antecedent-deriv
          ;; 2. Try to derive ZERO_P with B as an assumption
          (let ((contradiction-deriv (derive +constPzero+ :max-depth (1- max-depth) :assumptions (cons consequent assumptions))))
            (when contradiction-deriv
              (make-derivation-node
               :formula formula
               :rule 'NOT_IMPLIES_R
               :premises (list antecedent-deriv contradiction-deriv)))))))))

(defun derive-axiom-implies (formula)
  (when (equal formula (list +impliesP+ +constPzero+ +constPone+))
    ;; Axiom Implication Right: We can always derive (0_P -> 1_P)
    (make-derivation-node :formula formula :rule 'AXIOM_IMPLIES_R :premises nil)))

(defun derive-axiom-not-implies (formula)
  (when (equal formula (list +notImpliesP+ +constPone+ +constPzero+))
    ;; Axiom Non-Implication Right: We can always derive (1_P n-> 0_P)
    (make-derivation-node :formula formula :rule 'AXIOM_NOT_IMPLIES_R :premises nil)))


(defun find-suitable-term (formula variable)
  "Placeholder: Finds a suitable term to substitute for a variable.
   Needs significant improvement for a real prover.
   Currently returns a constant."
   ;; TODO: Implement proper term finding/unification.
  (declare (ignore formula variable))
  ;; For testing, let's just try a known constant if available.
  ;; This is NOT logically sound in general.
  +constPone+) ; Just guessing a term for demonstration

;;; ===============================
;;; Proof Printing
;;; ===============================

(defun print-derivation (derivation)
  "Prints a derivation tree in a readable format."
  (labels ((print-node (node indent)
             (format t "~&~v@tFormula: ~a~%~v@tRule: ~a~%"
                     indent (print-formula (derivation-node-formula node))
                     indent (derivation-node-rule node))
             (when (derivation-node-premises node)
               (format t "~v@tPremises:~%" indent)
               (dolist (premise (derivation-node-premises node))
                 (print-node premise (+ indent 2))))))
    (if derivation
        (progn (format t "~%Derivation Tree:~%")
               (print-node derivation 2))
        (format t "~%No derivation found.~%"))))

;;; ===============================
;;; Tautology Check (Strictly Propositional - Revised: Handling Unknown)
;;; ===============================
(defun tautology-p (formula)
  "Checks if a PROPOSITIONAL formula is a tautology by evaluating under all assignments.
   Errors if non-propositional constructs are found."
  (let* ((vars-or-props (remove-duplicates (variables-of formula)))
         (assignment (make-hash-table)))

    ;; Check if formula contains non-propositional elements
    (handler-case (check-propositional formula)
      (error (c)
             (warn "Formula not purely propositional: ~a. Tautology check skipped. (~a)" (print-formula formula) c)
             (return-from tautology-p :not-propositional)))

    ;; If no propositional variables, just evaluate once
    (if (null vars-or-props)
        (eq (evaluate formula assignment) +constPone+)
        (tautology-check-recursive formula vars-or-props assignment))))

(defun check-propositional (formula)
  "Recursively checks if a formula is purely propositional. Errors if not."
  (cond
    ((symbolp formula) t) ; Vars/constants/0-arity predicates are ok at this level
    ((listp formula)
     (let ((op (first formula)) (args (rest formula)))
       (case op
         ((AND_P OR_P IMPLIES_P NOT_IMPLIES_P)
          (check-propositional (first args))
          (check-propositional (second args)))
         ((FORALL EXISTS) (error "Quantifier found"))
         (otherwise
          (if (get-arity op) ; Predicate?
              (error "Predicate found")
              (error "Unknown operator found"))))))
    (t (error "Invalid formula structure"))))


(defun tautology-check-recursive (formula variables assignment)
  "Recursive helper function for tautology-p. Assumes formula is propositional."
  (if (null variables)
      (evaluate formula assignment) ; Base case: evaluate with current assignment
      (let ((var (car variables))
            (rest-vars (cdr variables)))
        ;; Check both branches (1P and 0P) for the current variable
        (let ((result1 (progn
                         (setf (gethash var assignment) +constPone+)
                         (tautology-check-recursive formula rest-vars assignment)))
              (result0 (progn
                         (setf (gethash var assignment) +constPzero+)
                         (tautology-check-recursive formula rest-vars assignment))))
          ;; A formula is a tautology if it evaluates to ONE_P for all assignments
          (and (eq result1 +constPone+) (eq result0 +constPone+))))))


;;; Update the run-prover function to use the new derive function signature
(defun run-prover ()
  ;; Define a predicate for use if needed
  (make-predicate 'P 1)
  (make-predicate 'Q 0) ; 0-arity predicate
  (make-predicate 'R 0) ; 0-arity predicate

  (let* ((f-str1 "(AND_P ONE_P ONE_P)")
         (f1 (handler-case (parse-formula f-str1) (error (c) (format t "Error parsing f1: ~a~%" c) nil)))
         (f-str2 "(OR_P Q (AND_P Q ONE_P))") ; Propositional example
         (f2 (handler-case (parse-formula f-str2) (error (c) (format t "Error parsing f2: ~a~%" c) nil)))
         (f-str3 "(EXISTS X (P X))") ; Formula with quantifier/predicate (Changed input format)
         (f3 (handler-case (parse-formula f-str3) (error (c) (format t "Error parsing f3: ~a~%" c) nil)))
         (f-str4 "(AND_P Q R)") ; Undefined R
         (f4 (handler-case (parse-formula f-str4) (error (c) (format t "Error parsing f4: ~a~%" c) nil)))
         (f-str5 "(IMPLIES_P ZERO_P ONE_P)")
         (f5 (handler-case (parse-formula f-str5) (error (c) (format t "Error parsing f5: ~a~%" c) nil)))
         (f-str6 "(NOT_IMPLIES_P ONE_P ZERO_P)")
         (f6 (handler-case (parse-formula f-str6) (error (c) (format t "Error parsing f6: ~a~%" c) nil)))
         (f-str7 "(IMPLIES_P Q ONE_P)")
         (f7 (handler-case (parse-formula f-str7) (error (c) (format t "Error parsing f7: ~a~%" c) nil)))
         (f-str8 "(NOT_IMPLIES_P ONE_P Q)")
         (f8 (handler-case (parse-formula f-str8) (error (c) (format t "Error parsing f8: ~a~%" c) nil)))
         )

    (format t "~%--- Formula 1 ---~%")
    (if f1 (progn
             (format t "Input String: ~s~%" f-str1)
             (format t "Parsed Formula: ~a~%" (print-formula f1))
             (format t "WFF?: ~a~%" (wff-p f1))
             (format t "Variables: ~a~%" (variables-of f1))
             (format t "Tautology?: ~a~%" (tautology-p f1))
             (print-derivation (derive f1)))
        (format t "Skipping Formula 1 due to parse error.~%"))


    (format t "~%--- Formula 2 ---~%")
    (if f2 (progn
             (format t "Input String: ~s~%" f-str2)
             (format t "Parsed Formula: ~a~%" (print-formula f2))
             (format t "WFF?: ~a~%" (wff-p f2))
             (format t "Variables: ~a~%" (variables-of f2))
             (format t "Tautology?: ~a~%" (tautology-p f2))
             (print-derivation (derive f2)))
        (format t "Skipping Formula 2 due to parse error.~%"))

    (format t "~%--- Formula 3 ---~%")
    (if f3 (progn
             (format t "Input String: ~s~%" f-str3)
             (format t "Parsed Formula: ~a~%" (print-formula f3))
             (format t "WFF?: ~a~%" (wff-p f3))
             (format t "Variables: ~a~%" (variables-of f3))
             (format t "Tautology?: ~a~%" (tautology-p f3)) ; Should return :NOT-PROPOSITIONAL
             (print-derivation (derive f3)))
        (format t "Skipping Formula 3 due to parse error.~%"))

    (format t "~%--- Formula 4 (Example with variable) ---~%")
    (if f4 (progn
             (format t "Input String: ~s~%" f-str4)
             (format t "Parsed Formula: ~a~%" (print-formula f4))
             (format t "WFF?: ~a~%" (wff-p f4))
             (format t "Variables: ~a~%" (variables-of f4))
             (format t "Tautology?: ~a~%" (tautology-p f4))
             (print-derivation (derive f4)))
        (format t "Skipping Formula 4 due to parse error.~%"))

    (format t "~%--- Formula 5 (Axiom Implication Right) ---~%")
    (if f5 (progn
             (format t "Input String: ~s~%" f-str5)
             (format t "Parsed Formula: ~a~%" (print-formula f5))
             (format t "WFF?: ~a~%" (wff-p f5))
             (format t "Variables: ~a~%" (variables-of f5))
             (format t "Tautology?: ~a~%" (tautology-p f5))
             (print-derivation (derive f5)))
        (format t "Skipping Formula 5 due to parse error.~%"))

    (format t "~%--- Formula 6 (Axiom Non-Implication Right) ---~%")
    (if f6 (progn
             (format t "Input String: ~s~%" f-str6)
             (format t "Parsed Formula: ~a~%" (print-formula f6))
             (format t "WFF?: ~a~%" (wff-p f6))
             (format t "Variables: ~a~%" (variables-of f6))
             (format t "Tautology?: ~a~%" (tautology-p f6))
             (print-derivation (derive f6)))
        (format t "Skipping Formula 6 due to parse error.~%"))

    (format t "~%--- Formula 7 (Implication Right - Simplified) ---~%")
    (if f7 (progn
             (format t "Input String: ~s~%" f-str7)
             (format t "Parsed Formula: ~a~%" (print-formula f7))
             (format t "WFF?: ~a~%" (wff-p f7))
             (format t "Variables: ~a~%" (variables-of f7))
             (format t "Tautology?: ~a~%" (tautology-p f7))
             (print-derivation (derive f7)))
        (format t "Skipping Formula 7 due to parse error.~%"))

    (format t "~%--- Formula 8 (Non-Implication Right - Simplified) ---~%")
    (if f8 (progn
             (format t "Input String: ~s~%" f-str8)
             (format t "Parsed Formula: ~a~%" (print-formula f8))
             (format t "WFF?: ~a~%" (wff-p f8))
             (format t "Variables: ~a~%" (variables-of f8))
             (format t "Tautology?: ~a~%" (tautology-p f8))
             (print-derivation (derive f8)))
        (format t "Skipping Formula 8 due to parse error.~%"))
    ))

;; To run the example:
(weaver-prover:run-prover)