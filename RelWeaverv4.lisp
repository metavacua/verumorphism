;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Improved RWSDL-Min Code with Meta-Closure Thread Types (Formalized) - IMPROVED - Rule Renaming ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Axioms (functions directly implementing axioms)
(defun rwsdl-min-axiom-con-r (expression thread-type)
  "Axiom for 'CON' (Consistency) in Proof and Meta-Closure Threads."
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread :p_close)  ; In proof closure thread, CON is proof-closed
        (:meta_closure_thread :p_close) ; In meta-closure thread, CON is also proof-closed (acts like proof thread for axioms)
        (otherwise :np_close))          ; In other threads, no proof-closure
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if axiom doesn't match, it's no-closure
        (otherwise :np_close))))         ; Otherwise, no proof-closure

(defun rwsdl-min-axiom-incon-l (expression thread-type)
  "Axiom for 'INCON' (Inconsistency) in Refutation and Meta-Closure Threads."
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation_closure_thread :r_close) ; In refutation closure thread, INCON is refutation-closed
        (:meta_closure_thread :r_close) ; In meta-closure thread, INCON is also refutation-closed (acts like refutation thread for axioms)
        (otherwise :nr_close))         ; In other threads, no refutation-closure
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if axiom doesn't match, it's no-closure
        (otherwise :nr_close))))         ; Otherwise, no refutation-closure


;;; Inference Rules (functions directly implementing rules)

(defun rwsdl-min-rule-or-thread-r (expression thread-type)
  "Rule for 'OR_Thread_R' (Right-Introduction of OR in Thread) in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type))) ; Proof if either branch proves
          (:meta_closure_thread  ; Meta-closure thread OR_Thread_R behavior:
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :p_close) :p_close)   ; If branch 'a' proves, OR_Thread_R proves
               ((eq result-b :p_close) :p_close)   ; If branch 'b' proves, OR_Thread_R proves
               ((eq result-a :r_close) :r_close)   ; Propagate refutation if either branch refutes (important for meta-closure)
               ((eq result-b :r_close) :r_close)   ; Propagate refutation if either branch refutes (important for meta-closure)
               (t :no_close))))         ; No closure if neither branch proves in meta-closure thread
          (otherwise :np_close)))         ; No proof-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))         ; Otherwise, no proof-closure

(defun rwsdl-min-rule-and-thread-r (expression thread-type)
  "Rule for 'AND_Thread_R' (Right-Introduction of AND in Thread) in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type))) ; Proof only if both branches prove
          (:meta_closure_thread ; Meta-closure thread AND_Thread_R behavior:
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :p_close) (eq result-b :p_close)) :p_close) ; Proof only if both branches prove in meta-closure
               ((eq result-a :no_close) :no_close)   ; Propagate no-closure if either branch is no-closure (important for meta-closure)
               ((eq result-b :no_close) :no_close)   ; Propagate no-closure if either branch is no-closure (important for meta-closure)
               (t :no_close))))        ; No closure otherwise in meta-closure thread
          (otherwise :np_close)))         ; No proof-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))         ; Otherwise, no proof-closure

(defun rwsdl-min-rule-duality-r (expression thread-type)
  "Rule for 'DUALITY_R' (Right-Introduction of Duality) in Proof and Meta-Closure Threads (Right-Introduction)."
  (if (and (listp expression) (eq (car expression) 'DUALITY_R) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality_R Rule (Proof/Meta-Closure Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Meta-Closure Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :meta_closure_thread))) ; Crucially, switch to meta-closure thread for duality
          (print (format nil "    Result of Meta-Closure Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :r_close)
             (progn
               (print (format nil "    Meta-Closure Thread returned :R_CLOSE. Therefore, Proof/Meta-Closure Thread concludes: :P_CLOSE for ~a" expression))
               :p_close))            ; If meta-closure refutes, duality in proof thread is proof
            ((eq dual-thread-result :no_close) ; Handle no closure from meta-closure thread
             (progn
               (print (format nil "    Meta-Closure Thread returned :NO_CLOSE. Therefore, Proof/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression))
               :no_close))          ; If meta-closure no-closure, duality in proof thread is no-closure
            (t  ; Default case if dual thread returns :np_close or anything else non-refuted/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not refute. Therefore, Proof/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))        ; Default to no-closure if meta-closure doesn't refute
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))         ; Otherwise, no proof-closure

(defun rwsdl-min-rule-or-thread-l (expression thread-type)
  "Rule for 'OR_Thread_L' (Left-Introduction of OR in Thread) in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type))) ; Refutes if either branch refutes
          (:meta_closure_thread ; Meta-closure thread OR_Thread_L behavior:
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :r_close) :r_close)   ; If branch 'a' refutes, OR_Thread_L refutes
               ((eq result-b :r_close) :r_close)   ; If branch 'b' refutes, OR_Thread_L refutes
               ((eq result-a :p_close) :p_close)   ; Propagate proof if either branch proves (important for meta-closure)
               ((eq result-b :p_close) :p_close)   ; Propagate proof if either branch proves (important for meta-closure)
               (t :no_close))))         ; No closure if neither branch refutes in meta-closure thread
          (otherwise :nr_close)))         ; No refutation-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))         ; Otherwise, no refutation-closure


(defun rwsdl-min-rule-and-thread-l (expression thread-type)
  "Rule for 'AND_Thread_L' (Left-Introduction of AND in Thread) in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type))) ; Refutes only if both branches refute
          (:meta_closure_thread  ; Meta-closure thread AND_Thread_L behavior:
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :r_close) (eq result-b :r_close)) :r_close) ; Refutes only if both branches refute in meta-closure
               ((eq result-a :no_close) :no_close)   ; Propagate no-closure if either branch is no-closure (important for meta-closure)
               ((eq result-b :no_close) :no_close)   ; Propagate no-closure if either branch is no-closure (important for meta-closure)
               (t :no_close))))        ; No closure otherwise in meta-closure thread
          (otherwise :nr_close)))         ; No refutation-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))         ; Otherwise, no refutation-closure

(defun rwsdl-min-rule-duality-l (expression thread-type)
  "Rule for 'DUALITY_L' (Left-Introduction of Duality) in Refutation and Meta-Closure Threads (Left-Introduction)."
  (if (and (listp expression) (eq (car expression) 'DUALITY_L) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality_L Rule (Refutation/Meta-Closure Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Meta-Closure Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :meta_closure_thread))) ; Crucially switch to meta-closure thread for duality
          (print (format nil "    Result of Meta-Closure Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :p_close)
             (progn
               (print (format nil "    Meta-Closure Thread returned :P_CLOSE. Therefore, Refutation/Meta-Closure Thread concludes: :R_CLOSE for ~a" expression))
               :r_close))            ; If meta-closure proves, duality in refutation thread is refutation
            ((eq dual-thread-result :no_close) ; Handle no closure from meta-closure thread
             (progn
               (print (format nil "    Meta-Closure Thread returned :NO_CLOSE. Therefore, Refutation/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression))
               :no_close))          ; If meta-closure no-closure, duality in refutation thread is no-closure
            (t  ; Default case if dual thread returns :nr_close or anything else non-proof/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not prove. Therefore, Refutation/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))        ; Default to no-closure if meta-closure doesn't prove
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))         ; Otherwise, no refutation-closure


;;; Minimal Self-Interpretation (modified to handle :meta_closure_thread type and closure outcomes)
(defun rwsdl-min-self-interpret (expression thread-type)
  "Minimal self-interpretation function, dispatching to axioms and rules based on expression and thread type."
  (cond
    ((eq expression 'CON) (rwsdl-min-axiom-con-r expression thread-type))
    ((eq expression 'INCON) (rwsdl-min-axiom-incon-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'OR_Thread_R)) (rwsdl-min-rule-or-thread-r expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Thread_R)) (rwsdl-min-rule-and-thread-r expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY_R)) (rwsdl-min-rule-duality-r expression thread-type)) ; Duality rule for proof/meta-closure thread
    ((and (listp expression) (eq (car expression) 'OR_Thread_L)) (rwsdl-min-rule-or-thread-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Thread_L)) (rwsdl-min-rule-and-thread-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY_L)) (rwsdl-min-rule-duality-l expression thread-type)) ; Duality rule for refutation/meta-closure thread
    (t (case thread-type
         (:proof_closure_thread :np_close)    ; Default to no proof-closure
         (:refutation_closure_thread :nr_close) ; Default to no refutation-closure
         (:meta_closure_thread :no_close)         ; Default for meta-closure threads is no-closure
         (otherwise (error "Invalid thread type: ~a. Must be :proof_closure_thread, :refutation_closure_thread, or :meta_closure_thread." thread-type))))))


;;; Evaluation Functions (modified to handle :meta_closure_thread type)
(defun rwsdl-min-evaluate (expression thread-type)
  "Evaluates an expression in the specified thread type."
  (case thread-type
    (:proof_closure_thread (rwsdl-min-self-interpret expression thread-type))
    (:refutation_closure_thread (rwsdl-min-self-interpret expression thread-type))
    (:meta_closure_thread (rwsdl-min-self-interpret expression thread-type))    ; Case for meta-closure_thread
    (otherwise (error "Invalid thread type: ~a. Must be :proof_closure_thread, :refutation_closure_thread, or :meta_closure_thread." thread-type))))


;;; Proof, Refutation, and Meta-Closure Threads (using rwsdl-min-evaluate)
(defun rwsdl-min-proof-closure-thread (expression)
  "Evaluates expression in a proof closure thread."
  (rwsdl-min-evaluate expression :proof_closure_thread))

(defun rwsdl-min-refutation-closure-thread (expression)
  "Evaluates expression in a refutation closure thread."
  (rwsdl-min-evaluate expression :refutation_closure_thread))

(defun rwsdl-min-meta-closure-thread (expression)
  "Evaluates expression in a meta-closure thread."
  (rwsdl-min-evaluate expression :meta_closure_thread))


;;; Bootstrap Loop Test (modified to test meta-closure threads and new closure outputs)
(defun rwsdl-min-bootstrap-loop-test ()
  "Tests RWSDL-Min self-interpretation with different thread types and expressions."
  (print "*** RWSDL-Min Bootstrap Loop Test (Self-Interpreted, Corrected Rules, Meta-Closure Threads, Renamed Rules) ***")

  (let ((test-expressions '(
                         CON
                         (OR_Thread_R CON 'INCON)
                         (AND_Thread_R CON CON)
                         (DUALITY_R INCON)
                         (DUALITY_R CON)
                         (OR_Thread_L INCON 'CON)
                         (AND_Thread_L INCON INCON)
                         (OR_Thread_R CON '(DUALITY_R CON))
                         (AND_Thread_R CON '(DUALITY_R INCON))
                         (OR_Thread_L INCON '(DUALITY_R CON))
                         (AND_Thread_L INCON '(DUALITY_R INCON))
                         (OR_Thread_R 'A 'B)
                         (AND_Thread_R 'A 'B)
                         (DUALITY_R 'A)
                         (OR_Thread_L 'A 'B)
                         (AND_Thread_L 'A 'B)
                         (DUALITY_L 'A) ; Corrected to DUALITY_L for symmetry in test expressions
                         )))


    (loop for expr in test-expressions do
      (print (format nil "** Evaluating ~a in Proof Closure Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-proof-closure-thread expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Refutation Closure Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-refutation-closure-thread expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Meta-Closure Thread (Self-Interpreted):" expr))    ; Test in Meta-Closure Thread
      (let ((result (rwsdl-min-meta-closure-thread expr)))
        (print  result))
      (terpri))
    ))

;;; Run Bootstrap Test
(rwsdl-min-bootstrap-loop-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation of Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;"**Code Changes Documentation (Rule Renaming for Symmetry and Interpretability):**"

;"This version introduces rule renaming to enhance symmetry and interpretability within the RWSDL-Min system, particularly concerning proof and refutation dualities."

;"1. **Rule Renaming for Proof and Refutation Duals:**"
;  "- `OR_Thread` is renamed to `OR_Thread_R` (Right-Introduction of OR in Thread)."
;  "- `AND_Thread` is renamed to `AND_Thread_R` (Right-Introduction of AND in Thread)."
;  "- `OR_Refutation` is renamed to `OR_Thread_L` (Left-Introduction of OR in Thread)."
;  "- `AND_Refutation` is renamed to `AND_Thread_L` (Left-Introduction of AND in Thread)."
;  "- `DUALITY` (for Proof Thread) is renamed to `DUALITY_R` (Right-Introduction of Duality)."
;  "- `DUALITY` (for Refutation Thread) is renamed to `DUALITY_L` (Left-Introduction of Duality)."

;"2. **Function Name Updates:**"
;  "- Corresponding function names are updated to reflect the rule renaming, ensuring consistency (e.g., `rwsdl-min-rule-or-thread-r` now corresponds to the `OR_Thread_R` rule)."

;"3. **Documentation and Comments Updated:**"
;  "- Comments and documentation strings are updated to use the new rule names and explain the rationale behind the renaming, emphasizing symmetry and clearer interpretation of rule behavior in proof and refutation contexts."

;"4. **Bootstrap Test Updated:**"
;  "- The bootstrap test is updated to use the renamed rules in the test expressions, ensuring that the renaming does not introduce regressions and that the system continues to function as expected."
;  "- A minor correction is made in the test expressions to include `DUALITY_L` instead of the generic `DUALITY` in the refutation context, to ensure symmetry in testing both left and right duality rules."

;"**Rationale for Renaming:**"
;"- **Enhanced Symmetry:** The renaming introduces a symmetric naming convention (`_R` for Right-Introduction, `_L` for Left-Introduction) that mirrors the duality between proof and refutation threads. This symmetry makes the rule set more conceptually elegant and easier to remember."
;"- **Improved Interpretability:** The `_Thread_R` and `_Thread_L` suffixes are designed to be more descriptive of the rules' behavior within their respective threads (Right for Proof-like, Left for Refutation-like), improving the interpretability of the rule names."
;"- **Preparation for RelWeaver Integration:** This renaming is a preparatory step towards integrating RWSDL-Min with RelWeaver, where symmetrical evaluation and clear rule naming conventions will be crucial for handling diverse logical expressions and interpretations."

;"**Next Steps and Considerations (Addressing User's Point on Expression Rules and Symmetric Evaluation):**"
;"- **Expression Rules for Initial Interpretation:** The user's point about 'expression rules' is insightful.  In RelWeaver, before dispatching to specific threads (proof, refutation, duality), we might indeed need a layer of 'expression rules'."
;"- **Symmetric Initial Evaluation:** These 'expression rules' would be responsible for the initial, symmetric interpretation of logical connectives (like 'AND', 'OR', 'NOT' or their non-bivalent counterparts like dependence, independence, duality)."
;"- **Dispatch to Threads:** After the initial interpretation by 'expression rules', the resulting expressions would then be dispatched to the appropriate threads (`proof_closure_thread`, `refutation_closure_thread`, `meta_closure_thread`) for further, thread-specific evaluation."
;"- **Example of Expression Rules (Conceptual):**"
;  "  - `AND_expr(A, B)`:  Initial interpretation of 'AND' could be mapped to a dependence relation or `AND_Thread_R` and `AND_Thread_L` depending on the context."
;  "  - `OR_expr(A, B)`: Initial interpretation of 'OR' could be mapped to an independence relation or `OR_Thread_R` and `OR_Thread_L`."
;  "  - `NOT_expr(A)` or `NEG_expr(A)`: Initial interpretation of 'NOT/NEG' could be mapped to a duality relation or `DUALITY_R` and `DUALITY_L`."
;"- **Further Development:** Implementing these 'expression rules' would be a logical next step to enhance RWSDL-Min's capabilities and align it more closely with the requirements of a system like RelWeaver, allowing for more flexible and context-aware logical evaluations."

