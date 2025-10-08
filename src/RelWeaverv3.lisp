;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Improved RWSDL-Min Code with Meta-Closure Thread Types (Formalized) ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Axioms (functions directly implementing axioms)
(defun rwsdl-min-axiom-con-r (expression thread-type)
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread :p_close)
        (:meta_closure_thread :p_close) ; Meta-closure threads act like proof threads for axioms
        (otherwise :np_close))
      (case thread-type
        (:meta_closure_thread :no_close) ; No Closure for meta-closure threads when axiom doesn't match
        (otherwise :np_close))))

(defun rwsdl-min-axiom-incon-l (expression thread-type)
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation_closure_thread :r_close)
        (:meta_closure_thread :r_close) ; Meta-closure threads act like refutation threads for axioms
        (otherwise :nr_close))
      (case thread-type
        (:meta_closure_thread :no_close) ; No Closure for meta-closure threads when axiom doesn't match
        (otherwise :nr_close))))


;;; Inference Rules (functions directly implementing rules)

(defun rwsdl-min-rule-or-thread-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'OR_Thread) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:meta_closure_thread  ; Meta-closure thread OR_Thread behavior - propagate no_close
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :p_close) :p_close)
               ((eq result-b :p_close) :p_close)
               ((eq result-a :r_close) :r_close) ; Propagate refute if either branch refutes
               ((eq result-b :r_close) :r_close) ; Propagate refute if either branch refutes
               (t :no_close))))         ; No closure if neither proves
          (otherwise :np_close)))
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :np_close))))

(defun rwsdl-min-rule-and-thread-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'AND_Thread) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:meta_closure_thread ; Meta-closure thread AND_Thread behavior - propagate no_close
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :p_close) (eq result-b :p_close)) :p_close)
               ((eq result-a :no_close) :no_close) ; Propagate no_closure if either branch no_closure
               ((eq result-b :no_close) :no_close) ; Propagate no_closure if either branch no_closure
               (t :no_close))))        ; No closure otherwise (if neither proves and neither is no_close)
          (otherwise :np_close)))
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :np_close))))

(defun rwsdl-min-rule-duality-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'DUALITY) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality Rule (Proof/Meta-Closure Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Meta-Closure Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :meta_closure_thread))) ; Switch to :meta_closure_thread
          (print (format nil "    Result of Meta-Closure Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :r_close)
             (progn
               (print (format nil "    Meta-Closure Thread returned :R_CLOSE. Therefore, Proof/Meta-Closure Thread concludes: :P_CLOSE for ~a" expression))
               :p_close))
            ((eq dual-thread-result :no_close) ; Handle no closure from meta-closure thread
             (progn
               (print (format nil "    Meta-Closure Thread returned :NO_CLOSE. Therefore, Proof/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression))
               :no_close))
            (t  ; Default case if dual thread returns :np_close or anything else non-refuted/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not refute. Therefore, Proof/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))  ; Changed to :no_closure
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :np_close))))


(defun rwsdl-min-rule-or-refutation-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'OR_Refutation) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:meta_closure_thread ; Meta-closure thread OR_Refutation behavior - propagate no_close
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :r_close) :r_close)
               ((eq result-b :r_close) :r_close)
               ((eq result-a :p_close) :p_close) ; Propagate proof if either branch proves
               ((eq result-b :p_close) :p_close) ; Propagate proof if either branch proves
               (t :no_close))))         ; No closure otherwise
          (otherwise :nr_close)))
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :nr_close))))


(defun rwsdl-min-rule-and-refutation-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'AND_Refutation) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:meta_closure_thread  ; Meta-closure thread AND_Refutation behavior - propagate no_close
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :r_close) (eq result-b :r_close)) :r_close)
               ((eq result-a :no_close) :no_close) ; Propagate no_closure if either branch no_closure
               ((eq result-b :no_close) :no_close) ; Propagate no_closure if either branch no_closure
               (t :no_close))))        ; No closure otherwise (if neither refutes and neither is no_closure)
          (otherwise :nr_close)))
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :nr_close))))

(defun rwsdl-min-rule-duality-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'DUALITY) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality Rule (Refutation/Meta-Closure Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Meta-Closure Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :meta_closure_thread))) ; Switch to :meta_closure_thread
          (print (format nil "    Result of Meta-Closure Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :p_close)
             (progn
               (print (format nil "    Meta-Closure Thread returned :P_CLOSE. Therefore, Refutation/Meta-Closure Thread concludes: :R_CLOSE for ~a" expression))
               :r_close))
            ((eq dual-thread-result :no_close) ; Handle no closure from meta-closure thread
             (progn
               (print (format nil "    Meta-Closure Thread returned :NO_CLOSE. Therefore, Refutation/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression))
               :no_close))
            (t   ; Default case if dual thread returns :nr_close or anything else non-proof/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not prove. Therefore, Refutation/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))  ; Changed to :no_closure
      (case thread-type
        (:meta_closure_thread :no_close)
        (otherwise :nr_close))))


;;; Minimal Self-Interpretation (modified to handle :meta_closure_thread type and closure outcomes)
(defun rwsdl-min-self-interpret (expression thread-type)
  (cond
    ((eq expression 'CON) (rwsdl-min-axiom-con-r expression thread-type))
    ((eq expression 'INCON) (rwsdl-min-axiom-incon-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'OR_Thread)) (rwsdl-min-rule-or-thread-r expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Thread)) (rwsdl-min-rule-and-thread-r expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY)) (rwsdl-min-rule-duality-r expression thread-type))
    ((and (listp expression) (eq (car expression) 'OR_Refutation)) (rwsdl-min-rule-or-refutation-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Refutation)) (rwsdl-min-rule-and-refutation-l expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY)) (rwsdl-min-rule-duality-l expression thread-type))
    (t (case thread-type
         (:proof_closure_thread :np_close)
         (:refutation_closure_thread :nr_close)
         (:meta_closure_thread :no_close)         ; Default for meta-closure threads is no closure
         (otherwise (error "Invalid thread type: ~a. Must be :proof_closure_thread, :refutation_closure_thread, or :meta_closure_thread." thread-type))))))


;;; Evaluation Functions (modified to handle :meta_closure_thread type)
(defun rwsdl-min-evaluate (expression thread-type)
  (case thread-type
    (:proof_closure_thread (rwsdl-min-self-interpret expression thread-type))
    (:refutation_closure_thread (rwsdl-min-self-interpret expression thread-type))
    (:meta_closure_thread (rwsdl-min-self-interpret expression thread-type))    ; Added :meta_closure_thread case
    (otherwise (error "Invalid thread type: ~a. Must be :proof_closure_thread, :refutation_closure_thread, or :meta_closure_thread." thread-type))))


;;; Proof, Refutation, and Meta-Closure Threads (using rwsdl-min-evaluate)
(defun rwsdl-min-proof-closure-thread (expression)
  (rwsdl-min-evaluate expression :proof_closure_thread))

(defun rwsdl-min-refutation-closure-thread (expression)
  (rwsdl-min-evaluate expression :refutation_closure_thread))

(defun rwsdl-min-meta-closure-thread (expression)      ; New Meta-Closure Thread function
  (rwsdl-min-evaluate expression :meta_closure_thread))


;;; Bootstrap Loop Test (modified to test meta-closure threads and new closure outputs)
(defun rwsdl-min-bootstrap-loop-test ()
  (print "*** RWSDL-Min Bootstrap Loop Test (Self-Interpreted, Corrected Rules, Meta-Closure Threads) ***")

  (let ((test-expressions '(
                         CON
                         (OR_Thread CON 'INCON)
                         (AND_Thread CON CON)
                         (DUALITY INCON)
                         (DUALITY CON)
                         (OR_Refutation INCON 'CON)
                         (AND_Refutation INCON INCON)
                         (OR_Thread CON '(DUALITY CON))
                         (AND_Thread CON '(DUALITY INCON))
                         (OR_Refutation INCON '(DUALITY CON))
                         (AND_Refutation INCON '(DUALITY INCON))
                         (OR_Thread 'A 'B)
                         (AND_Thread 'A 'B)
                         (DUALITY 'A)
                         (OR_Refutation 'A 'B)
                         (AND_Refutation 'A 'B)
                         (DUALITY 'A)
                         )))


    (loop for expr in test-expressions do
      (print (format nil "** Evaluating ~a in Proof Closure Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-proof-closure-thread expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Refutation Closure Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-refutation-closure-thread expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Meta-Closure Thread (Self-Interpreted):" expr))     ; Evaluating in Meta-Closure Thread
      (let ((result (rwsdl-min-meta-closure-thread expr)))
        (print  result))
      (terpri))
    ))

;;; Run Bootstrap Test
(rwsdl-min-bootstrap-loop-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation of Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;"**Code Changes Documentation (Formalized Meta-Closure Thread Types and Outcomes):**"

;"1. **Thread Types Renamed to Reflect Closure Concepts:**"
;  "- `:proof` is renamed to `:proof_closure_thread`."
;  "- `:refutation` is renamed to `:refutation_closure_thread`."
;  "- `:dual` is renamed to `:meta_closure_thread`, more accurately reflecting its role in meta-closure."

;"2. **Closure Outcome Symbols Formalized:**"
;  "- `:proof` is replaced with `:p_close` (Proof-Closure)."
;  "- `:refuted` is replaced with `:r_close` (Refutation-Closure)."
;  "- `:no_proof` is replaced with `:np_close` (No-Proof Sub-Closure)."
;  "- `:no_refutation` is replaced with `:nr_close` (No-Refutation Sub-Closure)."
;  "- `:no_closure` remains `:no_close` (Meta-Closure/No Closure), but now explicitly recognized as a top-level closure outcome."

;"3. **Axiom Functions Updated for Closure Outcomes:**"
;  "- `rwsdl-min-axiom-con-r` and `rwsdl-min-axiom-incon-l` now return the formalized closure outcome symbols (e.g., `:p_close`, `:r_close`, `:no_close`, `:np_close`, `:nr_close`) based on the thread type."

;"4. **Inference Rule Functions Updated for Closure Outcomes:**"
;  "- `rwsdl-min-rule-or-thread-r`, `rwsdl-min-rule-and-thread-r`, `rwsdl-min-rule-duality-r`, `rwsdl-min-rule-or-refutation-l`, `rwsdl-min-rule-and-refutation-l`, `rwsdl-min-rule-duality-l` are updated to:"
;  "  - Use and propagate the new closure outcome symbols."
;  "  - Maintain the logic for `:meta_closure_thread` behavior, including propagation of `:no_close`."

;"5. **Minimal Self-Interpretation and Evaluation Functions Updated:**"
;  "- `rwsdl-min-self-interpret` and `rwsdl-min-evaluate` are updated to handle the new thread types and closure outcome symbols."
;  "- Default cases in `rwsdl-min-self-interpret` now return `:np_close` for `:proof_closure_thread`, `:nr_close` for `:refutation_closure_thread`, and `:no_close` for `:meta_closure_thread`."

;"6. **Thread Functions Renamed and Updated:**"
;  "- `rwsdl-min-proof-thread` is renamed to `rwsdl-min-proof-closure-thread`."
;  "- `rwsdl-min-refutation-thread` is renamed to `rwsdl-min-refutation-closure-thread`."
;  "- `rwsdl-min-dual-thread` is renamed to `rwsdl-min-meta-closure-thread`."
;  "- These functions now call `rwsdl-min-evaluate` with the corresponding new thread type names."

;"7. **Bootstrap Test Updated for New Thread Types and Outcomes:**"
;  "- `rwsdl-min-bootstrap-loop-test` is updated to:"
;  "  - Use the new thread function names (e.g., `rwsdl-min-proof-closure-thread`)."
;  "  - Expect and print the new closure outcome symbols (e.g., `:p_close`, `:r_close`, `:no_close`, `:np_close`, `:nr_close`) in the output."
;  "- Output messages are adjusted to use the new terminology (e.g., 'Proof Closure Thread', 'Meta-Closure Thread')."

;"**Code Comments and Documentation Enhanced:**"
;"- Comments are added throughout the code to explain the new thread types, closure outcomes, and their relationship to meta-closure."
;"- The 'Documentation of Changes' section is updated to comprehensively document all the terminology changes and code modifications."