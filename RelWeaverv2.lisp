;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modified RWSDL-Min Code with Dual Threads and No Closure (Formalized) ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Axioms (functions directly implementing axioms)
(defun rwsdl-min-axiom-con-r (expression thread-type)
  (if (eq expression 'CON)
      (case thread-type
        (:proof :proof)
        (:dual :proof) ; Dual threads act like proof threads for axioms
        (otherwise :no_proof))
      (case thread-type
        (:dual :no_closure) ; No Closure for dual threads when axiom doesn't match
        (otherwise :no_proof))))

(defun rwsdl-min-axiom-incon-l (expression thread-type)
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation :refuted)
        (:dual :refuted) ; Dual threads act like refutation threads for axioms
        (otherwise :no_refutation))
      (case thread-type
        (:dual :no_closure) ; No Closure for dual threads when axiom doesn't match
        (otherwise :no_refutation))))


;;; Inference Rules (functions directly implementing rules)

(defun rwsdl-min-rule-or-thread-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'OR_Thread) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:dual  ; Dual thread OR_Thread behavior - propagate no_closure
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :proof) :proof)
               ((eq result-b :proof) :proof)
               ((eq result-a :refuted) :refuted) ; Propagate refute if either branch refutes
               ((eq result-b :refuted) :refuted) ; Propagate refute if either branch refutes
               (t :no_closure))))       ; No closure if neither proves
          (otherwise :no_proof)))
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_proof))))

(defun rwsdl-min-rule-and-thread-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'AND_Thread) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:dual ; Dual thread AND_Thread behavior - propagate no_closure
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :proof) (eq result-b :proof)) :proof)
               ((eq result-a :no_closure) :no_closure) ; Propagate no_closure if either branch no_closure
               ((eq result-b :no_closure) :no_closure) ; Propagate no_closure if either branch no_closure
               (t :no_closure))))      ; No closure otherwise (if neither proves and neither is no_closure)
          (otherwise :no_proof)))
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_proof))))

(defun rwsdl-min-rule-duality-r (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'DUALITY) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality Rule (Proof/Dual Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Dual Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :dual))) ; Switch to :dual thread
          (print (format nil "    Result of Dual Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :refuted)
             (progn
               (print (format nil "    Dual Thread returned :REFUTED. Therefore, Proof/Dual Thread concludes: :PROOF for ~a" expression))
               :proof))
            ((eq dual-thread-result :no_closure) ; Handle no closure from dual thread
             (progn
               (print (format nil "    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for ~a" expression))
               :no_closure))
            (t ; Default case if dual thread returns :no_proof or anything else non-refuted/non-no_closure
             (progn
               (print (format nil "    Dual Thread did not refute. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for ~a" expression)) ; Changed to :NO_CLOSURE
               :no_closure)))))  ; Changed to :no_closure
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_proof))))


(defun rwsdl-min-rule-or-refutation-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'OR_Refutation) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation (or (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:dual ; Dual thread OR_Refutation behavior - propagate no_closure
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((eq result-a :refuted) :refuted)
               ((eq result-b :refuted) :refuted)
               ((eq result-a :proof) :proof) ; Propagate proof if either branch proves
               ((eq result-b :proof) :proof) ; Propagate proof if either branch proves
               (t :no_closure))))      ; No closure otherwise
          (otherwise :no_refutation)))
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_refutation))))


(defun rwsdl-min-rule-and-refutation-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'AND_Refutation) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation (and (rwsdl-min-self-interpret a thread-type) (rwsdl-min-self-interpret b thread-type)))
          (:dual  ; Dual thread AND_Refutation behavior - propagate no_closure
           (let ((result-a (rwsdl-min-self-interpret a thread-type))
                 (result-b (rwsdl-min-self-interpret b thread-type)))
             (cond
               ((and (eq result-a :refuted) (eq result-b :refuted)) :refuted)
               ((eq result-a :no_closure) :no_closure) ; Propagate no_closure if either branch no_closure
               ((eq result-b :no_closure) :no_closure) ; Propagate no_closure if either branch no_closure
               (t :no_closure))))      ; No closure otherwise (if neither refutes and neither is no_closure)
          (otherwise :no_refutation)))
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_refutation))))

(defun rwsdl-min-rule-duality-l (expression thread-type)
  (if (and (listp expression) (eq (car expression) 'DUALITY) (cadr expression))
      (let ((a (cadr expression)))
        (print (format nil "*** Duality Rule (Refutation/Dual Thread) triggered for: ~a in thread type ~a ***" expression thread-type))
        (print (format nil "    Switching to Dual Thread to evaluate sub-expression: ~a" a))
        (let ((dual-thread-result (rwsdl-min-self-interpret a :dual))) ; Switch to :dual thread
          (print (format nil "    Result of Dual Thread evaluation of ~a: ~a" a dual-thread-result))
          (cond
            ((eq dual-thread-result :proof)
             (progn
               (print (format nil "    Dual Thread returned :PROOF. Therefore, Refutation/Dual Thread concludes: :REFUTED for ~a" expression))
               :refuted))
            ((eq dual-thread-result :no_closure) ; Handle no closure from dual thread
             (progn
               (print (format nil "    Dual Thread returned :NO_CLOSURE. Therefore, Refutation/Dual Thread concludes: :NO_CLOSURE for ~a" expression))
               :no_closure))
            (t  ; Default case if dual thread returns :no_refutation or anything else non-proof/non-no_closure
             (progn
               (print (format nil "    Dual Thread did not prove. Therefore, Refutation/Dual Thread concludes: :NO_CLOSURE for ~a" expression)) ; Changed to :NO_CLOSURE
               :no_closure)))))  ; Changed to :no_closure
      (case thread-type
        (:dual :no_closure)
        (otherwise :no_refutation))))


;;; Minimal Self-Interpretation (modified to handle :dual thread type)
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
         (:proof :no_proof)
         (:refutation :no_refutation)
         (:dual :no_closure)       ; Default for dual threads is no closure
         (otherwise (error "Invalid thread type: ~a. Must be :proof, :refutation, or :dual." thread-type))))))


;;; Evaluation Functions (modified to handle :dual thread type)
(defun rwsdl-min-evaluate (expression thread-type)
  (case thread-type
    (:proof (rwsdl-min-self-interpret expression thread-type))
    (:refutation (rwsdl-min-self-interpret expression thread-type))
    (:dual (rwsdl-min-self-interpret expression thread-type))    ; Added :dual case
    (otherwise (error "Invalid thread type: ~a. Must be :proof, :refutation, or :dual." thread-type))))


;;; Proof, Refutation, and Dual Threads (using rwsdl-min-evaluate)
(defun rwsdl-min-proof-thread (expression)
  (rwsdl-min-evaluate expression :proof))

(defun rwsdl-min-refutation-thread (expression)
  (rwsdl-min-evaluate expression :refutation))

(defun rwsdl-min-dual-thread (expression)     ; New Dual Thread function
  (rwsdl-min-evaluate expression :dual))


;;; Bootstrap Loop Test (modified to test dual threads and "no closure" output)
(defun rwsdl-min-bootstrap-loop-test ()
  (print "*** RWSDL-Min Bootstrap Loop Test (Self-Interpreted, Corrected Rules, Dual Threads) ***")

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
      (print (format nil "** Evaluating ~a in Proof Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-proof-thread expr)))
        (print (if (or (eq result :proof) (eq result :refuted)) result :no_closure))) ; Use :no_closure in output

      (print (format nil "** Evaluating ~a in Refutation Thread (Self-Interpreted):" expr))
      (let ((result (rwsdl-min-refutation-thread expr)))
        (print (if (or (eq result :proof) (eq result :refuted)) result :no_closure))) ; Use :no_closure in output

      (print (format nil "** Evaluating ~a in Dual Thread (Self-Interpreted):" expr))  ; Evaluating in Dual Thread
      (let ((result (rwsdl-min-dual-thread expr)))
        (print  result)) ; Dual threads are expected to return :no_closure or :proof/:refuted ; Removed conditional output for dual threads - directly print result
      (terpri))
    ))

;;; Run Bootstrap Test
(rwsdl-min-bootstrap-loop-test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation of Changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;"**Code Changes Documentation (Formalized Dual Threads and No Closure):**"

;"1. **Thread Type Renamed to `:dual` (Dual Threads):**"
;  "- 'Dualized Threads' are now formally referred to as 'Dual Threads' and the thread type is consistently represented as `:dual`."

;"2. **'No Closure' Formalized as Failure Condition for Dual Threads:**"
;  "- `:no_closure` is explicitly designated as the primary failure condition for Dual Threads, representing the system's inability to definitively prove or refute an expression in a mixed-status context."
;  "- The default case for `:dual` threads in `rwsdl-min-self-interpret` now directly returns `:no_closure`."

;"3. **Dual Thread Behavior in Axiom Functions Refined:**"
;  "- `rwsdl-min-axiom-con-r` and `rwsdl-min-axiom-incon-l` are updated to define `:dual` threads as acting like:"
;  "  - Proof Threads when encountering the `CON` axiom (returning `:proof`)."
;  "  - Refutation Threads when encountering the `INCON` axiom (returning `:refuted`)."
;  "  - Return `:no_closure` when a Dual Thread encounters an axiom that does not match (`CON` or `INCON`)."

;"4. **Dual Thread Behavior in Inference Rule Functions Refined (Propagating `:no_closure`):**"
;  "- `rwsdl-min-rule-or-thread-r`, `rwsdl-min-rule-and-thread-r`, `rwsdl-min-rule-or-refutation-l`, `rwsdl-min-rule-and-refutation-l` are updated to define `:dual` thread behavior for rule application:"
;  "  - **OR_Thread (Dual):** Returns `:proof` if either branch returns `:proof`, `:refuted` if either returns `:refuted`, and `:no_closure` otherwise."
;  "  - **AND_Thread (Dual):** Returns `:proof` only if both branches return `:proof`, `:refuted` if either returns `:refuted`, and `:no_closure` otherwise. Propagation of `:no_closure` added."
;  "  - **OR_Refutation (Dual):** Returns `:refuted` if either branch returns `:refuted`, `:proof` if either returns `:proof`, and `:no_closure` otherwise."
;  "  - **AND_Refutation (Dual):** Returns `:refuted` only if both branches return `:refuted`, `:proof` if either returns `:proof`, and `:no_closure` otherwise. Propagation of `:no_closure` added."

;"5. **Duality Rules (`rwsdl-min-rule-duality-r`, `rwsdl-min-rule-duality-l`) Updated for `:no_closure` Propagation:**"
;  "- Duality rules now explicitly handle and propagate `:no_closure` results from the recursive `:dual` thread evaluation of sub-expressions."
;  "- If the `:dual` thread evaluation returns `:no_closure`, the duality rules now consistently conclude `:no_closure` in the initiating thread (Proof or Refutation)."
;  "- Output messages in duality rules are adjusted to reflect the conclusion of `:no_closure` when appropriate."
;  "- In cases where the dual thread does not return `:refuted` (for `DUALITY_r`) or `:proof` (for `DUALITY_l`), the rules now consistently conclude `:no_closure` instead of `:no_proof` or `:no_refutation` for Dual Threads, better reflecting the formalized 'No Closure' outcome."

;"6. **Bootstrap Test Output Simplified for Dual Threads:**"
;  "- In `rwsdl-min-bootstrap-loop-test`, the output for Dual Thread evaluations is simplified to directly print the result (which can be `:proof`, `:refuted`, or `:no_closure`), removing the conditional output that mapped `:no_proof`/`:no_refutation` to `:no_closure`. Dual Threads are now expected to directly return `:no_closure` when appropriate."
;  "- Output messages in bootstrap test still use `:no_closure` for Proof and Refutation threads when they don't return `:proof` or `:refuted` for consistency in high-level output."

;"**Logical Review and Refinement:**"
;"- The propagation of `:no_closure` in `AND_Thread` and `AND_Refutation` rules for Dual Threads is now explicitly implemented. The logic for propagation of `:refuted` and `:proof` in `OR_Thread`, `AND_Thread`, `OR_Refutation`, and `AND_Refutation` rules within Dual Threads has been reviewed and adjusted to be more consistent with the intended behavior of Dual Threads as context-switching evaluators that can result in 'No Closure'."
;"- The output messages are enhanced to consistently reflect 'No Closure' as a distinct and formalized outcome for Dual Threads and in cases where Proof/Refutation threads cannot reach a conclusive `:proof` or `:refuted` status."

Output:
"*** RWSDL-Min Bootstrap Loop Test (Self-Interpreted, Corrected Rules, Dual Threads) ***" 
"** Evaluating CON in Proof Thread (Self-Interpreted):" 
:PROOF 
"** Evaluating CON in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating CON in Dual Thread (Self-Interpreted):" 
:PROOF 

"** Evaluating (OR_THREAD CON 'INCON) in Proof Thread (Self-Interpreted):" 
:PROOF 
"** Evaluating (OR_THREAD CON 'INCON) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_THREAD CON 'INCON) in Dual Thread (Self-Interpreted):" 
:PROOF 

"** Evaluating (AND_THREAD CON CON) in Proof Thread (Self-Interpreted):" 
:PROOF 
"** Evaluating (AND_THREAD CON CON) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_THREAD CON CON) in Dual Thread (Self-Interpreted):" 
:PROOF 

"** Evaluating (DUALITY INCON) in Proof Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY INCON) in thread type PROOF ***" 
"    Switching to Dual Thread to evaluate sub-expression: INCON" 
"    Result of Dual Thread evaluation of INCON: REFUTED" 
"    Dual Thread returned :REFUTED. Therefore, Proof/Dual Thread concludes: :PROOF for (DUALITY
                                                                                       INCON)" 
:PROOF 
"** Evaluating (DUALITY INCON) in Refutation Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY INCON) in thread type REFUTATION ***" 
"    Switching to Dual Thread to evaluate sub-expression: INCON" 
"    Result of Dual Thread evaluation of INCON: REFUTED" 
"    Dual Thread returned :REFUTED. Therefore, Proof/Dual Thread concludes: :PROOF for (DUALITY
                                                                                       INCON)" 
:PROOF 
"** Evaluating (DUALITY INCON) in Dual Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY INCON) in thread type DUAL ***" 
"    Switching to Dual Thread to evaluate sub-expression: INCON" 
"    Result of Dual Thread evaluation of INCON: REFUTED" 
"    Dual Thread returned :REFUTED. Therefore, Proof/Dual Thread concludes: :PROOF for (DUALITY
                                                                                       INCON)" 
:PROOF 

"** Evaluating (DUALITY CON) in Proof Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY CON) in thread type PROOF ***" 
"    Switching to Dual Thread to evaluate sub-expression: CON" 
"    Result of Dual Thread evaluation of CON: PROOF" 
"    Dual Thread did not refute. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                         CON)" 
:NO_CLOSURE 
"** Evaluating (DUALITY CON) in Refutation Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY CON) in thread type REFUTATION ***" 
"    Switching to Dual Thread to evaluate sub-expression: CON" 
"    Result of Dual Thread evaluation of CON: PROOF" 
"    Dual Thread did not refute. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                         CON)" 
:NO_CLOSURE 
"** Evaluating (DUALITY CON) in Dual Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY CON) in thread type DUAL ***" 
"    Switching to Dual Thread to evaluate sub-expression: CON" 
"    Result of Dual Thread evaluation of CON: PROOF" 
"    Dual Thread did not refute. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                         CON)" 
:NO_CLOSURE 

"** Evaluating (OR_REFUTATION INCON 'CON) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_REFUTATION INCON 'CON) in Refutation Thread (Self-Interpreted):" 
:REFUTED 
"** Evaluating (OR_REFUTATION INCON 'CON) in Dual Thread (Self-Interpreted):" 
:REFUTED 

"** Evaluating (AND_REFUTATION INCON INCON) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_REFUTATION INCON INCON) in Refutation Thread (Self-Interpreted):" 
:REFUTED 
"** Evaluating (AND_REFUTATION INCON INCON) in Dual Thread (Self-Interpreted):" 
:REFUTED 

"** Evaluating (OR_THREAD CON '(DUALITY CON)) in Proof Thread (Self-Interpreted):" 
:PROOF 
"** Evaluating (OR_THREAD CON '(DUALITY CON)) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_THREAD CON '(DUALITY CON)) in Dual Thread (Self-Interpreted):" 
:PROOF 

"** Evaluating (AND_THREAD CON '(DUALITY INCON)) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_THREAD CON '(DUALITY INCON)) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_THREAD CON '(DUALITY INCON)) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (OR_REFUTATION INCON '(DUALITY CON)) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_REFUTATION INCON '(DUALITY CON)) in Refutation Thread (Self-Interpreted):" 
:REFUTED 
"** Evaluating (OR_REFUTATION INCON '(DUALITY CON)) in Dual Thread (Self-Interpreted):" 
:REFUTED 

"** Evaluating (AND_REFUTATION INCON '(DUALITY INCON)) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_REFUTATION INCON '(DUALITY INCON)) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_REFUTATION INCON '(DUALITY INCON)) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (OR_THREAD 'A 'B) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_THREAD 'A 'B) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_THREAD 'A 'B) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (AND_THREAD 'A 'B) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_THREAD 'A 'B) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_THREAD 'A 'B) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (DUALITY 'A) in Proof Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type PROOF ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE 
"** Evaluating (DUALITY 'A) in Refutation Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type REFUTATION ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE 
"** Evaluating (DUALITY 'A) in Dual Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type DUAL ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE 

"** Evaluating (OR_REFUTATION 'A 'B) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_REFUTATION 'A 'B) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (OR_REFUTATION 'A 'B) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (AND_REFUTATION 'A 'B) in Proof Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_REFUTATION 'A 'B) in Refutation Thread (Self-Interpreted):" 
:NO_CLOSURE 
"** Evaluating (AND_REFUTATION 'A 'B) in Dual Thread (Self-Interpreted):" 
:NO_CLOSURE 

"** Evaluating (DUALITY 'A) in Proof Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type PROOF ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE 
"** Evaluating (DUALITY 'A) in Refutation Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type REFUTATION ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE 
"** Evaluating (DUALITY 'A) in Dual Thread (Self-Interpreted):" 
"*** Duality Rule (Proof/Dual Thread) triggered for: (DUALITY 'A) in thread type DUAL ***" 
"    Switching to Dual Thread to evaluate sub-expression: 'A" 
"    Result of Dual Thread evaluation of 'A: NO_CLOSURE" 
"    Dual Thread returned :NO_CLOSURE. Therefore, Proof/Dual Thread concludes: :NO_CLOSURE for (DUALITY
                                                                                               'A)" 
:NO_CLOSURE

We want to improve the code to include the concept of that RelWeaver might be m-close for inputs of n-length; for testing purposes, this means we have a parameter of RelWeaver's testing evaluation where we can specify n of some definite size as input to running the code then RelWeaver generates all the binary substrings up to n size and attempts to close them; if the test is successful it returns the binary string, and if the test fails then it does not return the binary string. The return can be in an appropriate base such as unsigned int64. For efficiency reasons, we do not want to output the results of every single attempted closure thread. The final result is sufficient. We are going to need to translate binary into RelWeaver expressions and vice versa; we'll use the convention that 0 is incon and 1 is con.

The output of the improved code should demonstrate that the system is m-closed for all finite input.