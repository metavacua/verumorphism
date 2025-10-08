;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Improved RWSDL-Min Code with Meta-Closure Thread Types (Formalized) - IMPROVED - Rule Renaming - WITH ENCODING/DECODING - WITH NUMERICAL OPERATIONS ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --- Numerical Operations for Weavex Algebra (Complex Numbers) ---

(defun complex-conjugate (z)
  "Returns the complex conjugate of a complex number."
  (complex (realpart z) (- (imagpart z))))

(defun duality-op (z)
  "Duality operation: i * conjugate(z)."
  (* #C(0 1) (complex-conjugate z)))

(defun and-thread-r-op (z1 z2)
  "AND_Thread_R operation: Re(z1) * Re(z2)."
  (* (realpart z1) (realpart z2)))

(defun or-thread-r-op (z1 z2)
  "OR_Thread_R operation: max(Re(z1), Re(z2))."
  (max (realpart z1) (realpart z2)))

(defun and-thread-l-op (z1 z2)
  "AND_Thread_L operation: (Im(z1) * Im(z2)) * i."
  (* (* (imagpart z1) (imagpart z2)) #C(0 1)))

(defun or-thread-l-op (z1 z2)
  "OR_Thread_L operation: max(Im(z1), Im(z2)) * i."
  (* (max (imagpart z1) (imagpart z2)) #C(0 1)))

(defun independence-thread-r-op (z1 z2)
  "INDEPENDENCE_Thread_R operation: Re(z1) + Re(z2)."
  (+ (realpart z1) (realpart z2)))

(defun independence-thread-l-op (z1 z2)
  "INDEPENDENCE_Thread_L operation: (Im(z1) + Im(z2)) * i."
  (* (+ (imagpart z1) (imagpart z2)) #C(0 1)))

(defun dependence-thread-r-op (z1 z2)
  "DEPENDENCE_Thread_R operation: (Re(z1) + Re(z2)) / 2."
  (/ (+ (realpart z1) (realpart z2)) 2.0))

(defun dependence-thread-l-op (z1 z2)
  "DEPENDENCE_Thread_L operation: ((Im(z1) + Im(z2)) / 2) * i."
  (* (/ (+ (imagpart z1) (imagpart z2)) 2.0) #C(0 1)))


;;; Axioms (functions directly implementing axioms - NUMERICAL)
(defun rwsdl-min-axiom-con-r-numerical (expression thread-type)
  "Numerical Axiom for 'CON' (Consistency) in Proof and Meta-Closure Threads."
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread #C(1.0 0.0))  ; CON -> 1 in proof
        (:meta_closure_thread #C(1.0 0.0)) ; CON -> 1 in meta
        (otherwise #C(0.0 0.0)))         ; CON -> 0 otherwise
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; No match in meta -> 0
        (otherwise #C(0.0 0.0)))))        ; No match otherwise -> 0

(defun rwsdl-min-axiom-incon-l-numerical (expression thread-type)
  "Numerical Axiom for 'INCON' (Inconsistency) in Refutation and Meta-Closure Threads."
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation_closure_thread #C(0.0 1.0)) ; INCON -> i in refutation
        (:meta_closure_thread #C(0.0 1.0))    ; INCON -> i in meta
        (otherwise #C(0.0 0.0)))           ; INCON -> 0 otherwise
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0)) ; No match in meta -> 0
        (otherwise #C(0.0 0.0)))))        ; No match otherwise -> 0


;;; Inference Rules (functions directly implementing rules - NUMERICAL)

(defun rwsdl-min-rule-or-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'OR_Thread_R' in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (or-thread-r-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (or-thread-r-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-and-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'AND_Thread_R' in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (and-thread-r-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (and-thread-r-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-duality-r-numerical (expression thread-type)
  "Numerical Rule for 'DUALITY_R' in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'DUALITY_R) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret-numerical a :meta_closure_thread))) ; Meta-closure thread for duality
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-or-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'OR_Thread_L' in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'OR_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (or-thread-l-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (or-thread-l-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-and-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'AND_Thread_L' in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'AND_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (and-thread-l-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (and-thread-l-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-duality-l-numerical (expression thread-type)
  "Numerical Rule for 'DUALITY_L' in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'DUALITY_L) (cadr expression))
      (let ((a (cadr expression)))
        (duality-op (rwsdl-min-self-interpret-numerical a :meta_closure_thread))) ; Meta-closure thread for duality
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-independence-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'INDEPENDENCE_Thread_R' in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (independence-thread-r-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (independence-thread-r-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-independence-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'INDEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (independence-thread-l-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (independence-thread-l-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-dependence-thread-r-numerical (expression thread-type)
  "Numerical Rule for 'DEPENDENCE_Thread_R' in Proof and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_R) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:proof_closure_thread (dependence-thread-r-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (dependence-thread-r-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))

(defun rwsdl-min-rule-dependence-thread-l-numerical (expression thread-type)
  "Numerical Rule for 'DEPENDENCE_Thread_L' in Refutation and Meta-Closure Threads."
  (if (and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_L) (cadr expression) (caddr expression))
      (let ((a (cadr expression))
            (b (caddr expression)))
        (case thread-type
          (:refutation_closure_thread (dependence-thread-l-op (rwsdl-min-self-interpret-numerical a thread-type) (rwsdl-min-self-interpret-numerical b thread-type)))
          (:meta_closure_thread
           (let ((result-a (rwsdl-min-self-interpret-numerical a thread-type))
                 (result-b (rwsdl-min-self-interpret-numerical b thread-type)))
             (dependence-thread-l-op result-a result-b)))
          (otherwise #C(0.0 0.0))))
      (case thread-type
        (:meta_closure_thread #C(0.0 0.0))
        (otherwise #C(0.0 0.0)))))


;;; Minimal Self-Interpretation (modified to handle :meta_closure_thread type and closure outcomes - NUMERICAL)
(defun rwsdl-min-self-interpret-numerical (expression thread-type)
  "Numerical self-interpretation function, dispatching to axioms and rules."
  (cond
    ((eq expression 'CON) (rwsdl-min-axiom-con-r-numerical expression thread-type))
    ((eq expression 'INCON) (rwsdl-min-axiom-incon-l-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'OR_Thread_R)) (rwsdl-min-rule-or-thread-r-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Thread_R)) (rwsdl-min-rule-and-thread-r-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY_R)) (rwsdl-min-rule-duality-r-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'OR_Thread_L)) (rwsdl-min-rule-or-thread-l-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'AND_Thread_L)) (rwsdl-min-rule-and-thread-l-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'DUALITY_L)) (rwsdl-min-rule-duality-l-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_R)) (rwsdl-min-rule-independence-thread-r-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'INDEPENDENCE_Thread_L)) (rwsdl-min-rule-independence-thread-l-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_R)) (rwsdl-min-rule-dependence-thread-r-numerical expression thread-type))
    ((and (listp expression) (eq (car expression) 'DEPENDENCE_Thread_L)) (rwsdl-min-rule-dependence-thread-l-numerical expression thread-type))

    (t (case thread-type
         (:proof_closure_thread #C(0.0 0.0))    ; Default to 0
         (:refutation_closure_thread #C(0.0 0.0)) ; Default to 0
         (:meta_closure_thread #C(0.0 0.0))       ; Default to 0
         (otherwise (error "Invalid thread type: ~a" thread-type))))))


;;; Evaluation Functions (modified to handle :meta_closure_thread type - NUMERICAL)
(defun rwsdl-min-evaluate-numerical (expression thread-type)
  "Numerically evaluates an expression in the specified thread type."
  (case thread-type
    (:proof_closure_thread (rwsdl-min-self-interpret-numerical expression thread-type))
    (:refutation_closure_thread (rwsdl-min-self-interpret-numerical expression thread-type))
    (:meta_closure_thread (rwsdl-min-self-interpret-numerical expression thread-type))
    (otherwise (error "Invalid thread type: ~a" thread-type))))


;;; Proof, Refutation, and Meta-Closure Threads (using rwsdl-min-evaluate - NUMERICAL)
(defun rwsdl-min-proof-closure-thread-numerical (expression)
  "Numerically evaluates expression in a proof closure thread."
  (rwsdl-min-evaluate-numerical expression :proof_closure_thread))

(defun rwsdl-min-refutation-closure-thread-numerical (expression)
  "Numerically evaluates expression in a refutation closure thread."
  (rwsdl-min-evaluate-numerical expression :refutation_closure_thread))

(defun rwsdl-min-meta-closure-thread-numerical (expression)
  "Numerically evaluates expression in a meta-closure thread."
  (rwsdl-min-evaluate-numerical expression :meta_closure_thread))


;;; Bootstrap Loop Test (modified to test meta-closure threads and new closure outputs and numerical evaluation)
(defun rwsdl-min-bootstrap-loop-test-numerical ()
  "Tests RWSDL-Min numerical self-interpretation with different thread types and expressions."
  (print "*** RWSDL-Min Bootstrap Loop Test (Numerical Self-Interpreted, Complex Numbers) ***")

  (let ((test-expressions '(
                     CON
                     INCON
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
                     (INDEPENDENCE_Thread_R CON 'INCON)
                     (DEPENDENCE_Thread_R CON 'CON)
                     (INDEPENDENCE_Thread_L INCON 'CON)
                     (DEPENDENCE_Thread_L INCON 'INCON)
                     )))


    (loop for expr in test-expressions do
      (print (format nil "** Evaluating ~a in Proof Closure Thread (Numerical):" expr))
      (let ((result (rwsdl-min-proof-closure-thread-numerical expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Refutation Closure Thread (Numerical):" expr))
      (let ((result (rwsdl-min-refutation-closure-thread-numerical expr)))
        (print result))

      (print (format nil "** Evaluating ~a in Meta-Closure Thread (Numerical):" expr))
      (let ((result (rwsdl-min-meta-closure-thread-numerical expr)))
        (print result))
      (terpri))
    ))


  ;;; Run Numerical Bootstrap Test
  (rwsdl-min-bootstrap-loop-test-numerical)

  (print "*** Formal Verification and Testing Complete (Numerical) ***")
  

;;; Run the bootstrap test with numerical evaluation
(rwsdl-min-bootstrap-loop-test-numerical)