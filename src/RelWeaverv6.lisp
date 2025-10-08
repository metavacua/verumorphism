;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Improved RWSDL-Min Code with Meta-Closure Thread Types (Formalized) - IMPROVED - Rule Renaming - WITH ENCODING/DECODING ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Axioms (functions directly implementing axioms)
(defun rwsdl-min-axiom-con-r (expression thread-type)
  "Axiom for 'CON' (Consistency) in Proof and Meta-Closure Threads."
  (if (eq expression 'CON)
      (case thread-type
        (:proof_closure_thread :p_close)   ; In proof closure thread, CON is proof-closed
        (:meta_closure_thread :p_close)  ; In meta-closure thread, CON is also proof-closed (acts like proof thread for axioms)
        (otherwise :np_close))         ; In other threads, no proof-closure
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if axiom doesn't match, it's no-closure
        (otherwise :np_close))))          ; Otherwise, no proof-closure

(defun rwsdl-min-axiom-incon-l (expression thread-type)
  "Axiom for 'INCON' (Inconsistency) in Refutation and Meta-Closure Threads."
  (if (eq expression 'INCON)
      (case thread-type
        (:refutation_closure_thread :r_close) ; In refutation closure thread, INCON is refutation-closed
        (:meta_closure_thread :r_close)  ; In meta-closure thread, INCON is also refutation-closed (acts like refutation thread for axioms)
        (otherwise :nr_close))         ; In other threads, no refutation-closure
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if axiom doesn't match, it's no-closure
        (otherwise :nr_close))))          ; Otherwise, no refutation-closure


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
               (t :no_close))))          ; No closure if neither branch proves in meta-closure thread
          (otherwise :np_close)))         ; No proof-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))          ; Otherwise, no proof-closure

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
               (t :no_close))))          ; No closure otherwise in meta-closure thread
          (otherwise :np_close)))         ; No proof-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))          ; Otherwise, no proof-closure

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
               :no_close))           ; If meta-closure no-closure, duality in proof thread is no-closure
            (t  ; Default case if dual thread returns :np_close or anything else non-refuted/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not refute. Therefore, Proof/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))         ; Default to no-closure if meta-closure doesn't refute
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :np_close))))          ; Otherwise, no proof-closure

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
               (t :no_close))))          ; No closure if neither branch refutes in meta-closure thread
          (otherwise :nr_close)))         ; No refutation-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))          ; Otherwise, no refutation-closure


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
               (t :no_close))))          ; No closure otherwise in meta-closure thread
          (otherwise :nr_close)))         ; No refutation-closure in other thread types
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))          ; Otherwise, no refutation-closure

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
               :no_close))           ; If meta-closure no-closure, duality in refutation thread is no-closure
            (t  ; Default case if dual thread returns :nr_close or anything else non-proof/non-no_closure
             (progn
               (print (format nil "    Meta-Closure Thread did not prove. Therefore, Refutation/Meta-Closure Thread concludes: :NO_CLOSE for ~a" expression)) ; Changed to :NO_CLOSE
               :no_close)))))         ; Default to no-closure if meta-closure doesn't prove
      (case thread-type
        (:meta_closure_thread :no_close) ; For meta-closure threads, if rule doesn't match, it's no-closure
        (otherwise :nr_close))))          ; Otherwise, no refutation-closure


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
         (:refutation_closure_thread :nr_close)  ; Default to no refutation-closure
         (:meta_closure_thread :no_close)       ; Default for meta-closure threads is no-closure
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


;;; Encoding Functions

(defun weavex-to-signed-int (weavex-expr)
  "Encodes a weavex expression to a signed integer."
  (cond
    ((eq weavex-expr 'CON) 1)
    ((eq weavex-expr 'INCON) -1)
    (t 0))) ; Default to 0 for complex or unhandled expressions

(defun signed-int-to-weavex (signed-int-val)
  "Decodes a signed integer to a weavex expression."
  (cond
    ((= signed-int-val 1) 'CON)
    ((= signed-int-val -1) 'INCON)
    ((= signed-int-val 0) ''NO_CLOSE_REPRESENTATION) ; Symbolic representation for no-close
    (t ''UNKNOWN_WEAVEX_REPRESENTATION))) ; Symbolic for unknown

(defun right-weavex-to-unsigned-int (right-weavex-expr)
  "Encodes a right-weavex expression to an unsigned integer."
  (if (eq right-weavex-expr 'CON)
      1
      0)) ; Default to 0 for no proof-closure

(defun unsigned-int-to-right-weavex (unsigned-int-val)
  "Decodes an unsigned integer to a right-weavex expression."
  (if (> unsigned-int-val 0) ; Assuming any non-zero is proof
      'CON
      ''NO_PROOF_REPRESENTATION)) ; Symbolic for no proof

(defun left-weavex-to-unsigned-int (left-weavex-expr)
  "Encodes a left-weavex expression to an unsigned integer."
  (if (eq left-weavex-expr 'INCON)
      1
      0)) ; Default to 0 for no refutation-closure

(defun unsigned-int-to-left-weavex (unsigned-int-val)
  "Decodes an unsigned integer to a left-weavex expression."
  (if (> unsigned-int-val 0) ; Assuming any non-zero is refutation
      'INCON
      ''NO_REFUTATION_REPRESENTATION)) ; Symbolic for no refutation

(defun regex-to-right-weavex (regex-expr)
  "Encodes a regex expression to a right-weavex expression (basic)."
  (cond
    ((string= regex-expr "CON") 'CON) ; Example: Regex "CON" to Weavex CON
    (t ''REGEX_NOT_HANDLED)))      ; Placeholder for unhandled regex

(defun classical-to-weavex (classical-expr thread-type)
  "Encodes a classical expression to a weavex expression (basic for OR/AND)."
  (cond
    ((and (listp classical-expr) (eq (car classical-expr) 'OR) (cadr classical-expr) (caddr classical-expr))
     (case thread-type
       (:right `(OR_Thread_R ,(classical-to-weavex (cadr classical-expr) thread-type) ,(classical-to-weavex (caddr classical-expr) thread-type)))
       (:left `(OR_Thread_L ,(classical-to-weavex (cadr classical-expr) thread-type) ,(classical-to-weavex (caddr classical-expr) thread-type)))
       (otherwise ''CLASSICAL_OR_NOT_HANDLED)))
    ((and (listp classical-expr) (eq (car classical-expr) 'AND) (cadr classical-expr) (caddr classical-expr))
     (case thread-type
       (:right `(AND_Thread_R ,(classical-to-weavex (cadr classical-expr) thread-type) ,(classical-to-weavex (caddr classical-expr) thread-type)))
       (:left `(AND_Thread_L ,(classical-to-weavex (cadr classical-expr) thread-type) ,(classical-to-weavex (caddr classical-expr) thread-type)))
       (otherwise ''CLASSICAL_AND_NOT_HANDLED)))
    ((eq classical-expr 'A) ''A) ; Atomic proposition - needs to be handled in context
    ((eq classical-expr 'B) ''B) ; Atomic proposition - needs to be handled in context
    (t ''CLASSICAL_EXPR_NOT_HANDLED))) ; Placeholder

(defun right-weavex-to-classical (right-weavex-expr)
  "Decodes a right-weavex expression to a classical expression (basic for OR/AND)."
  (cond
    ((and (listp right-weavex-expr) (eq (car right-weavex-expr) 'OR_Thread_R) (cadr right-weavex-expr) (caddr right-weavex-expr))
     `(OR ,(right-weavex-to-classical (cadr right-weavex-expr)) ,(right-weavex-to-classical (caddr right-weavex-expr))))
    ((and (listp right-weavex-expr) (eq (car right-weavex-expr) 'AND_Thread_R) (cadr right-weavex-expr) (caddr right-weavex-expr))
     `(AND ,(right-weavex-to-classical (cadr right-weavex-expr)) ,(right-weavex-to-classical (caddr right-weavex-expr))))
    ((eq right-weavex-expr ''A) 'A) ; Atomic proposition
    ((eq right-weavex-expr ''B) 'B) ; Atomic proposition
    ((eq right-weavex-expr 'CON) 'TRUE)
    (t ''RIGHT_WEAVEX_NOT_HANDLED))) ; Placeholder


;;; Bootstrap Loop Test (modified to test meta-closure threads and new closure outputs and encoding/decoding)
(defun rwsdl-min-bootstrap-loop-test ()
  "Tests RWSDL-Min self-interpretation with different thread types and expressions, and encoding/decoding."
  (print "*** RWSDL-Min Bootstrap Loop Test (Self-Interpreted, Corrected Rules, Meta-Closure Threads, Renamed Rules, Encoding/Decoding) ***")

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
                   (DUALITY_L 'A)         ; Corrected to DUALITY_L for symmetry in test expressions
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
    )

    (print "*** Encoding/Decoding Tests ***")

    (let ((weavex-expr 'CON))
      (print (format nil "** Weavex to Signed Int Encoding for ~a: ~a" weavex-expr (weavex-to-signed-int weavex-expr)))
      (print (format nil "** Signed Int to Weavex Decoding for 1: ~a" (signed-int-to-weavex 1))))

    (let ((weavex-expr 'INCON))
      (print (format nil "** Weavex to Signed Int Encoding for ~a: ~a" weavex-expr (weavex-to-signed-int weavex-expr)))
      (print (format nil "** Signed Int to Weavex Decoding for -1: ~a" (signed-int-to-weavex -1))))

    (let ((weavex-expr '(OR_Thread_R 'A 'B)))
      (print (format nil "** Weavex to Signed Int Encoding for ~a: ~a" weavex-expr (weavex-to-signed-int weavex-expr))) ; Expect 0 for complex expression in basic mapping
      (print (format nil "** Signed Int to Weavex Decoding for 0: ~a" (signed-int-to-weavex 0))))

    (let ((right-weavex 'CON))
      (print (format nil "** Right-Weavex to Unsigned Int Encoding for ~a: ~a" right-weavex (right-weavex-to-unsigned-int right-weavex)))
      (print (format nil "** Unsigned Int to Right-Weavex Decoding for 1: ~a" (unsigned-int-to-right-weavex 1))))

    (let ((left-weavex 'INCON))
      (print (format nil "** Left-Weavex to Unsigned Int Encoding for ~a: ~a" left-weavex (left-weavex-to-unsigned-int left-weavex)))
      (print (format nil "** Unsigned Int to Left-Weavex Decoding for 1: ~a" (unsigned-int-to-left-weavex 1))))

    (let ((regex-expr "a|b&c")) ; Example regex with alternation and concatenation
      (print (format nil "** Regex to Right-Weavex Encoding for ~a: ~a" regex-expr (regex-to-right-weavex regex-expr))))

    (let ((classical-or '(OR A B)))
      (print (format nil "** Classical to Right-Weavex Encoding for ~a: ~a" classical-or (classical-to-weavex classical-or :right)))
      (print (format nil "** Classical to Left-Weavex Encoding for ~a: ~a" classical-or (classical-to-weavex classical-or :left))))

    (let ((weavex-or-r '(OR_Thread_R 'A 'B)))
      (print (format nil "** Right-Weavex to Classical Decoding for ~a: ~a" weavex-or-r (right-weavex-to-classical weavex-or-r))))


    )

  ;;; Run Bootstrap Test
  (rwsdl-min-bootstrap-loop-test)