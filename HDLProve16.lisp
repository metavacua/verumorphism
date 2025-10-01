;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refactored RelNet Weaver Core (Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structures (Minimal for Prototype) - Enhanced Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass relnet-node ()
  ((name :initarg :name :accessor relnet-node-name)
   (type :initarg :type :accessor relnet-node-type)))

(defvar *knowledge-base* nil "Global Knowledge Base (Minimal for Prototype)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complexity Metrics - Global Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *axiom-applications-count* 0 "Counter for axiom applications.")
(defvar *rule-applications-count* 0 "Counter for rule applications.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms (con_R and incon_L - Minimal for Prototype) - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-con-r (kb)
  "Proof Axiom (axiom con_R (() con)). Minimal implementation.
   Represents an axiomatically proven sequent/context.
   [Complexity Metric: axiom-applications-count]"
  (declare (ignore kb))
  (incf *axiom-applications-count*) ;; Increment axiom application counter
  (format t "Proof Thread: Applying con_R axiom - Axiomatically Proven.~%")
  :proven)

(defun axiom-incon-l (kb)
  "Refutation Axiom (axiom incon_L (incon ())). Minimal implementation.
   Represents an axiomatically refuted sequent/context.
   [Complexity Metric: axiom-applications-count]"
  (declare (ignore kb))
  (incf *axiom-applications-count*) ;; Increment axiom application counter
  (format t "Refutation Thread: Applying incon_L axiom - Axiomatically Refuted.~%")
  :refuted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence - Minimal for Prototype) - THREADED INDEPENDENCE RULES - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-dependence-r (kb)
  "Dependence Right Rule (rule dependenceR). Sequential AND in Proof.
   [Complexity Metric: rule-applications-count]"
  (format t "Proof Thread: Attempting rule dependenceR (Dependence Right).~%")
  (incf *rule-applications-count*) ;; Increment rule application counter
  (let ((proof1-result (axiom-con-r kb))  ; Thread 1: Attempt to prove premise 1
        (proof2-result (axiom-con-r kb)))  ; Thread 2: Attempt to prove premise 2
    (if (and (eq proof1-result :proven) (eq proof2-result :proven))  ; Both premises must be proven
        (progn
          (format t "Proof Thread: Rule dependenceR applied successfully - Proven (dependence con con).~%")
          :rule-applied)          ; Rule applied successfully, context is now proven (collapsed)
        (progn
          (format t "Proof Thread: Rule dependenceR failed to apply.~%")
          nil))))                  ; Rule failed to apply

(defun rule-dependence-l (kb)
  "Dependence Left Rule (rule dependenceL). Sequential AND in Refutation.
   [Complexity Metric: rule-applications-count]"
  (format t "Refutation Thread: Attempting rule dependenceL (Dependence Left).~%")
  (incf *rule-applications-count*) ;; Increment rule application counter
  (let ((refute1-result (axiom-incon-l kb)) ; Thread 1: Attempt to refute premise 1
        (refute2-result (axiom-incon-l kb))) ; Thread 2: Attempt to refute premise 2
    (if (and (eq refute1-result :refuted) (eq refute2-result :refuted))  ; Both premises must be refuted
        (progn
          (format t "Refutation Thread: Rule dependenceL applied successfully - Refuted (dependence incon incon).~%")
          :rule-applied)          ; Rule applied successfully, context is now refuted (collapsed)
        (progn
          (format t "Refutation Thread: Rule dependenceL failed to apply.~%")
          nil))))                  ; Rule failed to apply


(defun rule-independence-r (kb &key axiom-con)
  "Independence Right Rule (rule independenceR) - THREADED. Concurrent OR in Proof.
   [Complexity Metric: rule-applications-count]"
  (format t "Proof Thread: Attempting rule independenceR (Independence Right) - THREADED.~%")
  (incf *rule-applications-count*) ;; Increment rule application counter
  (let ((premise1-result nil)
        (premise2-result nil)
        (thread1 nil)
        (thread2 nil))

    ;; Thread 1 for premise 1 (alternative proof path 1)
    (setf thread1 (sb-thread:make-thread
                   (lambda ()
                     (setf premise1-result (funcall (or axiom-con #'axiom-con-r) kb)))
                   :name "IND-R-Premise1-Thread"))

    ;; Thread 2 for premise 2 (alternative proof path 2)
    (setf thread2 (sb-thread:make-thread
                   (lambda ()
                     (setf premise2-result (funcall (or axiom-con #'axiom-con-r) kb)))
                   :name "IND-R-Premise2-Thread"))

    ;; Wait for threads to finish (both explore in parallel)
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)

    (format t "Proof Thread: Both premises of *IND*R evaluated in parallel.~%")

    (if (eq premise1-result :proven)  ; If premise 1 is proven, rule succeeds (OR)
        (progn
          (format t "Proof Thread: Rule independenceR applied successfully - Proven (independence con X) (via first premise thread).~%")
          (return-from rule-independence-r (values :rule-applied :premise1-satisfied)))
        (if (eq premise2-result :proven)  ; If premise 2 is proven, rule succeeds (OR)
            (progn
              (format t "Proof Thread: Rule independenceR applied successfully - Proven (independence X con) (via second premise thread).~%")
              (return-from rule-independence-r (values :rule-applied :premise2-satisfied)))
            (progn
              (format t "Proof Thread: Rule independenceR failed to apply.~%")
              (return-from rule-independence-r (values nil nil))))))) ; Rule fails if neither premise is proven


(defun rule-independence-l (kb &key axiom-incon)
  "Independence Left Rule (rule independenceL) - THREADED. Concurrent OR in Refutation.
   [Complexity Metric: rule-applications-count]"
  (format t "Refutation Thread: Attempting rule independenceL (Independence Left) - THREADED.~%")
  (incf *rule-applications-count*) ;; Increment rule application counter
  (let ((premise1-result nil)
        (premise2-result nil)
        (thread1 nil)
        (thread2 nil))

    ;; Thread 1 for premise 1 (alternative refutation path 1)
    (setf thread1 (sb-thread:make-thread
                   (lambda ()
                     (setf premise1-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
                   :name "IND-L-Premise1-Thread"))

    ;; Thread 2 for premise 2 (alternative refutation path 2)
    (setf thread2 (sb-thread:make-thread
                   (lambda ()
                     (setf premise2-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
                   :name "IND-L-Premise2-Thread"))

    ;; Wait for threads to finish (both explore in parallel)
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)

    (format t "Refutation Thread: Both premises of *IND*L evaluated in parallel.~%")

    (if (eq premise1-result :refuted)  ; If premise 1 is refuted, rule succeeds (OR for refutation)
        (progn
          (format t "Refutation Thread: Rule independenceL applied successfully - Refuted (independence incon X) (via first premise thread).~%")
          (return-from rule-independence-l (values :rule-applied :premise1-satisfied)))
        (if (eq premise2-result :refuted)  ; If premise 2 is refuted, rule succeeds (OR for refutation)
            (progn
              (format t "Refutation Thread: Rule independenceL applied successfully - Refuted (independence X incon) (via second premise thread).~%")
              (return-from rule-independence-l (values :rule-applied :premise2-satisfied)))
            (progn
              (format t "Refutation Thread: Rule independenceL failed to apply (neither premise refuted).~%")
              (return-from rule-independence-l (values nil nil))))))) ; Rule fails if neither premise is refuted


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread Functions (Proof and Refutation - Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *proof-result* nil "Variable to store proof thread result")
(defvar *refutation-result* nil "Variable to store refutation thread result")
(defvar *termination-flag* nil "Flag to signal termination to both threads")


(defun proof-thread-function ()
  "Proof Thread: Attempts to find a proof using axioms and rules.
   Sequential process: Tries con_R, then *DEP*R, then *IND*R in order.
   Sets *proof-result* and *termination-flag* upon finding a proof or exhausting rules."
  (format t "Proof Thread: Starting.~%")

  ;; 1. Try axiom con_R (Axiomatic Proof - Immediate termination if successful)
  (let ((axiom-con-r-result (axiom-con-r *knowledge-base*)))
    (when (eq axiom-con-r-result :proven)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via con_R axiom).~%")
      (return-from proof-thread-function :proven)))

  ;; 2. Try rule *DEP*R (Dependence Right Rule - Sequential AND in Proof)
  (let ((rule-dep-r-result (rule-dependence-r *knowledge-base*)))
    (when (eq rule-dep-r-result :rule-applied)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via rule dependenceR).~%")
      (return-from proof-thread-function :proven)))

  ;; 3. Try rule *IND*R (Independence Right Rule - Concurrent OR in Proof - THREADED)
  (multiple-value-bind (rule-ind-r-result premise-satisfied)
        (rule-independence-r *knowledge-base*)
    (declare (ignore premise-satisfied))
    (when (eq rule-ind-r-result :rule-applied)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via rule independenceR).~%")
      (return-from proof-thread-function :proven)))


  ;; 4. No proof found in this iteration (Exhausted rules)
  (format t "Proof Thread: No proof found in this iteration.~%")
  (setf *proof-result* :unknown)
  (setf *termination-flag* :proof-terminated)
  (format t "Proof Thread: Terminating (no proof).~%")
  :unknown)



(defun refutation-thread-function ()
  "Refutation Thread: Attempts to find a refutation using axioms and rules.
   Sequential process: Tries incon_L, then *DEP*L, then *IND*L in order.
   Sets *refutation-result* and *termination-flag* upon finding a refutation or exhausting rules."
  (format t "Refutation Thread: Starting.~%")

  ;; 1. Try axiom incon_L (Axiomatic Refutation - Immediate termination if successful)
  (let ((axiom-incon-l-result (axiom-incon-l *knowledge-base*)))
    (when (eq axiom-incon-l-result :refuted)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via incon_L axiom).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 2. Try rule *DEP*L (Dependence Left Rule - Sequential AND in Refutation)
  (let ((rule-dep-l-result (rule-dependence-l *knowledge-base*)))
    (when (eq rule-dep-l-result :rule-applied)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via rule dependenceL).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 3. Try rule *IND*L (Independence Left Rule - Concurrent OR in Refutation - THREADED)
  (multiple-value-bind (rule-ind-l-result premise-satisfied)
        (rule-independence-l *knowledge-base*)
    (declare (ignore premise-satisfied))
    (when (eq rule-ind-l-result :rule-applied)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via rule independenceL).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 4. No refutation found in this iteration (Exhausted rules)
  (format t "Refutation Thread: No refutation found in this iteration.~%")
  (setf *refutation-result* :unknown)
  (setf *termination-flag* :refutation-terminated)
  (format t "Refutation Thread: Terminating (no refutation).~%")
  :unknown)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parallel Interface and Orchestration (Barebones Prototype) - COMPLEXITY REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-knowledge-base ()
  "Initializes the global *knowledge-base* (minimal for prototype) and resets complexity counters."
  (setf *knowledge-base* nil)
  (reset-complexity-counters))  ;; Reset complexity counters at the start

(defun reset-complexity-counters ()
  "Resets all complexity counters to 0."
  (setf *axiom-applications-count* 0)
  (setf *rule-applications-count* 0))


(defun run-prover ()
  "Runs the barebones theorem prover prototype with two threads (proof and refutation).
   Orchestrates parallel proof and refutation attempts and determines the overall prover result.
   [Complexity Reporting: axiom-applications-count, rule-applications-count]"
  (format t "Prover: Initializing Knowledge Base.~%")
  (initialize-knowledge-base)

  (format t "Prover: Starting Proof and Refutation Threads.~%")
  (let ((proof-thread (sb-thread:make-thread #'proof-thread-function :name "Proof-Thread"))
        (refutation-thread (sb-thread:make-thread #'refutation-thread-function :name "Refutation-Thread")))

    (format t "Prover: Waiting for termination signal from threads.~%")
    (loop until *termination-flag*  ; Wait until either thread sets the termination flag
          do (sleep 0.1))

    (format t "Prover: Termination signal received: ~A~%" *termination-flag*)

    (cond ((eq *termination-flag* :proof-terminated)  ; Proof thread terminated first
           (format t "Prover: Proof Thread terminated first. Result: Proven.~%")
           :proven)          ; Prover result is PROVEN
          ((eq *termination-flag* :refutation-terminated) ; Refutation thread terminated first
           (format t "Prover: Refutation Thread terminated first. Result: Refuted.~%")
           :refuted)          ; Prover result is REFUTED
          (t            ; Unexpected termination state
           (format t "Prover: Unexpected termination state.~%")
           :unknown))))      ; Prover result is UNKNOWN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run the Prover Prototype - COMPLEXITY REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (format t "Starting Barebones Theorem Prover Prototype (Refactored - Threaded Independence Rules).~%")

  (let ((prover-result (run-prover)))
    (format t "~%Prover Result: ~A~%" prover-result)
    (format t "~%--- Complexity Metrics ---~%")
    (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
    (format t "Rule Applications:  ~A~%" *rule-applications-count*)
    (format t "Barebones Theorem Prover Prototype Finished.~%")))

(main)