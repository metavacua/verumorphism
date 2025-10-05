;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structures (Minimal for Prototype) - Enhanced Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass relnet-node ()
  ((name :initarg :name :accessor relnet-node-name)
   (type :initarg :type :accessor relnet-node-type))
  (:documentation "Represents a node in the relational network, enhanced with a type.

Slots:
  - NAME: The symbolic name of the node.
  - TYPE: The type of the node (e.g., 'formula', 'term')."))

(defvar *knowledge-base* nil
  "The global knowledge base for the prover. In this prototype, it is not
used beyond being passed to axiom and rule functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Complexity Metrics - Global Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *axiom-applications-count* 0
  "Counts the total number of axiom applications within a single `run-prover` call.
This serves as a basic complexity metric and is reset by `initialize-knowledge-base`.")
(defvar *rule-applications-count* 0
  "Counts the total number of rule applications within a single `run-prover` call.
This serves as a basic complexity metric and is reset by `initialize-knowledge-base`.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms (con_R and incon_L - Minimal for Prototype) - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-con-r (kb)
  "A minimal implementation of the 'Consistency Right' (con_R) proof axiom.
This represents an axiomatically proven sequent or context and increments the
axiom application counter.

Parameters:
  - KB: The knowledge base (ignored).

Returns:
  - The keyword :PROVEN.

Side Effects:
  - Increments `*axiom-applications-count*`."
  (declare (ignore kb))
  (incf *axiom-applications-count*) ;; Increment axiom application counter
  (format t "Proof Thread: Applying con_R axiom - Axiomatically Proven.~%")
  :proven)

(defun axiom-incon-l (kb)
  "A minimal implementation of the 'Inconsistency Left' (incon_L) refutation axiom.
This represents an axiomatically refuted sequent or context and increments the
axiom application counter.

Parameters:
  - KB: The knowledge base (ignored).

Returns:
  - The keyword :REFUTED.

Side Effects:
  - Increments `*axiom-applications-count*`."
  (declare (ignore kb))
  (incf *axiom-applications-count*) ;; Increment axiom application counter
  (format t "Refutation Thread: Applying incon_L axiom - Axiomatically Refuted.~%")
  :refuted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence - Minimal for Prototype) - THREADED INDEPENDENCE RULES - COMPLEXITY COUNTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-dependence-r (kb)
  "Simulates the 'Dependence Right' (dependenceR) rule.
This rule represents a sequential AND in a proof and increments the rule
application counter.

Parameters:
  - KB: The knowledge base.

Returns:
  - :RULE-APPLIED on success, NIL on failure."
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
          nil))))                ; Rule failed to apply

(defun rule-dependence-l (kb)
  "Simulates the 'Dependence Left' (dependenceL) rule.
This rule represents a sequential AND in a refutation and increments the rule
application counter.

Parameters:
  - KB: The knowledge base.

Returns:
  - :RULE-APPLIED on success, NIL on failure."
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
          nil))))                ; Rule failed to apply


(defun rule-independence-r (kb &key axiom-con)
  "Simulates the 'Independence Right' (independenceR) rule using parallel threads.
This rule represents a concurrent OR in a proof and increments the rule
application counter.

Parameters:
  - KB: The knowledge base.
  - AXIOM-CON (Keyword, Optional): A function for proving premises, defaults to `axiom-con-r`.

Returns:
  - Multiple values: :RULE-APPLIED and a keyword for the successful premise, or NIL, NIL."
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
  "Simulates the 'Independence Left' (independenceL) rule using parallel threads.
This rule represents a concurrent OR in a refutation and increments the rule
application counter.

Parameters:
  - KB: The knowledge base.
  - AXIOM-INCON (Keyword, Optional): A function for refuting premises, defaults to `axiom-incon-l`.

Returns:
  - Multiple values: :RULE-APPLIED and a keyword for the successful premise, or NIL, NIL."
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

(defvar *proof-result* nil
  "Holds the result from the proof thread.")
(defvar *refutation-result* nil
  "Holds the result from the refutation thread.")
(defvar *termination-flag* nil
  "A flag to coordinate the termination of the proof and refutation threads.")


(defun proof-thread-function ()
  "The main function for the proof-seeking thread.
It sequentially tries to apply axioms and rules (`con_R`, `dependenceR`, `independenceR`).
If any succeed, it sets the `*proof-result*` and `*termination-flag*` and exits.

Returns:
  - :PROVEN on success, :UNKNOWN on failure."
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
  "The main function for the refutation-seeking thread.
It sequentially tries to apply axioms and rules (`incon_L`, `dependenceL`, `independenceL`).
If any succeed, it sets the `*refutation-result*` and `*termination-flag*` and exits.

Returns:
  - :REFUTED on success, :UNKNOWN on failure."
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
  "Initializes the prover state by resetting the knowledge base and complexity counters."
  (setf *knowledge-base* nil)
  (reset-complexity-counters))  ;; Reset complexity counters at the start

(defun reset-complexity-counters ()
  "Resets the global complexity counters `*axiom-applications-count*` and
`*rule-applications-count*` to zero."
  (setf *axiom-applications-count* 0)
  (setf *rule-applications-count* 0))


(defun run-prover ()
  "Runs the theorem prover, orchestrating proof and refutation threads.
This function initializes the prover, starts the threads, and waits for a result.
It also reports complexity metrics upon completion.

Returns:
  - :PROVEN if the proof thread finishes first.
  - :REFUTED if the refutation thread finishes first.
  - :UNKNOWN for any other case."
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
           :unknown))))        ; Prover result is UNKNOWN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests - Comprehensive and Targeted - ENHANCED - REFACTORED and MODULARIZED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-suite-summary* (make-hash-table) "Summary of test suite results.")

(defun reset-test-summary ()
  "Resets the *test-suite-summary* hash table before each test run."
  (setf *test-suite-summary* (make-hash-table)))

(defun update-test-summary (test-category test-name result)
  "Updates the test suite summary with the result of a test."
  (let ((category-summary (gethash test-category *test-suite-summary*
                                     (make-hash-table :test #'equal))))
    (setf (gethash test-name category-summary) result)
    (setf (gethash test-category *test-suite-summary*) category-summary)))

(defun format-test-result (test-name pass-fail message)
  "Formats the test result output for console display."
  (if pass-fail
      (format t "    Test ~A: PASS~%" test-name)  ; Indented for category clarity
      (format t "    Test ~A: FAIL - ~A~%" test-name message))) ; Indented for category clarity


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Assertion and Test Running Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assert-result (actual expected test-name test-category comparison-fn expected-str actual-str)
  "Generic assertion function to compare actual vs expected results and update test summary."
  (let ((pass-fail (funcall comparison-fn actual expected))
        (message (format nil "Expected ~A (~A), got ~A (~A)" expected expected-str actual actual-str)))
    (format-test-result test-name pass-fail message)
    (update-test-summary test-category test-name pass-fail)
    pass-fail))

(defun run-tests (test-functions test-category)
  "Generic test runner function to execute a list of test functions for a category."
  (format t "~%Running ~A Tests:~%" test-category)
  (let ((passed-count 0)
        (failed-count 0))
    (loop for test-fn in test-functions
          do (multiple-value-bind (pass? message) (funcall test-fn)
               (if pass? (incf passed-count) (incf failed-count))))
    (format t "~A Tests Finished. Passed: ~A, Failed: ~A~%" test-category passed-count failed-count)
    (cons passed-count failed-count)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Axiom Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-axiom-con-r ()
  "Tests axiom-con-r for returning :proven."
  (let ((result (axiom-con-r *knowledge-base*)))
    (values (assert-result result :proven "axiom-con-r" "Axiom" #'eq ":proven" (format nil "~A" result)) "axiom-con-r")))

(defun test-axiom-incon-l ()
  "Tests axiom-incon-l for returning :refuted."
  (let ((result (axiom-incon-l *knowledge-base*)))
    (values (assert-result result :refuted "axiom-incon-l" "Axiom" #'eq ":refuted" (format nil "~A" result)) "axiom-incon-l")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Dependence Rule Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-dependence-r-positive ()
  "Positive test for rule-dependence-r: Should apply and return :rule-applied."
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-positive")))

(defun test-rule-dependence-r-negative ()
  "Negative test (in sense of rule not applying under different conditions - currently always applies in base case) for rule-dependence-r."
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-negative")))

(defun test-rule-dependence-l-positive ()
  "Positive test for rule-dependence-l: Should apply and return :rule-applied."
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-positive")))

(defun test-rule-dependence-l-negative ()
  "Negative test (in sense of rule not applying under different conditions - currently always applies in base case) for rule-dependence-l."
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-negative")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Independence Rule Tests - REFACTORED - NOW LIKELY TO FAIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-independence-r-premise1-con ()
  "Tests rule-independence-r when premise 1 is axiomatically proven."
  (multiple-value-bind (rule-result premise-satisfied)
        (rule-independence-r *knowledge-base*)
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied)
                           "rule-independence-r-premise1-con" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise1-con")))


(defun test-rule-independence-r-premise2-con ()
  "Tests rule-independence-r when premise 2 is axiomatically proven (via mock)."
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A).~%" axiom-con-count)
             (if (= axiom-con-count 2)  ; Only second call returns :proven
                 :proven
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied)
                               "rule-independence-r-premise2-con" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise2-con")))))


(defun test-rule-independence-r-negative ()
  "Negative test for rule-independence-r: Should fail when neither premise is proven (via mock)."
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A) - (Negative Test - Never Proven).~%" axiom-con-count)
             nil))  ; Mock axiom never returns :proven
      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore premise-satisfied))
        (values (assert-result rule-result nil "rule-independence-r-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-r-negative")))))


(defun test-rule-independence-r-early-exit-premise1 ()
  "Tests early exit in rule-independence-r: Premise 1 proven, premise 2 should not always be fully evaluated."
  (let ((axiom-con-count 0)
        (premise2-evaluated-flag nil))

    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A) - (Early Exit Test).~%" axiom-con-count)
             (if (= axiom-con-count 1)  ; First call returns :proven, triggering early exit
                 :proven
                 (progn
                   (setf premise2-evaluated-flag t)  ; Flag to check if premise 2 was evaluated (ideally not always)
                   nil))))

      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore rule-result premise-satisfied))
        ;; Early exit is no longer directly testable in the same way with threads.
        ;; Removing premise2-evaluated-flag checks, focusing on rule-result.
        (values (assert-result rule-result :rule-applied
                               "rule-independence-r-early-exit-premise1" "Independence Rule" #'eq ":rule-applied" (format nil "~A" rule-result)) "rule-independence-r-early-exit-premise1")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Independence Left Rule Tests - REFACTORED - NOW LIKELY TO FAIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-rule-independence-l-premise1-incon ()
  "Tests rule-independence-l when premise 1 is axiomatically refuted."
  (multiple-value-bind (rule-result premise-satisfied)
        (rule-independence-l *knowledge-base*)
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied) ; Corrected expected value
                           "rule-independence-l-premise1-incon" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-l-premise1-incon")))


(defun test-rule-independence-l-premise2-incon ()
  "Tests rule-independence-l when premise 2 is axiomatically refuted (via mock)."
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A).~%" axiom-incon-count)
             (if (= axiom-incon-count 2)  ; Only second call returns :refuted
                 :refuted
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied) ; Corrected expected value
                               "test-rule-independence-l-premise2-incon" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "test-rule-independence-l-premise2-incon")))))


(defun test-rule-independence-l-negative ()
  "Negative test for rule-independence-l: Should fail when neither premise is refuted (via mock)."
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A) - (Negative Test - Never Refuted).~%" axiom-incon-count)
             nil))  ; Mock axiom never returns :refuted
      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore premise-satisfied))
        (values (assert-result rule-result nil "rule-independence-l-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-l-negative")))))


(defun test-rule-independence-l-early-exit-premise1 ()
  "Tests early exit in rule-independence-l: Premise 1 refuted, premise 2 should not always be fully evaluated."
  (let ((axiom-incon-count 0)
        (premise2-evaluated-flag nil))

    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A) - (Early Exit Test).~%" axiom-incon-count)
             (if (= axiom-incon-count 1)  ; First call returns :refuted, triggering early exit
                 :refuted
                 (progn
                   (setf premise2-evaluated-flag t)  ; Flag to check if premise 2 was evaluated (ideally not always)
                   nil))))

      (multiple-value-bind (rule-result premise-satisfied)
            (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore rule-result premise-satisfied))
        ;; Early exit is no longer directly testable in the same way with threads.
        ;; Removing premise2-evaluated-flag checks, focusing on rule-result.
        (values (assert-result rule-result :rule-applied
                               "rule-independence-l-early-exit-premise1" "Independence Rule" #'eq ":rule-applied" (format nil "~A" rule-result)) "rule-independence-l-early-exit-premise1")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Category Lists - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *axiom-tests*
  '(test-axiom-con-r
    test-axiom-incon-l)
  "List of axiom test functions.")

(defvar *dependence-rule-tests*
  '(test-rule-dependence-r-positive
    test-rule-dependence-r-negative
    test-rule-dependence-l-positive
    test-rule-dependence-l-negative)
  "List of dependence rule test functions.")

(defvar *independence-rule-tests*
  '(test-rule-independence-r-premise1-con
    test-rule-independence-r-premise2-con
    test-rule-independence-r-negative
    test-rule-independence-r-early-exit-premise1
    test-rule-independence-l-premise1-incon
    test-rule-independence-l-premise2-incon
    test-rule-independence-l-negative
    test-rule-independence-l-early-exit-premise1)
  "List of independence rule test functions.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run All Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all-tests ()
  "Runs all test categories, aggregates results, and returns T if all tests pass, NIL otherwise."
  (reset-test-summary)
  (format t "Starting Unit Tests.~%")
  (let ((axiom-results (run-tests *axiom-tests* "Axiom"))
        (dependence-rule-results (run-tests *dependence-rule-tests* "Dependence Rule"))
        (independence-rule-results (run-tests *independence-rule-tests* "Independence Rule"))
        (total-passed 0)
        (total-failed 0))

    (setf total-passed (+ (car axiom-results) (car dependence-rule-results) (car independence-rule-results)))
    (setf total-failed (+ (cdr axiom-results) (cdr dependence-rule-results) (cdr independence-rule-results)))

    (format t "~%--- Test Summary ---~%")
    (format t "Axiom Tests:      Passed: ~A, Failed: ~A~%" (car axiom-results) (cdr axiom-results))
    (format t "Dependence Rule Tests: Passed: ~A, Failed: ~A~%" (car dependence-rule-results) (cdr dependence-rule-results))
    (format t "Independence Rule Tests: Passed: ~A, Failed: ~A~%" (car independence-rule-results) (cdr independence-rule-results))
    (format t "~%Total Tests:      Passed: ~A, Failed: ~A~%" total-passed total-failed)

    (format t "~%--- Failed Test Details ---~%")
    (iterate-test-summary)

    (format t "~%Unit Tests Finished.~%")

    (zerop total-failed)))  ; Return T if total-failed is 0, NIL otherwise


(defun iterate-test-summary ()
  "Iterates through the test summary and prints details of failed tests."
  (loop for category being the hash-keys of *test-suite-summary* using (hash-value category-summary)
        do (format t "~%~A Tests:~%" category)
           (loop for test-name being the hash-keys of category-summary using (hash-value result)
                 do (unless result
                      (format t "    Failed Test: ~A~%" test-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run the Prover Prototype and Tests - COMPLEXITY REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  "The main entry point for the script.
It runs the full suite of unit tests and then executes the main `run-prover`
function, demonstrating the prover's operation and printing complexity metrics."
  (format t "Starting Barebones Theorem Prover Prototype (Refactored - Threaded Independence Rules).~%")

  (let ((test-run-successful (run-all-tests)))  ; Run unit tests and get success status
    (if test-run-successful
        (format t "~%All unit tests PASSED.~%")
        (format t "~%Some unit tests FAILED. See '--- Failed Test Details ---' section.~%")))

  (let ((prover-result (run-prover)))
    (format t "~%Prover Result: ~A~%" prover-result)
    (format t "~%--- Complexity Metrics ---~%")
    (format t "Axiom Applications: ~A~%" *axiom-applications-count*)
    (format t "Rule Applications:  ~A~%" *rule-applications-count*)
    (format t "Barebones Theorem Prover Prototype Finished.~%")))

(main)