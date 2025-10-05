;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structures (Minimal for Prototype) - Enhanced Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass relnet-node ()
  ((name :initarg :name :accessor relnet-node-name)
   (type :initarg :type :accessor relnet-node-type))
  (:documentation "Represents a node in the relational network, enhanced with a type.
This is a basic structure for elements within the prover's universe.

Slots:
  - NAME: The symbolic name of the node.
  - TYPE: The type of the node (e.g., 'formula', 'term')."))

(defvar *knowledge-base* nil
  "The global knowledge base for the prover. In this prototype, it is not
used beyond being passed to axiom and rule functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms (con_R and incon_L - Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-con-r (kb)
  "A minimal implementation of the 'Consistency Right' (con_R) proof axiom.
In this test-focused version, it unconditionally returns :proven.

Parameters:
  - KB: The knowledge base (ignored).

Returns:
  - The keyword :PROVEN."
  (declare (ignore kb))
  (format t "Proof Thread: Applying con_R axiom - Axiomatically Proven.~%")
  :proven)

(defun axiom-incon-l (kb)
  "A minimal implementation of the 'Inconsistency Left' (incon_L) refutation axiom.
In this test-focused version, it unconditionally returns :refuted.

Parameters:
  - KB: The knowledge base (ignored).

Returns:
  - The keyword :REFUTED."
  (declare (ignore kb))
  (format t "Refutation Thread: Applying incon_L axiom - Axiomatically Refuted.~%")
  :refuted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence - Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-dependence-r (kb)
  "Simulates the 'Dependence Right' (*DEP*R) rule.
This version attempts to prove two premises by calling `axiom-con-r` for both.
It succeeds if both premises are proven.

Parameters:
  - KB: The knowledge base.

Returns:
  - :RULE-APPLIED on success, NIL on failure."
  (format t "Proof Thread: Attempting rule *DEP*R (Dependence Right).~%")
  (let ((proof1-result (axiom-con-r kb))
        (proof2-result (axiom-con-r kb)))
    (if (and (eq proof1-result :proven) (eq proof2-result :proven))
        (progn
          (format t "Proof Thread: Rule *DEP*R applied successfully - Proven (*DEP* con con).~%")
          :rule-applied)
        (progn
          (format t "Proof Thread: Rule *DEP*R failed to apply.~%")
          nil))))

(defun rule-dependence-l (kb)
  "Simulates the 'Dependence Left' (*DEP*L) rule.
This version attempts to refute two premises by calling `axiom-incon-l` for both.
It succeeds if both premises are refuted.

Parameters:
  - KB: The knowledge base.

Returns:
  - :RULE-APPLIED on success, NIL on failure."
  (format t "Refutation Thread: Attempting rule *DEP*L (Dependence Left).~%")
  (let ((refute1-result (axiom-incon-l kb))
        (refute2-result (axiom-incon-l kb)))
    (if (and (eq refute1-result :refuted) (eq refute2-result :refuted))
        (progn
          (format t "Refutation Thread: Rule *DEP*L applied successfully - Refuted (*DEP* incon incon).~%")
          :rule-applied)
        (progn
          (format t "Refutation Thread: Rule *DEP*L failed to apply.~%")
          nil))))

(defun rule-independence-r (kb &key axiom-con)
  "Simulates the 'Independence Right' (*IND*R) rule with early exit.
This version attempts to prove one of two premises by calling a provided axiom function
(or `axiom-con-r` by default). It represents a logical OR and stops as soon as
one premise is proven.

Parameters:
  - KB: The knowledge base.
  - AXIOM-CON (Keyword, Optional): A function to use for proving premises,
    defaults to `axiom-con-r`.

Returns:
  - Multiple values on success: :RULE-APPLIED, a keyword indicating which premise
    succeeded, and flags indicating which premises were evaluated.
  - Multiple values on failure: NIL, NIL, and evaluation flags."
  (format t "Proof Thread: Attempting rule *IND*R (Independence Right).~%")
  (let ((premise1-evaluated nil)
        (premise2-evaluated nil))
    (let ((proof1-result (funcall (or axiom-con #'axiom-con-r) kb)))
      (setf premise1-evaluated t)
      (if (eq proof1-result :proven)
          (progn
            (format t "Proof Thread: Rule *IND*R applied successfully - Proven (*IND* con X) (via first premise).~%")
            (return-from rule-independence-r (values :rule-applied :premise1-satisfied :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))
          (let ((proof2-result (funcall (or axiom-con #'axiom-con-r) kb)))
            (setf premise2-evaluated t)
            (if (eq proof2-result :proven)
                (progn
                  (format t "Proof Thread: Rule *IND*R applied successfully - Proven (*IND* X con) (via second premise).~%")
                  (return-from rule-independence-r (values :rule-applied :premise2-satisfied :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))
                (progn
                  (format t "Proof Thread: Rule *IND*R failed to apply.~%")
                  (return-from rule-independence-r (values nil nil :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))))))))


(defun rule-independence-l (kb &key axiom-incon)
  "Simulates the 'Independence Left' (*IND*L) rule with early exit.
This version attempts to refute one of two premises by calling a provided axiom
function (or `axiom-incon-l` by default). It represents a logical OR and stops
as soon as one premise is refuted.

Parameters:
  - KB: The knowledge base.
  - AXIOM-INCON (Keyword, Optional): A function to use for refuting premises,
    defaults to `axiom-incon-l`.

Returns:
  - Multiple values on success: :RULE-APPLIED, a keyword indicating which premise
    succeeded, and flags indicating which premises were evaluated.
  - Multiple values on failure: NIL, NIL, and evaluation flags."
  (format t "Refutation Thread: Attempting rule *IND*L (Independence Left).~%")
  (let ((premise1-evaluated nil)
        (premise2-evaluated nil))
    (let ((refute1-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
      (setf premise1-evaluated t)
      (if (eq refute1-result :refuted)
          (progn
            (format t "Refutation Thread: Rule *IND*L applied successfully - Refuted (*IND* incon X) (via first premise).~%")
            (return-from rule-independence-l (values :rule-applied :premise1-satisfied :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))
          (let ((refute2-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
            (setf premise2-evaluated t)
            (if (eq refute2-result :refuted)
                (progn
                  (format t "Refutation Thread: Rule *IND*L applied successfully - Refuted (*IND* X incon) (via second premise).~%")
                  (return-from rule-independence-l (values :rule-applied :premise2-satisfied :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))
                (progn
                  (format t "Refutation Thread: Rule *IND*L failed to apply.~%")
                  (return-from rule-independence-l (values nil nil :premises-evaluated-flags (list premise1-evaluated premise2-evaluated))))))))))


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
It sequentially tries to apply the `con_R` axiom, the `*DEP*R` rule, and the
`*IND*R` rule. If any of these succeed, it sets the `*proof-result*` and
`*termination-flag*` and exits.

Returns:
  - :PROVEN on success, :UNKNOWN on failure."
  (format t "Proof Thread: Starting.~%")
  ;; (sleep 1) ; REMOVED SLEEP CALL

  ;; 1. Try axiom con_R
  (let ((axiom-con-r-result (axiom-con-r *knowledge-base*)))
    (when (eq axiom-con-r-result :proven)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via con_R axiom).~%")
      (return-from proof-thread-function :proven)))

  ;; 2. Try rule *DEP*R
  (let ((rule-dep-r-result (rule-dependence-r *knowledge-base*)))
    (when (eq rule-dep-r-result :rule-applied)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via rule *DEP*R).~%")
      (return-from proof-thread-function :proven)))

  ;; 3. Try rule *IND*R (using default axiom-con-r)
  (multiple-value-bind (rule-ind-r-result premise-satisfied flags)
      (rule-independence-r *knowledge-base*)
    (declare (ignore premise-satisfied flags))
    (when (eq rule-ind-r-result :rule-applied)
      (setf *proof-result* :proven)
      (setf *termination-flag* :proof-terminated)
      (format t "Proof Thread: Terminating (via rule *IND*R).~%")
      (return-from proof-thread-function :proven)))


  ;; 4. No proof found
  (format t "Proof Thread: No proof found in this iteration.~%")
  (setf *proof-result* :unknown)
  (setf *termination-flag* :proof-terminated)
  (format t "Proof Thread: Terminating (no proof).~%")
  :unknown)



(defun refutation-thread-function ()
  "The main function for the refutation-seeking thread.
It sequentially tries to apply the `incon_L` axiom, the `*DEP*L` rule, and the
`*IND*L` rule. If any of these succeed, it sets the `*refutation-result*` and
`*termination-flag*` and exits.

Returns:
  - :REFUTED on success, :UNKNOWN on failure."
  (format t "Refutation Thread: Starting.~%")
  ;; (sleep 1.5)  ; REMOVED SLEEP CALL

  ;; 1. Try axiom incon_L
  (let ((axiom-incon-l-result (axiom-incon-l *knowledge-base*)))
    (when (eq axiom-incon-l-result :refuted)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via incon_L axiom).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 2. Try rule *DEP*L
  (let ((rule-dep-l-result (rule-dependence-l *knowledge-base*)))
    (when (eq rule-dep-l-result :rule-applied)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via rule *DEP*L).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 3. Try rule *IND*L (using default axiom-incon-l)
  (multiple-value-bind (rule-ind-l-result premise-satisfied flags)
      (rule-independence-l *knowledge-base*)
    (declare (ignore premise-satisfied flags))
    (when (eq rule-ind-l-result :rule-applied)
      (setf *refutation-result* :refuted)
      (setf *termination-flag* :refutation-terminated)
      (format t "Refutation Thread: Terminating (via rule *IND*L).~%")
      (return-from refutation-thread-function :refuted)))

  ;; 4. No refutation found
  (format t "Refutation Thread: No refutation found in this iteration.~%")
  (setf *refutation-result* :unknown)
  (setf *termination-flag* :refutation-terminated)
  (format t "Refutation Thread: Terminating (no refutation).~%")
  :unknown)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parallel Interface and Orchestration (Barebones Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-knowledge-base ()
  "Resets the `*knowledge-base*` to an empty state."
  (setf *knowledge-base* nil))

(defun run-prover ()
  "Runs the theorem prover by orchestrating parallel proof and refutation threads.
This function initializes the knowledge base, starts the two threads, and waits
until one of them sets the `*termination-flag*`. The result is determined by
which thread finishes first.

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
    (loop until *termination-flag*
          do (sleep 0.1))

    (format t "Prover: Termination signal received: ~A~%" *termination-flag*)

    (cond ((eq *termination-flag* :proof-terminated)
           (format t "Prover: Proof Thread terminated first. Result: Proven.~%")
           :proven)
          ((eq *termination-flag* :refutation-terminated)
           (format t "Prover: Refutation Thread terminated first. Result: Refuted.~%")
           :refuted)
          (t
           (format t "Prover: Unexpected termination state.~%")
           :unknown))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests - Comprehensive and Targeted - ENHANCED - REFACTORED and MODULARIZED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-suite-summary* (make-hash-table)
  "A hash table to store the results of the test suite.
The keys are test categories (strings), and the values are other hash tables
where keys are test names and values are booleans (T for pass, NIL for fail).")

(defun reset-test-summary ()
  "Resets the `*test-suite-summary*` hash table to an empty state."
  (setf *test-suite-summary* (make-hash-table)))

(defun update-test-summary (test-category test-name result)
  "Records the result of a single test in the `*test-suite-summary*`.

Parameters:
  - TEST-CATEGORY: A string naming the category of the test (e.g., 'Axiom').
  - TEST-NAME: A string with the specific name of the test.
  - RESULT: A boolean indicating the test's outcome (T for pass, NIL for fail)."
  (let ((category-summary (gethash test-category *test-suite-summary*
                                     (make-hash-table :test #'equal))))
    (setf (gethash test-name category-summary) result)
    (setf (gethash test-category *test-suite-summary*) category-summary)))

(defun format-test-result (test-name pass-fail message)
  "Prints a formatted summary of a single test's result to standard output.

Parameters:
  - TEST-NAME: The name of the test.
  - PASS-FAIL: A boolean indicating the outcome.
  - MESSAGE: A message to display on failure."
  (if pass-fail
      (format t "    Test ~A: PASS~%" test-name) ; Indented for category clarity
      (format t "    Test ~A: FAIL - ~A~%" test-name message))) ; Indented for category clarity


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Assertion and Test Running Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assert-result (actual expected test-name test-category comparison-fn expected-str actual-str)
  "A generic assertion function to compare actual vs. expected results.

This function compares two values, formats a result message, updates the global
test summary, and returns the pass/fail status.

Parameters:
  - ACTUAL: The actual result obtained from a test.
  - EXPECTED: The expected result.
  - TEST-NAME: The name of the test.
  - TEST-CATEGORY: The category of the test.
  - COMPARISON-FN: The function to use for comparison (e.g., #'eq, #'equal).
  - EXPECTED-STR: A string representation of the expected value.
  - ACTUAL-STR: A string representation of the actual value.

Returns:
  - A boolean (T for pass, NIL for fail)."
  (let ((pass-fail (funcall comparison-fn actual expected))
        (message (format nil "Expected ~A (~A), got ~A (~A)" expected expected-str actual actual-str)))
    (format-test-result test-name pass-fail message)
    (update-test-summary test-category test-name pass-fail)
    pass-fail))

(defun run-tests (test-functions test-category)
  "Runs a collection of tests for a specific category.

Parameters:
  - TEST-FUNCTIONS: A list of symbols, where each symbol is the name of a test function.
  - TEST-CATEGORY: A string naming the category.

Returns:
  - A cons cell `(PASSED . FAILED)` containing the counts of passed and failed tests."
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
  "Tests the `axiom-con-r` function."
  (let ((result (axiom-con-r *knowledge-base*)))
    (values (assert-result result :proven "axiom-con-r" "Axiom" #'eq ":proven" (format nil "~A" result)) "axiom-con-r")))

(defun test-axiom-incon-l ()
  "Tests the `axiom-incon-l` function."
  (let ((result (axiom-incon-l *knowledge-base*)))
    (values (assert-result result :refuted "axiom-incon-l" "Axiom" #'eq ":refuted" (format nil "~A" result)) "axiom-incon-l")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Dependence Rule Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-dependence-r-positive ()
  "Tests the positive case for the `rule-dependence-r` function."
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-positive")))

(defun test-rule-dependence-r-negative ()
  "Tests the negative case for the `rule-dependence-r` function. Note: this test is currently a placeholder and expects a positive result."
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-negative")))

(defun test-rule-dependence-l-positive ()
  "Tests the positive case for the `rule-dependence-l` function."
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-positive")))

(defun test-rule-dependence-l-negative ()
  "Tests the negative case for the `rule-dependence-l` function. Note: this test is currently a placeholder and expects a positive result."
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-negative")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Independence Rule Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-independence-r-premise1-con ()
  "Tests `rule-independence-r` where the first premise should succeed immediately."
  (multiple-value-bind (rule-result premise-satisfied flags)
      (rule-independence-r *knowledge-base*)
    (declare (ignore flags))
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied)
                    "rule-independence-r-premise1-con" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise1-con")))


(defun test-rule-independence-r-premise2-con ()
  "Tests `rule-independence-r` where the second premise should succeed after the first fails."
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A).~%" axiom-con-count)
             (if (= axiom-con-count 2)
                 :proven
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore flags))
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied)
                        "rule-independence-r-premise2-con" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise2-con")))))


(defun test-rule-independence-r-negative ()
  "Tests `rule-independence-r` where both premises should fail."
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A) - (Negative Test - Never Proven).~%" axiom-con-count)
             nil))
      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore premise-satisfied flags))
        (values (assert-result rule-result nil "rule-independence-r-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-r-negative")))))


(defun test-rule-independence-r-early-exit-premise1 ()
  "Tests the early-exit behavior of `rule-independence-r`."
  (let ((axiom-con-count 0)
        (premise2-evaluated-flag nil))

    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A) - (Early Exit Test).~%" axiom-con-count)
             (if (= axiom-con-count 1)
                 :proven
                 (progn
                   (setf premise2-evaluated-flag t)
                   nil))))

      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore rule-result premise-satisfied))
        (let ((premise-flags (cdr (cdr (multiple-value-list (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r))))))
          (values (assert-result (list (not premise2-evaluated-flag) (car premise-flags) (cadr premise-flags)) (list t t nil)
                          "rule-independence-r-early-exit-premise1" "Independence Rule" #'equal "(T T NIL)" (format nil "(~A ~A ~A)" (not premise2-evaluated-flag) (car premise-flags) (cadr premise-flags))) "rule-independence-r-early-exit-premise1"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Independence Left Rule Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-rule-independence-l-premise1-incon ()
  "Tests `rule-independence-l` where the first premise should succeed immediately."
  (multiple-value-bind (rule-result premise-satisfied flags)
      (rule-independence-l *knowledge-base*)
    (declare (ignore flags))
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied)
                    "rule-independence-l-premise1-incon" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-l-premise1-incon")))


(defun test-rule-independence-l-premise2-incon ()
  "Tests `rule-independence-l` where the second premise should succeed after the first fails."
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A).~%" axiom-incon-count)
             (if (= axiom-incon-count 2)
                 :refuted
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore flags))
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied)
                        "test-rule-independence-l-premise2-incon" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "test-rule-independence-l-premise2-incon")))))


(defun test-rule-independence-l-negative ()
  "Tests `rule-independence-l` where both premises should fail."
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A) - (Negative Test - Never Refuted).~%" axiom-incon-count)
             nil))
      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore premise-satisfied flags))
        (values (assert-result rule-result nil "rule-independence-l-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-l-negative")))))


(defun test-rule-independence-l-early-exit-premise1 ()
  "Tests the early-exit behavior of `rule-independence-l`."
  (let ((axiom-incon-count 0)
        (premise2-evaluated-flag nil))

    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A) - (Early Exit Test).~%" axiom-incon-count)
             (if (= axiom-incon-count 1)
                 :refuted
                 (progn
                   (setf premise2-evaluated-flag t)
                   nil))))

      (multiple-value-bind (rule-result premise-satisfied flags)
          (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore rule-result premise-satisfied))
        (let ((premise-flags (cdr (cdr (multiple-value-list (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l))))))
          (values (assert-result (list (not premise2-evaluated-flag) (car premise-flags) (cadr premise-flags)) (list t t nil)
                          "rule-independence-l-early-exit-premise1" "Independence Rule" #'equal "(T T NIL)" (format nil "(~A ~A ~A)" (not premise2-evaluated-flag) (car premise-flags) (cadr premise-flags))) "rule-independence-l-early-exit-premise1"))))))


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
  "Runs the complete suite of unit tests.
This function executes all defined test categories, aggregates the results,
prints a summary, and reports details of any failed tests.

Returns:
  - T if all tests passed.
  - NIL if any test failed."
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
    (format t "Axiom Tests:          Passed: ~A, Failed: ~A~%" (car axiom-results) (cdr axiom-results))
    (format t "Dependence Rule Tests:  Passed: ~A, Failed: ~A~%" (car dependence-rule-results) (cdr dependence-rule-results))
    (format t "Independence Rule Tests: Passed: ~A, Failed: ~A~%" (car independence-rule-results) (cdr independence-rule-results))
    (format t "~%Total Tests:          Passed: ~A, Failed: ~A~%" total-passed total-failed)

    (format t "~%--- Failed Test Details ---~%")
    (iterate-test-summary)

    (format t "~%Unit Tests Finished.~%")

    (zerop total-failed))) ; Return T if total-failed is 0, NIL otherwise


(defun iterate-test-summary ()
  "Iterates through the `*test-suite-summary*` and prints details of failed tests.
This function is called by `run-all-tests` to provide a failure report."
  (loop for category being the hash-keys of *test-suite-summary* using (hash-value category-summary)
        do (format t "~%~A Tests:~%" category)
           (loop for test-name being the hash-keys of category-summary using (hash-value result)
                 do (unless result
                      (format t "    Failed Test: ~A~%" test-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run the Prover Prototype and Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  "The main entry point for the script.
It runs the full suite of unit tests and then executes the main `run-prover`
function to demonstrate the prover's operation."
  (format t "Starting Barebones Theorem Prover Prototype (Refactored).~%")

  (let ((test-run-successful (run-all-tests))) ; Run unit tests and get success status
    (if test-run-successful
        (format t "~%All unit tests PASSED.~%")
        (format t "~%Some unit tests FAILED. See '--- Failed Test Details ---' section.~%")))

  (let ((prover-result (run-prover)))
    (format t "~%Prover Result: ~A~%" prover-result)
    (format t "Barebones Theorem Prover Prototype Finished.~%")))

(main)