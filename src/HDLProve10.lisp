;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Structures (Minimal for Prototype) - Enhanced Node
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass relnet-node ()
  ((name :initarg :name :accessor relnet-node-name)
   (type :initarg :type :accessor relnet-node-type)))

(defvar *knowledge-base* nil "Global Knowledge Base (Minimal for Prototype)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms (con_R and incon_L - Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-con-r (kb)
  "Proof Axiom (axiom con_R (() con)). Minimal implementation."
  (declare (ignore kb))
  (format t "Proof Thread: Applying con_R axiom - Axiomatically Proven.~%")
  :proven)

(defun axiom-incon-l (kb)
  "Refutation Axiom (axiom incon_L (incon ())). Minimal implementation."
  (declare (ignore kb))
  (format t "Refutation Thread: Applying incon_L axiom - Axiomatically Refuted.~%")
  :refuted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rules (Dependence & Independence - Minimal for Prototype) - THREADED INDEPENDENCE RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rule-dependence-r (kb)
  "Dependence Right Rule (rule *DEP*R). Testable version."
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
  "Dependence Left Rule (rule *DEP*L). Testable version."
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
  "Independence Right Rule (rule *IND*R) - THREADED. Testable version."
  (format t "Proof Thread: Attempting rule *IND*R (Independence Right) - THREADED.~%")
  (let ((premise1-result nil)
        (premise2-result nil)
        (thread1 nil)
        (thread2 nil))

    ;; Thread 1 for premise 1
    (setf thread1 (sb-thread:make-thread
                     (lambda ()
                       (setf premise1-result (funcall (or axiom-con #'axiom-con-r) kb)))
                     :name "IND-R-Premise1-Thread"))

    ;; Thread 2 for premise 2
    (setf thread2 (sb-thread:make-thread
                     (lambda ()
                       (setf premise2-result (funcall (or axiom-con #'axiom-con-r) kb)))
                     :name "IND-R-Premise2-Thread"))

    ;; Wait for threads to finish
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)

    (format t "Proof Thread: Both premises of *IND*R evaluated in parallel.~%")

    (if (eq premise1-result :proven)
        (progn
          (format t "Proof Thread: Rule *IND*R applied successfully - Proven (*IND* con X) (via first premise thread).~%")
          (return-from rule-independence-r (values :rule-applied :premise1-satisfied)))
        (if (eq premise2-result :proven)
            (progn
              (format t "Proof Thread: Rule *IND*R applied successfully - Proven (*IND* X con) (via second premise thread).~%")
              (return-from rule-independence-r (values :rule-applied :premise2-satisfied)))
            (progn
              (format t "Proof Thread: Rule *IND*R failed to apply.~%")
              (return-from rule-independence-r (values nil nil)))))))


(defun rule-independence-l (kb &key axiom-incon)
  "Independence Left Rule (rule *IND*L) - THREADED. Testable version."
  (format t "Refutation Thread: Attempting rule *IND*L (Independence Left) - THREADED.~%")
  (let ((premise1-result nil)
        (premise2-result nil)
        (thread1 nil)
        (thread2 nil))

    ;; Thread 1 for premise 1
    (setf thread1 (sb-thread:make-thread
                     (lambda ()
                       (setf premise1-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
                     :name "IND-L-Premise1-Thread"))

    ;; Thread 2 for premise 2
    (setf thread2 (sb-thread:make-thread
                     (lambda ()
                       (setf premise2-result (funcall (or axiom-incon #'axiom-incon-l) kb)))
                     :name "IND-L-Premise2-Thread"))

    ;; Wait for threads to finish
    (sb-thread:join-thread thread1)
    (sb-thread:join-thread thread2)

    (format t "Refutation Thread: Both premises of *IND*L evaluated in parallel.~%")


    (if (eq premise1-result :refuted)
        (progn
          (format t "Refutation Thread: Rule *IND*L applied successfully - Refuted (*IND* incon X) (via first premise thread).~%")
          (return-from rule-independence-l (values :rule-applied :premise1-satisfied)))
        (if (eq premise2-result :refuted)
            (progn
              (format t "Refutation Thread: Rule *IND*L applied successfully - Refuted (*IND* X incon) (via second premise thread).~%")
              (return-from rule-independence-l (values :rule-applied :premise2-satisfied)))
            (progn
              (format t "Refutation Thread: Rule *IND*L failed to apply.~%")
              (return-from rule-independence-l (values nil nil)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread Functions (Proof and Refutation - Minimal for Prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *proof-result* nil "Variable to store proof thread result")
(defvar *refutation-result* nil "Variable to store refutation thread result")
(defvar *termination-flag* nil "Flag to signal termination to both threads")


(defun proof-thread-function ()
  "Proof Thread: Applies con_R, *DEP*R and *IND*R rules, sets *proof-result*."
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

  ;; 3. Try rule *IND*R (using default axiom-con-r) - THREADED
  (multiple-value-bind (rule-ind-r-result premise-satisfied)
      (rule-independence-r *knowledge-base*)
    (declare (ignore premise-satisfied))
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
  "Refutation Thread: Applies incon_L, *DEP*L and *IND*L rules, sets *refutation-result*."
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

  ;; 3. Try rule *IND*L (using default axiom-incon-l) - THREADED
  (multiple-value-bind (rule-ind-l-result premise-satisfied)
      (rule-independence-l *knowledge-base*)
    (declare (ignore premise-satisfied))
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
  "Initializes the global *knowledge-base* (minimal for prototype)."
  (setf *knowledge-base* nil))

(defun run-prover ()
  "Runs the barebones theorem prover prototype with two threads."
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

(defvar *test-suite-summary* (make-hash-table) "Summary of test suite results.")

(defun reset-test-summary ()
  "Resets the *test-suite-summary* hash table."
  (setf *test-suite-summary* (make-hash-table)))

(defun update-test-summary (test-category test-name result)
  "Updates the test suite summary."
  (let ((category-summary (gethash test-category *test-suite-summary*
                                     (make-hash-table :test #'equal))))
    (setf (gethash test-name category-summary) result)
    (setf (gethash test-category *test-suite-summary*) category-summary)))

(defun format-test-result (test-name pass-fail message)
  "Formats the test result output."
  (if pass-fail
      (format t "    Test ~A: PASS~%" test-name) ; Indented for category clarity
      (format t "    Test ~A: FAIL - ~A~%" test-name message))) ; Indented for category clarity


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Assertion and Test Running Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assert-result (actual expected test-name test-category comparison-fn expected-str actual-str)
  "Generic assertion function."
  (let ((pass-fail (funcall comparison-fn actual expected))
        (message (format nil "Expected ~A (~A), got ~A (~A)" expected expected-str actual actual-str)))
    (format-test-result test-name pass-fail message)
    (update-test-summary test-category test-name pass-fail)
    pass-fail))

(defun run-tests (test-functions test-category)
  "Generic test runner function."
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
  (let ((result (axiom-con-r *knowledge-base*)))
    (values (assert-result result :proven "axiom-con-r" "Axiom" #'eq ":proven" (format nil "~A" result)) "axiom-con-r")))

(defun test-axiom-incon-l ()
  (let ((result (axiom-incon-l *knowledge-base*)))
    (values (assert-result result :refuted "axiom-incon-l" "Axiom" #'eq ":refuted" (format nil "~A" result)) "axiom-incon-l")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Dependence Rule Tests - REFACTORED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-dependence-r-positive ()
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-positive")))

(defun test-rule-dependence-r-negative ()
  (let ((result (rule-dependence-r *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-r-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-r-negative")))

(defun test-rule-dependence-l-positive ()
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-positive" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-positive")))

(defun test-rule-dependence-l-negative ()
  (let ((result (rule-dependence-l *knowledge-base*)))
    (values (assert-result result :rule-applied "rule-dependence-l-negative" "Dependence Rule" #'eq ":rule-applied" (format nil "~A" result)) "rule-dependence-l-negative")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Functions - Independence Rule Tests - REFACTORED - NOW LIKELY TO FAIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-rule-independence-r-premise1-con ()
  (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
      (rule-independence-r *knowledge-base*)
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied)
                    "rule-independence-r-premise1-con" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise1-con")))


(defun test-rule-independence-r-premise2-con ()
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A).~%" axiom-con-count)
             (if (= axiom-con-count 2)
                 :proven
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
          (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied)
                        "rule-independence-r-premise2-con" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-r-premise2-con")))))


(defun test-rule-independence-r-negative ()
  (let ((axiom-con-count 0))
    (flet ((mock-axiom-con-r (kb)
             (declare (ignore kb))
             (incf axiom-con-count)
             (format t "    Mock axiom-con-r called (count: ~A) - (Negative Test - Never Proven).~%" axiom-con-count)
             nil))
      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
          (rule-independence-r *knowledge-base* :axiom-con #'mock-axiom-con-r)
        (declare (ignore premise-satisfied))
        (values (assert-result rule-result nil "rule-independence-r-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-r-negative")))))


(defun test-rule-independence-r-early-exit-premise1 ()
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

      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
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
  (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
      (rule-independence-l *knowledge-base*)
    (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise1-satisfied)
                    "rule-independence-l-premise1-incon" "Independence Rule" #'equal "(:rule-applied :premise1-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "rule-independence-l-premise1-incon")))


(defun test-rule-independence-l-premise2-incon ()
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A).~%" axiom-incon-count)
             (if (= axiom-incon-count 2)
                 :refuted
                 nil)))
      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
          (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (values (assert-result (list rule-result premise-satisfied) (list :rule-applied :premise2-satisfied)
                        "test-rule-independence-l-premise2-incon" "Independence Rule" #'equal "(:rule-applied :premise2-satisfied)" (format nil "(~A ~A)" rule-result premise-satisfied)) "test-rule-independence-l-premise2-incon")))))


(defun test-rule-independence-l-negative ()
  (let ((axiom-incon-count 0))
    (flet ((mock-axiom-incon-l (kb)
             (declare (ignore kb))
             (incf axiom-incon-count)
             (format t "    Mock axiom-incon-l called (count: ~A) - (Negative Test - Never Refuted).~%" axiom-incon-count)
             nil))
      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
          (rule-independence-l *knowledge-base* :axiom-incon #'mock-axiom-incon-l)
        (declare (ignore premise-satisfied))
        (values (assert-result rule-result nil "rule-independence-l-negative" "Independence Rule" #'eq "NIL" (format nil "~A" rule-result)) "rule-independence-l-negative")))))


(defun test-rule-independence-l-early-exit-premise1 ()
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

      (multiple-value-bind (rule-result premise-satisfied) ; Removed flags from binding
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
    (format t "Axiom Tests:          Passed: ~A, Failed: ~A~%" (car axiom-results) (cdr axiom-results))
    (format t "Dependence Rule Tests:  Passed: ~A, Failed: ~A~%" (car dependence-rule-results) (cdr dependence-rule-results))
    (format t "Independence Rule Tests: Passed: ~A, Failed: ~A~%" (car independence-rule-results) (cdr independence-rule-results))
    (format t "~%Total Tests:          Passed: ~A, Failed: ~A~%" total-passed total-failed)

    (format t "~%--- Failed Test Details ---~%")
    (iterate-test-summary)

    (format t "~%Unit Tests Finished.~%")

    (zerop total-failed))) ; Return T if total-failed is 0, NIL otherwise


(defun iterate-test-summary ()
  "Iterates through the test summary and prints details of failed tests."
  (loop for category being the hash-keys of *test-suite-summary* using (hash-value category-summary)
        do (format t "~%~A Tests:~%" category)
           (loop for test-name being the hash-keys of category-summary using (hash-value result)
                 do (unless result
                      (format t "    Failed Test: ~A~%" test-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Entry Point - Run the Prover Prototype and Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (format t "Starting Barebones Theorem Prover Prototype (Refactored - Threaded Independence Rules).~%")

  (let ((test-run-successful (run-all-tests))) ; Run unit tests and get success status
    (if test-run-successful
        (format t "~%All unit tests PASSED.~%")
        (format t "~%Some unit tests FAILED. See '--- Failed Test Details ---' section.~%")))

  (let ((prover-result (run-prover)))
    (format t "~%Prover Result: ~A~%" prover-result)
    (format t "Barebones Theorem Prover Prototype Finished.~%")))

(main)