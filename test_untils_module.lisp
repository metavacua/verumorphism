;; test-utils.lisp
(defpackage :test-utils-module
  (:use :cl)
  (:export :reset-test-summary :update-test-summary :format-test-result
           :assert-eq :assert-true :assert-rule-applied :assert-axiom-proven :assert-axiom-refuted
           :*test-suite-summary* :iterate-test-summary))

(in-package :test-utils-module)

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

(defun assert-eq (actual expected test-name test-category)
  "Assertion for equality, updates test summary."
  (let ((pass-fail (equal actual expected))
        (message (format nil "Expected ~A, got ~A" expected actual)))
    (format-test-result test-name pass-fail message)
    (update-test-summary test-category test-name pass-fail)
    pass-fail))

(defun assert-true (condition test-name test-category message)
  "Assertion for truth, updates test summary."
  (let ((pass-fail condition))
    (format-test-result test-name pass-fail message)
    (update-test-summary test-category test-name pass-fail)
    pass-fail))

(defun assert-rule-applied (result test-name test-category)
  "Assertion for rule application, updates test summary."
  (assert-true (eq result :rule-applied) test-name test-category (format nil "Expected :rule-applied, got ~A" result)))

(defun assert-axiom-proven (result test-name test-category)
  "Assertion for axiom proof, updates test summary."
  (assert-true (eq result :proven) test-name test-category (format nil "Expected :proven, got ~A" result)))

(defun assert-axiom-refuted (result test-name test-category)
  "Assertion for axiom refutation, updates test summary."
  (assert-true (eq result :refuted) test-name test-category (format nil "Expected :refuted, got ~A" result)))


(defun iterate-test-summary ()
  "Iterates through the test summary and prints details of failed tests."
  (loop for category being the hash-keys of *test-suite-summary* using (hash-value category-summary)
        do (format t "~%~A Tests:~%" category)
           (loop for test-name being the hash-keys of category-summary using (hash-value result)
                 do (unless result
                      (format t "    Failed Test: ~A~%" test-name)))))