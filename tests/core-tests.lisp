;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hardened Core Logic Tests
;;;
;;; This file contains a robust test suite for the Prover and
;;; Refuter modules, including checks for correctness, edge cases,
;;; and invalid inputs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:core-tests
  (:use #:cl #:prover #:refuter)
  (:export #:run-all-tests))

(in-package #:core-tests)

;;; A more robust test harness
(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro deftest (name expression expected)
  `(defun ,name ()
     (let ((result ,expression))
       (if (equal result ,expected)
           (progn
             (incf *tests-passed*)
             (format t "~&[PASS] ~a~%" ',name)
             t)
           (progn
             (incf *tests-failed*)
             (format t "~&[FAIL] ~a~%  Expected: ~s~%  Got:      ~s~%" ',name ,expected result)
             nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prover Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-prover-axiom-dep-A-A
    (run-prover '(dep some-atom some-atom))
  'some-atom)

(deftest test-prover-unprovable-con
    (run-prover '(con))
  nil)

(deftest test-prover-unprovable-ind
    (run-prover '(ind a b))
  nil)

(deftest test-prover-malformed-atom
    (run-prover 'just-an-atom)
  nil)

(deftest test-prover-malformed-dep-short
    (run-prover '(dep a))
  nil)

(deftest test-prover-malformed-empty-list
    (run-prover '())
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refuter Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-refuter-axiom-incon
    (run-refuter '(incon))
  '(incon))

(deftest test-refuter-axiom-ind-A-A
    (run-refuter '(ind some-atom some-atom))
  '(ind some-atom some-atom))

(deftest test-refuter-rule-dep-and
    (run-refuter '(dep (incon) (ind a a)))
  '(dep (incon) (ind a a)))

(deftest test-refuter-rule-dep-fail
    (run-refuter '(dep (incon) (something-else)))
  nil)

(deftest test-refuter-rule-ind-or-1
    (run-refuter '(ind (incon) (something-else)))
  '(ind (incon) (something-else)))

(deftest test-refuter-rule-ind-or-2
    (run-refuter '(ind (something-else) (incon)))
  '(ind (something-else) (incon)))

(deftest test-refuter-rule-ind-fail
    (run-refuter '(ind (a) (b)))
  nil)

(deftest test-refuter-rule-dual-dual-A
    (run-refuter '(dual (dual (incon))))
  '(incon))

(deftest test-refuter-rule-dual-incon-fail
    (run-refuter '(dual (incon)))
  nil)

(deftest test-refuter-malformed-dual-short
    (run-refuter '(dual))
  nil)

(deftest test-refuter-malformed-atom
    (run-refuter 'incon)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Runner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all-tests ()
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (format t "~&~%===== Running All Core Logic Tests =====~%")

  ;; --- Prover ---
  (test-prover-axiom-dep-A-A)
  (test-prover-unprovable-con)
  (test-prover-unprovable-ind)
  (test-prover-malformed-atom)
  (test-prover-malformed-dep-short)
  (test-prover-malformed-empty-list)

  ;; --- Refuter ---
  (test-refuter-axiom-incon)
  (test-refuter-axiom-ind-A-A)
  (test-refuter-rule-dep-and)
  (test-refuter-rule-dep-fail)
  (test-refuter-rule-ind-or-1)
  (test-refuter-rule-ind-or-2)
  (test-refuter-rule-ind-fail)
  (test-refuter-rule-dual-dual-A)
  (test-refuter-rule-dual-incon-fail)
  (test-refuter-malformed-dual-short)
  (test-refuter-malformed-atom)

  (format t "~%~%===== Test Summary =====~%")
  (format t "Passed: ~a~%" *tests-passed*)
  (format t "Failed: ~a~%" *tests-failed*)
  (format t "======================~%")

  (zerop *tests-failed*))