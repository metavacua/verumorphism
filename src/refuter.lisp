(defpackage #:refuter
  (:use #:cl)
  (:export #:run-refuter))

(in-package #:refuter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refuter Core Logic
;;;
;;; This file implements the core logic for the Refuter.
;;; The code has been hardened to be side-effect-free, robust,
;;; and free of infinite recursion bugs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun formula-type (formula)
  "Safely returns the type of a formula."
  (when (and formula (consp formula))
    (first formula)))

(defun formula-p (formula)
  "Checks if the input is a valid formula structure (a non-empty list)."
  (and formula (consp formula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Axioms & Rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom-inconl (formula)
  "Axiom InconL: Refutes '(incon)' and '(ind A A)'."
  (when (formula-p formula)
    (cond
      ((string-equal (symbol-name (formula-type formula)) "INCON") formula)
      ((and (string-equal (symbol-name (formula-type formula)) "IND")
            (equal (second formula) (third formula))) formula)
      (t nil))))

(defun rule-dependence-l (formula)
  "Dependence Left Rule (AND): Refutes '(dep A B)' if both A and B are refuted."
  (when (and (formula-p formula) (<= 2 (length (rest formula))))
    (let ((refutation1-result (run-refuter (second formula)))
          (refutation2-result (run-refuter (third formula))))
      (when (and refutation1-result refutation2-result)
        formula))))

(defun rule-independence-l (formula)
  "Independence Left Rule (OR): Refutes '(ind A B)' if A or B is refuted."
  ;; First, check for the axiom case (ind A A)
  (let ((axiom-result (axiom-inconl formula)))
    (if axiom-result
        axiom-result
        (when (and (formula-p formula) (<= 2 (length (rest formula))))
          (let ((premise1-result (run-refuter (second formula)))
                (premise2-result (run-refuter (third formula))))
            (when (or premise1-result premise2-result)
              formula))))))

(defun rule-dual-l (formula)
  "Rule dualL: Handles refutation for '(dual A)'."
  (when (and (formula-p formula) (consp (rest formula)))
    (let* ((sub-formula (second formula)))
      (when (formula-p sub-formula)
        (cond
          ;; (dual incon) is not refuted
          ((string-equal (symbol-name (formula-type sub-formula)) "INCON")
           nil)
          ;; (dual (dual A)) simplifies to A, which is then re-evaluated
          ((string-equal (symbol-name (formula-type sub-formula)) "DUAL")
           (run-refuter (second sub-formula)))
          ;; Standard case: (dual A) is refuted if A is refuted
          (t (when (run-refuter sub-formula)
               formula)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Refuter Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-refuter (formula)
  "Runs the theorem refuter on a given formula.
   This is the main entry point and dispatcher for the refuter logic."
  (when (formula-p formula)
    (let ((type-name (symbol-name (formula-type formula))))
      (cond
        ((string-equal type-name "INCON") (axiom-inconl formula))
        ((string-equal type-name "IND") (rule-independence-l formula))
        ((string-equal type-name "DEP") (rule-dependence-l formula))
        ((string-equal type-name "DUAL") (rule-dual-l formula))
        (t nil)))))