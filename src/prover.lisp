(defpackage #:prover
  (:use #:cl)
  (:export #:run-prover))

(in-package #:prover)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prover Core Logic
;;;
;;; This file implements the core logic for the Prover, which
;;; is based on a simplified set of rules for a sequent calculus.
;;; The code has been hardened to be side-effect-free and robust.
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

(defun axiom-conr (formula)
  "Proof Axiom: Proves '(dep A A)' by returning A."
  (when (and (formula-p formula)
             (string-equal (symbol-name (formula-type formula)) "DEP")
             (equal (second formula) (third formula)))
    (second formula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Prover Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-prover (formula)
  "Runs the theorem prover on a given formula.
   This is the main entry point and dispatcher for the prover logic.
   It has been hardened to be side-effect-free."
  (when (formula-p formula)
    (axiom-conr formula)))