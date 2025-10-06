;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Suite Runner
;;;
;;; This script loads all necessary dependencies, the core application logic,
;;; and the test suite, and then runs the tests.
;;;
;;; To run from the command line:
;;; sbcl --load run-tests.lisp --eval '(refuter-tests:run-all-tests)' --quit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the dependency setup script first
(load "QHJ.lisp")

;; Load all required libraries (including fiveam)
(setup-dependencies)

;; Load the core refuter logic
(load "RefuterV0.lisp")

;; Load the test definitions
(load "tests.lisp")

;; Announce that tests are about to be run
(format t "~%~%All files loaded. Running test suite...~%")

;; Run the tests
(refuter-tests:run-all-tests)