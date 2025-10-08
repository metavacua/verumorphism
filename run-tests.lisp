;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Runner Script (ASDF-based)
;;;
;;; This script loads the Weaver system via ASDF and Quicklisp,
;;; then runs the test suite.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure Quicklisp is loaded
(load (merge-pathnames #P"quicklisp/setup.lisp" (user-homedir-pathname)))

;; Add the project root to ASDF's central registry
(push *default-pathname-defaults* asdf:*central-registry*)

;; Load the Weaver system, which includes the prover and refuter
(ql:quickload :weaver)

;; Load the test definitions
(load "tests/core-tests.lisp")

;; Run the tests and exit with a status code indicating success or failure
(let ((success (core-tests:run-all-tests)))
  (sb-ext:exit :code (if success 0 1)))