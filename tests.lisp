(defpackage #:refuter-tests
  (:use #:cl #:fiveam)
  (:import-from #:refuter-core
		#:axiom-inconl
		#:make-incon
		#:make-ind))

(in-package #:refuter-tests)

;; Define a test suite for the refuter
(def-suite refuter-suite
  :description "Test suite for the Lisp-based refuter logic.")

;; Set the current suite to the one we just defined
(in-suite refuter-suite)

;; Test case for the axiom-inconl function
(test axiom-inconl-tests
  "Test the behavior of the axiom-inconl function."

  ;; Test 1: Should refute (incon)
  (let ((formula (make-incon)))
    (is (equal formula (axiom-inconl formula))
        "axiom-inconl should return (incon) when given (incon)."))

  ;; Test 2: Should refute (ind A A)
  (let* ((atom-a '(a))
         (formula (make-ind atom-a atom-a)))
    (is (equal formula (axiom-inconl formula))
        "axiom-inconl should return the formula itself for (ind A A)."))

  ;; Test 3: Should NOT refute (ind A B)
  (let* ((atom-a '(a))
         (atom-b '(b))
         (formula (make-ind atom-a atom-b)))
    (is (null (axiom-inconl formula))
        "axiom-inconl should return nil for (ind A B) where A is not equal to B."))

  ;; Test 4: Should return nil for other formula types
  (let ((formula '(dep (a) (b))))
    (is (null (axiom-inconl formula))
        "axiom-inconl should return nil for formulas other than 'incon' or 'ind'.")))

;; Function to run all tests in this suite
(defun run-all-tests ()
  "Runs all tests defined in the refuter-suite."
  (run! 'refuter-suite))