;;; --- Refocused Macro Test Suite (Comprehensive Basis Testing) ---

(format t ";;; --- Refocused Macro Test Suite (Comprehensive Basis Testing) --- ~%")

;;; --- Macro Test Suite ---
(format t "~%--- Macro Test Suite (Comprehensive Basis Testing) ---~%")

;; 1. Global Scope Macro Tests
(format t "~%--- 1. Global Scope Macro Tests ---~%")

;; 1.1. Global Macro Definition and Expansion
(defmacro global-macro-test-1 (x)
  `(* ,x 2))
(format t "~%1.1. Global Macro Definition: GLOBAL-MACRO-TEST-1 defined.~%")
(format t "~%1.1a. MACROEXPAND-1 of (global-macro-test-1 7) in global scope:~%")
(handler-case (format t "  Expansion: ~A~%" (macroexpand-1 '(global-macro-test-1 7)))
  (error (e) (format t "  Error during MACROEXPAND-1: ~A~%" e)))

;; 1.1b. Global Macro Usage in Global Scope (Correct Expansion)
(format t "~%1.1b. Usage of (global-macro-test-1 8) in global scope (correct expansion):~%")
(handler-case (format t "  Result: ~A~%" (global-macro-test-1 8))
  (error (e) (format t "  Error during macro usage: ~A~%" e)))

;; 1.1c. Global Macro Usage as Function Call (Incorrect) in Global Scope
(format t "~%1.1c. Incorrect Function-like Usage of (global-macro-test-1 9) in global scope (expect error):~%")
(handler-case (format t "  Attempted Function Call Result: ~A~%" (funcall 'global-macro-test-1 9)) ; Attempt incorrect function call
  (error (e) (format t "  Expected Error during incorrect macro usage: ~A~%" e)))


;; 2. Function Scope Macro Tests
(format t "~%--- 2. Function Scope Macro Tests ---~%")

;; 2.1. Macro Definition within Function and Expansion
(defun func-macro-test-outer ()
  (defmacro func-local-macro-test-1 (x)
    `(/ ,x 2))
  (format t "~%2.1. Function-Local Macro Definition: FUNC-LOCAL-MACRO-TEST-1 defined within function.~%")
  (format t "~%2.1a. MACROEXPAND-1 of (func-local-macro-test-1 10) within function scope:~%")
  (handler-case (format t "  Expansion: ~A~%" (macroexpand-1 '(func-local-macro-test-1 10)))
    (error (e) (format t "  Error during MACROEXPAND-1: ~A~%" e)))

  ;; 2.1b. Macro Usage in Function Scope (Correct Expansion)
  (format t "~%2.1b. Usage of (func-local-macro-test-1 12) within function scope (correct expansion):~%")
  (handler-case (format t "  Result: ~A~%" (func-local-macro-test-1 12))
    (error (e) (format t "  Error during macro usage in function: ~A~%" e))))

(format t "~%2.2. Calling function (func-macro-test-outer) to define and test function-local macro:~%")
(handler-case (funcall 'func-macro-test-outer)
  (error (e) (format t "  Error calling function defining macro: ~A~%" e)))

;; 2.3. Global Usage of Function-Local Macro (Should Fail)
(format t "~%2.3. Global Usage of Function-Local Macro (FUNC-LOCAL-MACRO-TEST-1) - Expected Error:~%")
(format t "~%2.3a. Attempting to use (func-local-macro-test-1 14) in global scope:~%")
(handler-case (format t "  Attempted Usage Result: ~A~%" (func-local-macro-test-1 14))
  (error (e) (format t "  Expected Error during global usage of function-local macro: ~A~%" e)))


;; 3. LET Scope Macro Tests
(format t "~%--- 3. LET Scope Macro Tests ---~%")

;; 3.1. Macro Definition within LET and Expansion
(let ()
  (defmacro let-local-macro-test-1 (x)
    `(+ ,x 3))
  (format t "~%3.1. LET-Local Macro Definition: LET-LOCAL-MACRO-TEST-1 defined within LET.~%")
  (format t "~%3.1a. MACROEXPAND-1 of (let-local-macro-test-1 15) within LET scope:~%")
  (handler-case (format t "  Expansion: ~A~%" (macroexpand-1 '(let-local-macro-test-1 15)))
    (error (e) (format t "  Error during MACROEXPAND-1 in LET: ~A~%" e)))

  ;; 3.1b. Macro Usage in LET Scope (Correct Expansion)
  (format t "~%3.1b. Usage of (let-local-macro-test-1 16) within LET scope (correct expansion):~%")
  (handler-case (format t "  Result: ~A~%" (let-local-macro-test-1 16))
    (error (e) (format t "  Error during macro usage in LET: ~A~%" e))))

;; 3.2. Global Usage of LET-Local Macro (Should Fail)
(format t "~%3.2. Global Usage of LET-Local Macro (LET-LOCAL-MACRO-TEST-1) - Expected Error:~%")
(format t "~%3.2a. Attempting to use (let-local-macro-test-1 17) in global scope:~%")
(handler-case (format t "  Attempted Usage Result: ~A~%" (let-local-macro-test-1 17))
  (error (e) (format t "  Expected Error during global usage of LET-local macro: ~A~%" e)))


;; 4. BLOCK Scope Macro Tests
(format t "~%--- 4. BLOCK Scope Macro Tests ---~%")

;; 4.1. Macro Definition within BLOCK and Expansion
(block test-block-macro
  (defmacro block-local-macro-test-1 (x)
    `(* ,x 4))
  (format t "~%4.1. BLOCK-Local Macro Definition: BLOCK-LOCAL-MACRO-TEST-1 defined within BLOCK.~%")
  (format t "~%4.1a. MACROEXPAND-1 of (block-local-macro-test-1 18) within BLOCK scope:~%")
  (handler-case (format t "  Expansion: ~A~%" (macroexpand-1 '(block-local-macro-test-1 18)))
    (error (e) (format t "  Error during MACROEXPAND-1 in BLOCK: ~A~%" e)))

  ;; 4.1b. Macro Usage in BLOCK Scope (Correct Expansion)
  (format t "~%4.1b. Usage of (block-local-macro-test-1 19) within BLOCK scope (correct expansion):~%")
  (handler-case (format t "  Result: ~A~%" (block-local-macro-test-1 19))
    (error (e) (format t "  Error during macro usage in BLOCK: ~A~%" e)))
  (return-from test-block-macro nil))

;; 4.2. Global Usage of BLOCK-Local Macro (Should Fail)
(format t "~%4.2. Global Usage of BLOCK-Local Macro (BLOCK-LOCAL-MACRO-TEST-1) - Expected Error:~%")
(format t "~%4.2a. Attempting to use (block-local-macro-test-1 20) in global scope:~%")
(handler-case (format t "  Attempted Usage Result: ~A~%" (block-local-macro-test-1 20))
  (error (e) (format t "  Expected Error during global usage of BLOCK-local macro: ~A~%" e)))


;; 5. LABELS Scope Macro Tests - NOTE: LABELS is for function definitions, macro in LABELS is unusual but testing for completeness
(format t "~%--- 5. LABELS Scope Macro Tests (Unusual Scope for Macros) ---~%")

;; 5.1. Macro Definition within LABELS and Expansion
(labels ()
  (defmacro labels-local-macro-test-1 (x)
    `(+ ,x 5))
  (format t "~%5.1. LABELS-Local Macro Definition: LABELS-LOCAL-MACRO-TEST-1 defined within LABELS.~%")
  (format t "~%5.1a. MACROEXPAND-1 of (labels-local-macro-test-1 21) within LABELS scope:~%")
  (handler-case (format t "  Expansion: ~A~%" (macroexpand-1 '(labels-local-macro-test-1 21)))
    (error (e) (format t "  Error during MACROEXPAND-1 in LABELS: ~A~%" e)))

  ;; 5.1b. Macro Usage in LABELS Scope (Correct Expansion)
  (format t "~%5.1b. Usage of (labels-local-macro-test-1 22) within LABELS scope (correct expansion):~%")
  (handler-case (format t "  Result: ~A~%" (labels-local-macro-test-1 22))
    (error (e) (format t "  Error during macro usage in LABELS: ~A~%" e))))

;; 5.2. Global Usage of LABELS-Local Macro (Should Fail)
(format t "~%5.2. Global Usage of LABELS-Local Macro (LABELS-LOCAL-MACRO-TEST-1) - Expected Error:~%")
(format t "~%5.2a. Attempting to use (labels-local-macro-test-1 23) in global scope:~%")
(handler-case (format t "  Attempted Usage Result: ~A~%" (labels-local-macro-test-1 23))
  (error (e) (format t "  Expected Error during global usage of LABELS-local macro: ~A~%" e)))


(format t "~%--- End of Refocused Macro Test Suite (Comprehensive Basis Testing) ---~%")
(format t ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")