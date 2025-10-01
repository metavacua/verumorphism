;;;; Package definition for Weaver System

(defpackage :weaver-system
  (:use #:cl)
  (:export #:weaver-constants
           #:get-weaver-constant
           #:ax-con-r
           #:ax-incon-l
           #:dual
           #:ind-r
           #:ind-l
           #:dep-r
           #:dep-l))

(in-package :weaver-system)

;;; Constants

(defvar weaver-constants
  (list
   (cons 'CON #C(1 0))  ; Represents Consistency
   (cons 'INCON #C(0 1)) ; Represents Inconsistency
   )
  "List of Weaver constants.")

(defun get-weaver-constant (constant-name)
  "Retrieves a Weaver constant by name."
  (cdr (assoc constant-name weaver-constants)))

;;; Axioms

(defun ax-con-r (expression)
  "Axiom of Consistency - Right-handed."
  (if (eq expression 'CON)
      (get-weaver-constant 'CON)
      #C(0 0)))

(defun ax-incon-l (expression)
  "Axiom of Inconsistency - Left-handed."
  (if (eq expression 'INCON)
      (get-weaver-constant 'INCON)
      #C(0 0)))

;;; Operators

(defun dual (z)
  "Computes the dual of a complex number."
  (complex (realpart z) (- (imagpart z))))

(defun ind-r (e1 e2)
  "Right-handed independence operator."
  (+ (realpart (ax-con-r e1)) (realpart (ax-con-r e2))))

(defun ind-l (e1 e2)
  "Left-handed independence operator."
  (complex 0 1) (* (+ (imagpart (ax-con-r e1)) (imagpart (ax-con-r e2))) (complex 0 1)))

(defun dep-r (e1 e2)
  "Right-handed dependence operator."
  (/ (+ (realpart (ax-con-r e1)) (realpart (ax-con-r e2))) 2.0))

(defun dep-l (e1 e2)
  "Left-handed dependence operator."
  (complex 0 1) (* (/ (+ (imagpart (ax-con-r e1)) (imagpart (ax-con-r e2))) 2.0) (complex 0 1)))

;;; Example Usage (for testing and demonstration - not part of the package definition itself, but shows how to use it)
;; (in-package :cl-user) ;; Return to the CL-USER package to use the WEAVER-SYSTEM package

;; (defun test-weaver-package ()
;;   (format t "Weaver Constants:~%")
;;   (format t "CON: ~A~%" (weaver-system:get-weaver-constant 'CON))
;;   (format t "INCON: ~A~%" (weaver-system:get-weaver-constant 'INCON))

;;   (format t "~%Weaver Axioms:~%")
;;   (format t "AxConR('CON'): ~A~%" (weaver-system:ax-con-r 'CON))
;;   (format t "AxConR('INCON'): ~A~%" (weaver-system:ax-con-r 'INCON))
;;   (format t "AxInconL('INCON'): ~A~%" (weaver-system:ax-incon-l 'INCON))
;;   (format t "AxInconL('CON'): ~A~%" (weaver-system:ax-incon-l 'CON))

;;   (format t "~%Weaver Operators:~%")
;;   (format t "Dual(#C(2 3)): ~A~%" (weaver-system:dual #C(2 3)))
;;   (format t "IndR('CON', 'INCON'): ~A~%" (weaver-system:ind-r 'CON 'INCON))
;;   (format t "IndL('CON', 'INCON'): ~A~%" (weaver-system:ind-l 'CON 'INCON))
;;   (format t "DepR('CON', 'INCON'): ~A~%" (weaver-system:dep-r 'CON 'INCON))
;;   (format t "DepL('CON', 'INCON'): ~A~%" (weaver-system:dep-l 'CON 'INCON)))

;; To run the example, uncomment the following line and the (in-package :cl-user) line above
;; (test-weaver-package)

;;;; Package definition for Weaver Weavexes Library

(defpackage :weaver-weavexes
  (:use #:cl)
  (:export #:weavex
           #:make-weavex
           #:weavex-coefficients
           #:evaluate-weavex
           #:print-weavex))

(in-package :weaver-weavexes)

;;; Structure definition for WEAVEX

(defstruct weavex
  "Represents a weavex, which is a complex polynomial P(x) = 0."
  (coefficients nil :type list :read-only t) ;; List of complex coefficients, from constant term to highest power.
  )

;;; Function to create a WEAVEX

(defun make-weavex (coefficients)
  "Creates a WEAVEX object from a list of complex coefficients.
  Coefficients are ordered from the constant term to the coefficient of the highest power."
  (make-weavex :coefficients coefficients))

;;; Function to evaluate a WEAVEX at a complex number x

(defun evaluate-weavex (weavex x)
  "Evaluates the WEAVEX (polynomial) at a given complex number x."
  (let ((result #C(0.0 0.0))
        (coeffs (weavex-coefficients weavex)))
    (loop for coeff in coeffs
          for power from 0
          do (incf result (* coeff (expt x power))))
    result))

;;; Function to print a WEAVEX in a readable format

(defun print-weavex (weavex &optional (stream t))
  "Prints a WEAVEX in a polynomial format to the specified stream (default: standard output)."
  (let ((coeffs (weavex-coefficients weavex))
        (degree (1- (length coeffs))))
    (loop for coeff in coeffs
          for power from 0 to degree
          do
             (cond
               ((and (zerop power) (not (zerop (realpart coeff))) (not (zerop (imagpart coeff)))) (format stream "~A " coeff))
               ((and (zerop power) (not (zerop (realpart coeff)))) (format stream "~F " (realpart coeff)))
               ((and (zerop power) (not (zerop (imagpart coeff)))) (format stream "~Fi " (imagpart coeff)))

               ((and (> power 0) (not (zerop (realpart coeff))) (not (zerop (imagpart coeff)))) (format stream "+ ~A x^~D " coeff power))
               ((and (> power 0) (not (zerop (realpart coeff)))) (format stream "+ ~F x^~D " (realpart coeff) power))
               ((and (> power 0) (not (zerop (imagpart coeff)))) (format stream "+ ~Fi x^~D " (imagpart coeff) power))
               (t nil))) ; Handle zero coefficients gracefully, skip printing terms with zero coefficients
    (format stream "= 0")))


;;; Example Usage (for testing and demonstration - not part of the package definition itself, but shows how to use it)
;; (in-package :cl-user) ;; Return to the CL-USER package to use the WEAVER-WEAVEXES package

;; (defun test-weavex-library ()
;;   (let* ((coeffs '(#C(1 2) #C(3 -4) #C(0.5 0) #C(0 1))) ; Coefficients: 1+2i + (3-4i)x + 0.5x^2 + ix^3
;;          (w (weaver-weavexes:make-weavex coeffs)))

;;     (format t "Weavex coefficients: ~A~%" (weaver-weavexes:weavex-coefficients w))
;;     (format t "Weavex polynomial form: ")
;;     (weaver-weavexes:print-weavex w)
;;     (format t "~%")

;;     (let ((x #C(2 0))) ; Evaluate at x = 2
;;       (format t "Evaluate weavex at x = ~A: ~A~%" x (weaver-weavexes:evaluate-weavex w x)))

;;     (let ((x #C(0 1))) ; Evaluate at x = i
;;         (format t "Evaluate weavex at x = ~A: ~A~%" x (weaver-weavexes:evaluate-weavex w x)))
;;     ))

;; To run the example, uncomment the following lines and the (in-package :cl-user) line above
;; (test-weavex-library)