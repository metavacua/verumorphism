		(defpackage :weaver-system
		  (:use #:cl)
		  (:documentation "This package defines the core matrix representations for the Weaver
		system's logical operators and provides functions to verify their algebraic
		identities. It serves as a mathematical foundation and verification script for
		the operator algebra.")
		  (:export #:weaver-constants
		           #:get-weaver-constant
		#:ax-con-r
		#:ax-incon-l
		#:dual
		#:ind-r
		#:ind-l
		#:dep-r
		#:dep-l
		#:check-matrix-identities))
		
		(in-package :weaver-system)
		
		;; Define matrices using make-array with :initial-contents
		
		(defvar indl (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(0.0 0.0) #C(0.0 1.0)) (#C(0.0 1.0) #C(0.0 1.0)))))
		(defvar depl (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(0.0 0.0) #C(0.0 0.0)) (#C(0.0 0.0) #C(0.0 1.0)))))
		(defvar depr (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(1.0 0.0) #C(0.0 0.0)) (#C(0.0 0.0) #C(0.0 0.0)))))
		(defvar indr (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(1.0 0.0) #C(1.0 0.0)) (#C(1.0 0.0) #C(0.0 0.0)))))
		(defvar identity-matrix (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(1.0 0.0) #C(0.0 0.0)) (#C(0.0 0.0) #C(1.0 0.0)))))
		
		;; Helper functions for matrix operations
		
		(defun matrix-add (matrix1 matrix2)
		  "Adds two 2x2 matrices element-wise.

		Parameters:
		  - MATRIX1: The first 2x2 matrix.
		  - MATRIX2: The second 2x2 matrix.

		Returns:
		  - A new 2x2 matrix representing the sum."
		  (let ((rows (array-dimension matrix1 0))
		        (cols (array-dimension matrix1 1))
		        (result (make-array (array-dimensions matrix1) :element-type 'complex-float)))
		    (loop for r from 0 below rows do
		      (loop for c from 0 below cols do
		        (setf (aref result r c) (+ (aref matrix1 r c) (aref matrix2 r c)))))
		    result))
		
		(defun scalar-multiply (scalar matrix)
		  "Multiplies a matrix by a scalar value.

		Parameters:
		  - SCALAR: The scalar number to multiply by.
		  - MATRIX: The 2x2 matrix to be scaled.

		Returns:
		  - A new 2x2 matrix representing the scaled result."
		  (let ((rows (array-dimension matrix 0))
		        (cols (array-dimension matrix 1))
		        (result (make-array (array-dimensions matrix) :element-type 'complex-float)))
		    (loop for r from 0 below rows do
		      (loop for c from 0 below cols do
		        (setf (aref result r c) (* scalar (aref matrix r c)))))
		    result))
		
		(defun complex-conjugate-transpose (matrix)
		  "Computes the complex conjugate transpose (dagger) of a 2x2 matrix.

		Parameters:
		  - MATRIX: The 2x2 matrix to process.

		Returns:
		  - A new 2x2 matrix that is the complex conjugate transpose of the input."
		  (let ((rows (array-dimension matrix 0))
		        (cols (array-dimension matrix 1))
		        (result (make-array '(2 2) :element-type 'complex-float)))
		    (loop for r from 0 below rows do
		      (loop for c from 0 below cols do
		        (setf (aref result c r) (conjugate (aref matrix r c)))) )
		    result))
		
		(defun matrix-equalp (matrix1 matrix2)
		  "Checks if two matrices are element-wise equal using `equalp`.

		Parameters:
		  - MATRIX1: The first matrix.
		  - MATRIX2: The second matrix.

		Returns:
		  - T if the matrices have the same dimensions and all corresponding elements are equal.
		  - NIL otherwise."
		  (let ((rows (array-dimension matrix1 0))
		        (cols (array-dimension matrix1 1)))
		    (if (and (= rows (array-dimension matrix2 0))
		             (= cols (array-dimension matrix2 1)))
		        (loop for r from 0 below rows always
		          (loop for c from 0 below cols always
		            (equalp (aref matrix1 r c) (aref matrix2 r c))))
		        nil)))
		
		(defun matrix-subtract (matrix1 matrix2)
		  "Subtracts the second matrix from the first element-wise.

		Parameters:
		  - MATRIX1: The matrix to subtract from.
		  - MATRIX2: The matrix to subtract.

		Returns:
		  - A new 2x2 matrix representing the difference."
		  (matrix-add matrix1 (scalar-multiply #C(-1.0 0.0) matrix2)))
		
		(defun matrix-multiply (matrix1 matrix2)
		  "Performs matrix multiplication on two 2x2 matrices.

		Parameters:
		  - MATRIX1: The left 2x2 matrix.
		  - MATRIX2: The right 2x2 matrix.

		Returns:
		  - A new 2x2 matrix representing the product."
		  (let ((rows1 (array-dimension matrix1 0))
		        (cols1 (array-dimension matrix1 1))
		        (cols2 (array-dimension matrix2 1))
		        (result (make-array '(2 2) :element-type 'complex-float :initial-contents '((#C(0.0 0.0) #C(0.0 0.0)) (#C(0.0 0.0) #C(0.0 0.0))))))
		    (loop for r from 0 below rows1 do
		      (loop for c from 0 below cols2 do
		        (loop for k from 0 below cols1 do
		          (incf (aref result r c) (* (aref matrix1 r k) (aref matrix2 k c))))))
		    result))
		
		
		;; Identity functions (Additive)
		
		(defun identity-1-check ()
		  "Verifies the additive identity: I = DepR + i * DepL†"
		  (matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 1.0) (complex-conjugate-transpose depl)))))

		(defun identity-2-check ()
		  "Verifies the additive identity: I = DepR - i * DepL"
		  (matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 -1.0) depl))))

		(defun identity-3-check ()
		  "Verifies the additive identity: I = 2*DepR - IndR - i*IndL"
		  (matrix-equalp identity-matrix
		                 (matrix-add (matrix-add (scalar-multiply 2 depr) (scalar-multiply #C(-1.0 0.0) indr)) (scalar-multiply #C(0.0 -1.0) indl))))

		(defun identity-4-check ()
		  "Verifies the additive identity: I = DepR + i/2 * (DepL† - DepL)"
		  (matrix-equalp identity-matrix
		                 (matrix-add depr (scalar-multiply #C(0.0 0.5)
		                                                   (matrix-subtract (complex-conjugate-transpose depl) depl)))))
		(defun identity-5-check ()
		  "Verifies the additive identity: I = 2*DepR - (IndR + i*IndL)"
		  (matrix-equalp identity-matrix
		                 (matrix-subtract (scalar-multiply 2 depr) (matrix-add indr (scalar-multiply #C(0.0 1.0) indl)))))
		
		;; Identity functions (Multiplicative)
		
		(defun multiplicative-identity-1-check ()
		  "Verifies the multiplicative identity: I = DepR + (DepL . DepL†)"
		  (matrix-equalp identity-matrix (matrix-add depr (matrix-multiply depl (complex-conjugate-transpose depl)))))

		(defun multiplicative-identity-2-check ()
		  "Verifies the multiplicative identity: I = DepR + (DepL† . DepL)"
		  (matrix-equalp identity-matrix (matrix-add depr (matrix-multiply (complex-conjugate-transpose depl) depl))))
		
		
		(defun check-matrix-identities ()
		  "Runs all the defined additive and multiplicative identity checks and prints
		the results to standard output. This function serves as the main entry point
		for verifying the algebraic properties of the core Weaver matrices.

		Side Effects:
		  - Prints the result (T or NIL) of each identity check."
		  (format t "Additive Identity 1 (I = DepR + i DepL*): ~A~%" (identity-1-check))
		  (format t "Additive Identity 2 (I = DepR - i DepL): ~A~%" (identity-2-check))
		(format t "Additive Identity 3 (I = 2 DepR - IndR - i IndL): ~A~%" (identity-3-check))
		(format t "Additive Identity 4 (I = DepR + i/2 (DepL* - DepL)): ~A~%" (identity-4-check))
		(format t "Additive Identity 5 (I = 2 DepR - (IndR + i IndL)): ~A~%" (identity-5-check))
		(format t "Multiplicative Identity 1 (I = DepR + (DepL . DepL*)): ~A~%" (multiplicative-identity-1-check))
		(format t "Multiplicative Identity 2 (I = DepR + (DepL* . DepL)): ~A~%" (multiplicative-identity-2-check)))
		
		
		(check-matrix-identities)