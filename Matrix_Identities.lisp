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
		(let ((rows (array-dimension matrix1 0))
		(cols (array-dimension matrix1 1))
		(result (make-array (array-dimensions matrix1) :element-type 'complex-float)))
		(loop for r from 0 below rows do
		(loop for c from 0 below cols do
		(setf (aref result r c) (+ (aref matrix1 r c) (aref matrix2 r c)))))
		result))
		
		(defun scalar-multiply (scalar matrix)
		(let ((rows (array-dimension matrix 0))
		(cols (array-dimension matrix 1))
		(result (make-array (array-dimensions matrix) :element-type 'complex-float)))
		(loop for r from 0 below rows do
		(loop for c from 0 below cols do
		(setf (aref result r c) (* scalar (aref matrix r c)))))
		result))
		
		(defun complex-conjugate-transpose (matrix)
		(let ((rows (array-dimension matrix 0))
		(cols (array-dimension matrix 1))
		(result (make-array '(2 2) :element-type 'complex-float)))
		(loop for r from 0 below rows do
		(loop for c from 0 below cols do
		(setf (aref result c r) (conjugate (aref matrix r c)))) )
		result))
		
		(defun matrix-equalp (matrix1 matrix2)
		(let ((rows (array-dimension matrix1 0))
		(cols (array-dimension matrix1 1)))
		(if (and (= rows (array-dimension matrix2 0))
		(= cols (array-dimension matrix2 1)))
		(loop for r from 0 below rows always
		(loop for c from 0 below cols always
		(equalp (aref matrix1 r c) (aref matrix2 r c))))
		nil)))
		
		(defun matrix-subtract (matrix1 matrix2)
		(matrix-add matrix1 (scalar-multiply #C(-1.0 0.0) matrix2)))
		
		(defun matrix-multiply (matrix1 matrix2)
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
		(matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 1.0) (complex-conjugate-transpose depl)))))
		
		(defun identity-2-check ()
		(matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 -1.0) depl))))
		
		(defun identity-3-check ()
		(matrix-equalp identity-matrix
		(matrix-add (matrix-add (scalar-multiply 2 depr) (scalar-multiply #C(-1.0 0.0) indr)) (scalar-multiply #C(0.0 -1.0) indl))))
		
		(defun identity-4-check ()
		(matrix-equalp identity-matrix
		(matrix-add depr (scalar-multiply #C(0.0 0.5)
		(matrix-subtract (complex-conjugate-transpose depl) depl)))) )
		(defun identity-5-check ()
		(matrix-equalp identity-matrix
		(matrix-subtract (scalar-multiply 2 depr) (matrix-add indr (scalar-multiply #C(0.0 1.0) indl)))))
		
		;; Identity functions (Multiplicative)
		
		(defun multiplicative-identity-1-check ()
		(matrix-equalp identity-matrix (matrix-add depr (matrix-multiply depl (complex-conjugate-transpose depl)))))
		
		(defun multiplicative-identity-2-check ()
		(matrix-equalp identity-matrix (matrix-add depr (matrix-multiply (complex-conjugate-transpose depl) depl))))
		
		
		(defun check-matrix-identities ()
		(format t "Additive Identity 1 (I = DepR + i DepL*): ~A~%" (identity-1-check))
		(format t "Additive Identity 2 (I = DepR - i DepL): ~A~%" (identity-2-check))
		(format t "Additive Identity 3 (I = 2 DepR - IndR - i IndL): ~A~%" (identity-3-check))
		(format t "Additive Identity 4 (I = DepR + i/2 (DepL* - DepL)): ~A~%" (identity-4-check))
		(format t "Additive Identity 5 (I = 2 DepR - (IndR + i IndL)): ~A~%" (identity-5-check))
		(format t "Multiplicative Identity 1 (I = DepR + (DepL . DepL*)): ~A~%" (multiplicative-identity-1-check))
		(format t "Multiplicative Identity 2 (I = DepR + (DepL* . DepL)): ~A~%" (multiplicative-identity-2-check)))
		
		
		(check-matrix-identities)