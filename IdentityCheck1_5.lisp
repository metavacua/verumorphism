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


;; Identity functions

(defun identity-1-check ()
  (matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 1.0) (complex-conjugate-transpose depl)))))

(defun identity-2-check ()
  (matrix-equalp identity-matrix (matrix-add depr (scalar-multiply #C(0.0 -1.0) depl))))

(defun identity-3-check ()
  (matrix-equalp identity-matrix
               (matrix-add (matrix-add (scalar-multiply 2 depr) (scalar-multiply -1 indr)) (scalar-multiply #C(0.0 -1.0) indl))))

(defun identity-4-check ()
  (matrix-equalp identity-matrix
               (matrix-add depr (scalar-multiply #C(0.0 1.0)
                                                 (matrix-add (complex-conjugate-transpose depl) (scalar-multiply #C(0.0 -1.0) depl))))))

(defun identity-5-check ()
  (matrix-equalp identity-matrix
               (matrix-add (scalar-multiply 2 depr) (matrix-add (scalar-multiply -1 indr) (scalar-multiply #C(0.0 -1.0) indl)))))


(defun check-matrix-identities ()
  (format t "Identity 1 (I = DepR + i DepL*): ~A~%" (identity-1-check))
  (format t "Identity 2 (I = DepR - i DepL): ~A~%" (identity-2-check))
  (format t "Identity 3 (I = 2 DepR - IndR - i IndL): ~A~%" (identity-3-check))
  (format t "Identity 4 (I = DepR + i/2 (DepL* - DepL)): ~A~%" (identity-4-check))
  (format t "Identity 5 (I = 2 DepR - (IndR + i IndL)): ~A~%" (identity-5-check)))

(check-matrix-identities)