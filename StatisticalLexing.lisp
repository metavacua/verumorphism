;;; ===============================


;;; Package for Core Functionality


;;; ===============================


(defpackage :statistical-presorting


(:use :cl)


(:export :presort-inputs :ngram-frequencies :entropy))



(in-package :statistical-presorting)



(defun entropy (bit-string)


"Calculates the entropy of a bit string."


(let ((length (length bit-string)))


(if (zerop length)


0.0


(let ((p0 (/ (count #\0 bit-string) length))


(p1 (/ (count #\1 bit-string) length)))


(let ((term0 (if (zerop p0) 0.0 (* p0 (/ (log p0) (log 2)))))


(term1 (if (zerop p1) 0.0 (* p1 (/ (log p1) (log 2))))))


(- (+ term0 term1)))))))



(defun ngram-frequencies (bit-string n)


"Calculates the frequency of n-grams in a bit string."


(let ((length (length bit-string)))


(if (zerop length) ; Add this check


(make-hash-table :test #'equal)


(if (or (<= n 0) (> n length))


(error "N-gram length must be positive and not greater than the string length.")


(let ((frequencies (make-hash-table :test #'equal)))


(loop for i from 0 to (- length n)


do (let ((ngram (subseq bit-string i (+ i n))))


(incf (gethash ngram frequencies 0))))


frequencies)))))



(defun presort-inputs (bit-strings)


"Presorts a list of bit strings based on their likelihood of representing valid expressions."


(let* (


(scored-strings (mapcar (lambda (bit-string)


(let* ((ent (entropy bit-string))


(trigram-freqs (if (>= (length bit-string) 3)


(ngram-frequencies bit-string 3)


(make-hash-table :test #'equal)))


(num-unique-trigrams (hash-table-count trigram-freqs))


(max-possible-trigrams (max 0 (- (length bit-string) 2)))


(trigram-diversity (if (zerop max-possible-trigrams)


0.0


(/ num-unique-trigrams max-possible-trigrams)))


(score (+ (* 0.8 (- 1 ent)) ; Adjusted weight


(* 0.2 trigram-diversity))))


(list bit-string ent trigram-diversity score)))


bit-strings))


(sorted-strings (sort scored-strings #'> :key #'fourth))) ; Sort by the combined score (now at index 3)


(mapcar #'first sorted-strings)))



;;; ===============================


;;; Package for Example Usage


;;; ===============================


(defpackage :statistical-presorting-example


(:use :cl :statistical-presorting)


(:export :string-to-bits :main))



(in-package :statistical-presorting-example)



(defun string-to-bits (char-string)


"Converts a character string to a string of bits using UTF-8 encoding."


(let* ((octets (sb-ext:string-to-octets char-string :external-format :utf-8))


(bits (make-array (* 8 (length octets)) :element-type 'bit)))


(loop for i from 0 below (length octets)


for octet = (aref octets i)


do (loop for j from 0 below 8


for bit-index = (+ (* 8 i) (- 7 j))


do (setf (aref bits bit-index) (ldb (byte 1 j) octet))))


(map 'string (lambda (b) (if (= b 1) #\1 #\0)) bits)))



(defun main ()


(format t "~%--- Example Usage (with Comparison Sorts) ---~%")


(let ((inputs (list


"A" "B" "AND" "^" "(AND A B)" "A AND B"


"00000000" "11111111" ; UTF-8 encoded


(make-string 10 :initial-element #\0) ; Trash control: all zeros


(make-string 10 :initial-element #\1) ; Trash control: all ones


(let ((random-state (make-random-state t))


(length 20))


(map 'string (lambda (x) (if x #\1 #\0))


(loop repeat length collect (zerop (random 2 random-state))))) ; Trash control: random bits


"This is a sentence." ; Natural language


"Another example sentence." ; Natural language


"Lorem ipsum dolor sit amet, consectetur adipiscing elit." ; Lorem Ipsum


"OR" "NOT A" "A XOR B"


"((A AND B) OR (C AND D))"


"A AND B" "( A AND B )"


"a and b"


"010101010101" "101010101010"


"001001001001" "110110110110"


(let ((random-state (make-random-state t)))


(map 'string (lambda (x) (if x #\1 #\0))


(loop repeat 5 collect (zerop (random 2 random-state)))))


(let ((random-state (make-random-state t)))


(map 'string (lambda (x) (if x #\1 #\0))


(loop repeat 30 collect (zerop (random 2 random-state)))))


"#@!%^&*()" "$$$$$$$$"


"" "0" "1" "01" "10" "00" "11"


"12345" "3.14159"


"def hello(): print('Hello')" "int main() { return 0; }"


"AQIDBA==" "SGVsbG8gV29ybGQh"


)))


(format t "Original inputs:~%")


(dolist (input inputs)


(format t "~a~%" input))



(let ((scored-data (mapcar (lambda (char-string)


(let* ((bit-string (string-to-bits char-string))


(ent (entropy bit-string))


(trigram-freqs (ngram-frequencies bit-string 3))


(num-unique-trigrams (hash-table-count trigram-freqs))


(max-possible-trigrams (max 0 (- (length bit-string) 2)))


(trigram-diversity (if (zerop max-possible-trigrams)


0.0


(/ num-unique-trigrams max-possible-trigrams)))


(current-score (+ (* 0.8 (- 1 ent))


(* 0.2 trigram-diversity))))


(list char-string bit-string ent trigram-diversity current-score)))


inputs)))



(format t "~%--- Sorted by Current Score ---~%")


(dolist (item (sort (copy-list scored-data) #'> :key #'fifth))


(format t "~a (Entropy=~10,8f, Trigram Diversity=~f, Score=~f)~%"


(first item) (third item) (fourth item) (fifth item)))



(format t "~%--- Sorted by Entropy (Ascending) ---~%")


(dolist (item (sort (copy-list scored-data) #'< :key #'third))


(format t "~a (Entropy=~10,8f, Trigram Diversity=~f, Score=~f)~%"


(first item) (third item) (fourth item) (fifth item)))



(format t "~%--- Sorted by Trigram Diversity (Descending) ---~%")


(dolist (item (sort (copy-list scored-data) #'> :key #'fourth))


(format t "~a (Entropy=~10,8f, Trigram Diversity=~f, Score=~f)~%"


(first item) (third item) (fourth item) (fifth item))))))




;;; ===============================


;;; Package for Unit Tests


;;; ===============================


(defpackage :statistical-presorting-tests


(:use :cl :statistical-presorting :statistical-presorting-example)


(:export :run-tests :run-all-tests)) ; Export run-all-tests



(in-package :statistical-presorting-tests)



(defun assert-equal (expected actual description)


"Asserts that the expected value is equal to the actual value."


(if (equal expected actual)


(format t "Test Passed: ~a~%" description)


(format t "Test Failed: ~a - Expected: ~s, Actual: ~s~%" description expected actual)))



(defun assert-true (condition description)


"Asserts that the condition is true."


(if condition


(format t "Test Passed: ~a~%" description)


(format t "Test Failed: ~a~%" description)))



(defun assert-error (function description)


"Asserts that the function call results in an error."


(handler-case (funcall function)


(error (e) (format t "Test Passed: ~a - Caught error: ~a~%" description e))


(:no-error (result) (format t "Test Failed: ~a - Expected an error, but got: ~s~%" description result))))



(defun run-entropy-tests ()


(format t "~%--- Running Entropy Tests ---~%")


(assert-equal 0.0 (entropy "") "Entropy of empty string")


(assert-equal 1.0 (entropy "01") "Entropy of '01'")


(assert-equal 1.0 (entropy "10") "Entropy of '10'")


(assert-equal 0.0 (entropy "0000") "Entropy of all zeros")


(assert-equal 0.0 (entropy "1111") "Entropy of all ones")


(assert-equal 1.0 (entropy "01010101") "Entropy of alternating bits")


(assert-equal 1.0 (entropy "10101010") "Entropy of alternating bits (opposite)")


(assert-equal 0.9182958340544896 (entropy "001") "Entropy of '001'")


(assert-equal 1.0 (entropy "0011") "Entropy of '0011'"))



(defun hash-table-equal (ht1 ht2)


"Checks if two hash tables have the same keys and values."


(and (= (hash-table-count ht1) (hash-table-count ht2))


(loop for key being the hash-keys of ht1


always (and (nth-value 1 (gethash key ht2)) ; Check if key exists


(equal (gethash key ht1) (gethash key ht2))))))



(defun run-ngram-frequencies-tests ()


(format t "~%--- Running N-gram Frequencies Tests ---~%")


(assert-error (lambda () (ngram-frequencies "010" 0)) "N-gram length 0 should error")


(assert-error (lambda () (ngram-frequencies "010" 4)) "N-gram length greater than string length should error")


(assert-true (hash-table-equal (make-hash-table :test #'equal) (ngram-frequencies "" 1)) "Empty string should have no n-grams")


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "0" ht) 1) (setf (gethash "1" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "01" 1)) "1-grams of '01'"))


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "01" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "01" 2)) "2-grams of '01'"))


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "0" ht) 2) (setf (gethash "1" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "001" 1)) "1-grams of '001'"))


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "00" ht) 1) (setf (gethash "01" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "001" 2)) "2-grams of '001'"))


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "001" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "001" 3)) "3-grams of '001'"))


(let ((ht (make-hash-table :test #'equal))) (setf (gethash "010" ht) 1) (setf (gethash "101" ht) 1) (assert-true (hash-table-equal ht (ngram-frequencies "0101" 3)) "3-grams of '0101'")))



(defun run-presort-inputs-tests ()


(format t "~%--- Running Presort Inputs Tests ---~%")


(let ((inputs '("01" "10")))


(let ((result (presort-inputs inputs)))


(assert-true (and (stringp (first result)) (stringp (second result))) "Presort returns a list of strings (short inputs)")))


(let ((inputs '("000" "001" "010" "100" "111")))


(let ((result (presort-inputs inputs)))


(assert-equal 5 (length result) "Presort returns the same number of inputs")))


(let ((inputs (mapcar #'statistical-presorting-example::string-to-bits '("A" "B" "AND" "^" "(AND A B)" "A AND B"))))


(let ((result (presort-inputs inputs)))


(assert-true (and (stringp (first result)) (stringp (second result))) "Presort handles bit strings from expressions")))


(let ((inputs (mapcar #'statistical-presorting-example::string-to-bits '("010101010101" "00000000" "11111111" "A")))


(expected-order (mapcar #'statistical-presorting-example::string-to-bits '("A" "010101010101" "00000000" "11111111")))) ; Expected order might need adjustment based on scoring


(let ((result (presort-inputs inputs)))


(assert-equal (length expected-order) (length result) "Presort handles mixed entropy inputs")))


(let ((inputs (mapcar #'statistical-presorting-example::string-to-bits '("" "0" "1" "01" "10" "00" "11"))))


(let ((result (presort-inputs inputs)))


(assert-equal 7 (length result) "Presort handles very short strings")))


(let ((inputs (mapcar #'statistical-presorting-example::string-to-bits '("A AND B" "( A AND B )"))))


(let ((result (presort-inputs inputs)))


(assert-equal 2 (length result) "Presort handles expressions with different spacing"))))



(defun run-all-tests ()


(run-entropy-tests)


(run-ngram-frequencies-tests)


(run-presort-inputs-tests)


(format t "~%--- All Tests Completed ---~%"))



;;; ===============================


;;; To run the example and tests:


;;; ===============================



(statistical-presorting-example:main)


(statistical-presorting-tests:run-all-tests) 