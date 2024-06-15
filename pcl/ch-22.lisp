;;;; Chapter 22 LOOP for Black Belts


(defparameter *pangkat* '(a b c d e))

;; Simple 5 loops that increment
(defun sample-loop ()
  (loop for item in *pangkat*
	for i from 1 to 5
	do (format t "Item: ~a, Index: ~a~%" item i)))

;; Loops that counts down
(defun down-loop ()
  (loop for i from 20 downto 10 collect i))

;; Loops 1 word 5 times
(defun loop-one-word ()
  (loop repeat 5
	do (print "Hello, world!")))


(defparameter *list1* '(1 2 3 4 5))

;; Loop in a list

(defun loop-list ()
  (let ((result1 (loop for a in *list1* collect a)) 
        (result2 (loop for i in *list1* by #'cddr collect i)) 
	(result3 (loop for x across "abcd" collect x))) ; for vector
    (format t "First loop result: ~a~%" result1) 
    (format t "Second loop result: ~a~%" result2)
    (format t "Third loop result: ~a~%" result3)))


;; With = loop
(defun equal-loop ()
    (loop repeat 5
	  for x = 0 then y ; then is used to initialize the value of y to x
	  for y = 1 then (+ x y)
	  collect y))

;; With = and loop
(defun and-loop ()
  (loop repeat 5
	for x = 0 then y
	and y = 1 then (+ x y)
	collect y) )

;; Print inside the loop
(defun print-loop ()
    (loop for (a b) in '((1 2) (3 4) (5 6))
	  do (format t "a: ~a; b: ~a~%" a b)))

;; Note: Verbs that can be used; collect, append, nconc, count, sum, maximize, and minimize

(defparameter *random* (loop repeat 100 collect (random 10000)))


(defun verb-loop ()
  (loop for i in *random* ; how many elements
	counting (evenp i) into evens ; num of even
	counting (oddp i) into odds ; num of odd
	summing i into total ; sum of all elements
	maximizing i into max ; highest value 
	minimizing i into min ; lowest value 
	finally (return (list min max total evens odds))) ) ; prints the values


;;; Unconditional Execution

(defun print-1-to-10 ()
  (loop for i from 1 to 10 do (print i)))

;; Note: The do and return clauses are collectively called the unconditional execution clauses.

;;; Conditional Execution

(defun even-loop ()
  (loop for i from 1 to 10 do (when (evenp i) (print i))))

(defun sum-even-loop ()
  (loop for i from 1 to 10 when (evenp i) sum i))
					
;; Note: Control constructs such as IF and WHEN

(defparameter numbers '(2 4 6))

(defun even-check()
    (if (loop for n in numbers always (evenp n))
	(print "All numbers even.")))

