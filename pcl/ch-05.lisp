;; Chapter 5 Functions

;; Defining new functions
(defun verbose-sum (x y)
 "Sum any two numbers after printing a message."
 (format t "Summing ~d and ~d.~%" x y)
 (+ x y))

;; Simple example of optional parameters
(defun foo (a b &optional c d) (list a b c d))

;; Other example
(defun foo (a &optional (b 10)) (list a b))

;; FORMAT has two required arguments, the stream and the control string.
;; The + function also takes a variable number of arguments—there is no particular reason to limit it to summing just two numbers; it will sum any number of values.

(format t "hello, world")
(format t "hello, ~a" name)
(format t "x: ~d y: ~d" x y)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)

;; Function that has only keyword parameters
(defun foo (&key a b c) (list a b c))


;; Mixing Different Parameter Types
(defun foo (x &optional y &key z) (list x y z))

;; Can be called like this
;(foo 1 2 :z 3)
;(foo 1)

;; Funcall is the one to use when you know the number of arguments you are going to pass to the function at the time you write the code.

(foo 1 2 3) ≡ (funcall #'foo 1 2 3)

;; Example og min to max stepping by step

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

;; APPLY is a function object but instead of individual arguments, it expects a list.

(apply #'plot plot-data)

;; LAMBDA directly describes what the function does.

(funcall #'(lambda (x y) (+ x y)) 2 3)
