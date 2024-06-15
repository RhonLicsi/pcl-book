;Chapter 12, They called it Lisp for a reason

(defparameter *cons* (cons 1 2))


;Sample input to change the first data since it is CAR
;(setf (car *cons*) 10)

;Sample input to change the first data since it is CAR
;(setf (cdr *cons*) 20)

(defparameter *list* (list 1 2 3 4))

(defparameter *list-1* (list 1 2 3 ))
(defparameter *list-2* (list 4 5 6 ))
(defparameter *list-3* (append *list-1* *list-2*))

;Sample input for concatinating list to list using nconc
;(nconc *list* (list 4 5 6))

;For reversing a list
;(setf *list* (reverse *list*))

;For deleting 1 in list
;(setf *list-3* (delete 4 *list-3*)) 

;adding using map 
;(mapcar #'+ (list) (list))

(defmacro def (&body body) `(defun ,@body))
					;Sample input (macroexpand-1 '(flet* (foo) ((fn (x) (+ x 1))) (fn foo)))

(let ((x))
  (princ x))

















