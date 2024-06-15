;; Chapter 6 Variables

;; Sample function with variables
(defun foo (x y z) (+ x y z))

;; LET creates another binding
(defun foo (x)
  (format t "Parameter: ~a~%" x) ;x is argument
  (let ((x 2))
    (format t "Outer LET: ~a~%" x) ;x is 2
    (let ((x 3))
       (format t "Inner LET: ~a~%" x)) ;x is 3
     (format t "Outer LET: ~a~%" x))
   (format t "Parameter: ~a~%" x))

;; Sample Input and Output
;; CL-USER> (foo 1)
;; Parameter: 1
;; Outer LET: 2
;; Inner LET: 3
;; Outer LET: 2
;; Parameter: 1
;; NIL

;; dotimes loop prints 0-9, binds the variable x:

(dotimes (x 10) (format t "~d " x))

;; Nested LETs

(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

;; Example of DEFVAR and DEFPARAMETER
; DEFPARAMETER always assigns the initial value to the names variable while DEFVAR does so only if the variable is undefined.

(defvar *count* 0
  "Count of widgets made so far.")

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

;; Another example

(defun increment-widget-count () (incf *count*))

;; Lexical Bindings
(defun foo ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*)

;; INCF and DECF are examples of a kind of macro called modify macros.

;; Sample og INCF
; (incf (aref *array* (random (length *array*))))

;; ROTATEF and SHIFTF
; (rotatef a b) is simliar with (let ((tmp a)) (setf a b b tmp) nil)
; (shiftf a b 10) is similar with (let ((tmp a)) (setf a b b 10) tmp) 

