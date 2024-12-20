;;;; Chapter 20 Special Operators

(defun calc ()
  (flet ((add (x) (+ x 1)))   ; Define a local function
    (flet ((sub (y) (- y 1))) ; Define another local function
      (list (add 5)           
            (sub 5)))))

;;; Format of flet
; (flet (function-definition*)
 ; body-form*)


;;; Format of labels
; (labels (function-definition*)
 ; body-form*)

(defun factorial (n)
  (labels ((temp (x acc)              ; Define a local recursive function
             (if (zerop x)
                 acc
                 (temp (- x 1) (* acc x)))))
    (temp n 1)))                     ; Call the local recursive function


;;; Block Skeleton ; Use to break out such as loops
; (block name
 ; form*)

;;;
(block test-block
  (defun test()
    (dotimes (i 10)
      (let ((answer (random 100)))
	(print answer)
	(if (> answer 50) (return))))))

;;; Tag Body Skeleton
; (tagbody
 ; tag-or-compound-form*)

(defun calc (option x y)
  (tagbody
     (cond ((string= option "1") (go add-case))
           ((string= option "2") (go sub-case))
           ((string= option "3") (go multi-case))
           ((string= option "4") (go div-case)))
   add-case
     (print (+ x y))
     (return-from calc)
   sub-case
     (print (- x y))
     (return-from calc)
   multi-case
     (print (* x y))
     (return-from calc)
   div-case
     (if (zerop y)
         (format t "Cannot divide by zero"))
     (print (/ x y))
     (return-from calc)))

;;; Sample of catch throw with calling inside function
(defparameter *obj* nil) ; i.e. some arbitrary object

(defun foo ()
  (format t "Entering foo~%")
  (catch *obj*
    (format t " Entering CATCH~%")
    (bar)
    (format t " Leaving CATCH~%"))
  (format t "Leaving foo~%"))

(defun bar ()
  (format t " Entering bar~%")
  (baz)
  (format t " Leaving bar~%"))

(defun baz ()
  (format t " Entering baz~%")
  (format t " Leaving baz~%")
  (throw *obj* nil))


;;; Hash table 
(defvar *h* (make-hash-table)) 

(defun add-hash ()
  (setf (gethash 'a *h*) 10)
  (setf (gethash 'b *h*) 2)
  (+ (gethash 'a *h*) (gethash 'b *h*)))


;;; Values-list
(defvar lit '(1 2 3))

(defun list-to-values ()
  (values-list lit))

;;; Sample of funcall
; (funcall #'+ (values 1 2) (values 3 4))


;;; Sample of multiple-value-call
; (multiple-value-call #'+ (values 1 2) (values 3 4))


;;; Sample of multiple-value-bind

(defun multiple-bind ()
  (multiple-value-bind (x y) (values 1 2)
    (+ x y)))

;;; Sample of multiple-value-list

(defun multiple-list ()
  (multiple-value-list (values 1 2)))

(defparameter *word* "madam im adam")
(defparameter *a* (reverse *word*))


(defun palindromep ()
  (if (equal *a* *word*)
      (format t "true ~a" *a*)
      (format nil "mali ka tanginamo")))

(defun reverse1 ()
  (reverse *word*))
