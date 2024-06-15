;; Chapter 11 Collections

;;(vector) → #()
;;(vector 1) → #(1)
;;(vector 1 2) → #(1 2)

;;Making a vector that has zero inside but has 5 index
;(make-array 5 :fill-pointer 0) → #()
;(defparameter *x* (make-array 5 :fill-pointer 0))

;;Function of push and pop for Vector
;(vector-push 'a *x*)
;(vector-pop *x*)


;defparameter *x* (vector 1 2 3))

;Syntax of length and elt
;(length *x*) → 3
;(elt *x* 0) → 1


;(defmacro array1 (name size)
  ;`(defparameter ,name (arraytype ,size)))

(defvar *size* nil) ; Define a global variable *size* with an initial value of nil

(defun arraytype (size)
  (setf *size* size) ; Set the global variable *size* to the provided size
  (make-array size :fill-pointer 0))

(defparameter *x* (arraytype *size*))

(defvar *wording* nil) ;array of words

(defun add (word)
  (push word *wording*))

(defun addnum (num)
  (push num *num*))


;Counter function 
(defvar *input* nil)

(defun counter (input)
  (count input *wording* :test #'string=))

;Sorting function
(defun sorting ()
  (setf *wording* (sort *wording* #'string<)))

(defvar *num* nil ) ;array of num

;Counter even
(defun counter-even ()
    (count-if #'evenp *num*))

;Counter odd 
(defun counter-odd ()
    (count-if-not #'evenp *num*))

;Use of Map
(defun multiply (vector1 vector2)
  (map 'vector #'* vector1 vector2))


(defparameter *h* (make-hash-table))

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
  (if present
    (format nil "Value ~a actually present." value)
    (format nil "Value ~a because key not found." value))))
(setf (gethash 'bar *h*) nil)





