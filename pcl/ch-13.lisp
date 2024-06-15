;Chapter 13 Beyond Lists: Other Uses for Cons Cells

;Trees
;has SUBSTITUTE functions

(defvar *tree* '(1 2 (3 2 1) ((1 1) (2 2))))

(defun sub(*tree*)
  (subst 10 1 *tree*))

;Sets
;has ADJOIN to create new cells but doesnt affect the original list
;has SUBSETP a checker 1st list to the 2nd list
(defvar *set* ())

(defun jn (*set*)
  (setf *set* (adjoin 1 *set*)))

(defun psh (*set*)
  (pushnew 3 *set*))

(defvar *set1* '(4 5 6))
(defvar *set2* '(6 4 3))

(defun checker ()
  (subsetp *set1* *set2* ))


;Alists
;has a function of CAR matches key

(defvar *set3* '(((a . 1) (b . 2) (c . 3))))
(defvar *set4* '((("a" . 1) ("b". 2) ("c". 3))))
(defvar *set5* '(A B C))

;for first element
(defun aso ()
 (assoc 'c (car *set3*)))

;for last element
(defun aso2 ()
 (cdr(assoc 'a *set3*)))

;for string 
(defun aso3 ()
  (assoc "a" (car *set4*) :test #'string=))

;pairlis
(defun pair ()
  (pairlis *set1* *set5*))

;Plist
;has function of GETF

(defvar *plist* ())

;getf function
(defun gets ()
 (setf (getf *plist* :a) 6))

;remf function
(defun remrem ()
  (remf *plist* :a))

;Destructing Bind


































