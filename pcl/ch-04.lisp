;; Chapter 4 Syntax and Semantics

;; Some examples of build s-expressions

;;x ; the symbol X
;;() ; the empty list
;;(1 2 3) ; a list of three numbers
;;("foo" "bar") ; a list of two strings
;;(x y z) ; a list of three symbols
;;(x 1 "foo") ; a list of a symbol, a number, and a string
;;(+ (* 2 3) 4) ; a list of a symbol, a list, and a number.


;; Sample Input in the REPL
; (* (+ 1 2) (- 3 4))

;; If simple format
; (if x (format t "yes") (format t "no"))


;; Quote which takes a single expression as its "argument" and simply returns it.

(quote (+ 1 2))

;; Formatting Lisp Code
defun print-list (list)
  (dolist (i list)
    (format t "item: ~a~%" i)))

;; Dont do this
(defun foo ()
  (dotimes (i 10)
    (format t "~d. hello~%" i)
)
)

;; Do this instead
(defun foo ()
  (dotimes (i 10)
    (format t "~d. hello~%" i)))


;;Commenting

;;;; Four semicolons are used for a file header comment.
;;; A comment with three semicolons will usually be a paragraph comment that applies to a larg  section of code that follows
;; Two semicolons indicate this comment applies to the code that follows. Note that this comment is indented the same as the code that follows.
; This comment applies to this line only






