;;;; Chapter 16 Object Reorientation: Generic Functions


;;;; Variables
(defvar account "Rhon")
(defvar amount nil)

(defun add ()
  (setq amount 10000)
  (format t "This is the amount: ~a" amount))

;;;; defgeneric is an abstract operation but no implementation

;;; Example 
(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))

;;; Example
(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
   Signal an error if the current balance is less than amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount)) ;Minus the balance 


#|
This is another way to comment big portions
|#

;;;; NOTES: The nine combinations are named for the operators: +, AND, OR, LIST, APPEND, NCONC, MIN, MAX, and PROGN.

;;;; Continuation on Chapter 17
