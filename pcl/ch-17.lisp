;;;; Chapter 17 Object Reorientation: Classes

;;;; Defclass Format
(defclass name (direct-superclass-name*)
  (slot-specifier*))


(defvar *account-numbers* 0) ; How many accounts
(defvar *opening-bonus* 0) ; Bonus
(defvar *list-account* '()) ; Holder of the accouts

;;;; Creates each bank account
(defclass bank-account () ;:accesor (creates reader and SETF function) and :reader 
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   (account-type
    :initarg :account-type)
   (opening-bonus-percentage
    :initarg :opening-bonus-percentage
    :initform (incf *opening-bonus*))
   

   ))

(defparameter *account* nil)

;;;; Creating account
(defun create-acct (name bal bonus)
  (let* ((type (cond
                 ((>= bal 100000) "Gold")
                 ((>= bal 50000) "Silver")
                 (t "Bronze")))
	 ;(bonus (* bal (/ bonus 100)))
         (account (make-instance 'bank-account
                                 :customer-name name
                                 :balance bal
                                 :account-type type
				 :opening-bonus-percentage bonus)))
    (push account *list-account*)
    (setf *account* account)))

(defparameter *minimum-balance* 1500 )

;;;; Bonus Percentage + Balance
(defmethod initialize-instance :after ((account  bank-account)
				       &key opening-bonus-percentage )
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
	  (* (slot-value account 'balance) (/ opening-bonus-percentage 100))))
  
  (when (< (slot-value account 'balance) *minimum-balance*)
    (decf (slot-value account 'balance) (* (slot-value account 'balance) 0.01)))
  
  (when (< (slot-value account 'balance) *sample-withdraw*)
    (error "Account overdrawn."))
  (decf (slot-value account 'balance) *sample-withdraw*))

;;;; Printing of accounts
(defun display-all-accounts ()
  (dolist (account *list-account*)
    (print account)
    (format t "Account Number: ~a, Account Type: ~a, Customer Name: ~a, Bonus: ~a, Balance: ~a~%,"
            (slot-value account 'account-number)
	    (slot-value account 'account-type)
            (slot-value account 'customer-name)
	    (slot-value account 'opening-bonus-percentage)
            (slot-value account 'balance))))

(defparameter *sample-withdraw* 70 )






;;-----------
(defmethod withdraw ((account bank-account) *sample-withdraw*)
  (when (< (slot-value account 'balance)  *sample-withdraw*)
    (error "Account overdrawn."))
  (decf (slot-value account 'balance)  *sample-withdraw*))

(defmethod low-balance :after ((account bank-account))
  (when (< (balance account) *minimum-balance*)
    (decf (slot-value account 'balance) (* (balance account) .01))))

(defvar *list1* '((1 2 3) (4 5 6) (7 8 9)))
(defun act ()
  (apply #'mapcar #'(lambda (&rest args) args) *list1*)


