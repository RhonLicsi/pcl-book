;;;; Chapter 19 Beyond Exception Handling: Conditions and Restarts

;;; Example of Error Condition
(define-condition invalid-argument-error (error)
  ((text :initarg :text :reader text)))

;;; Check-argument
(defun check-arg (a)
  (unless (integerp a)
    (error 'invalid-argument-error :text "Argument must be an integer")))

;;; Example of using the Invalid-argument-error
(defun add (a b)
  (if (not (integerp a))
      (error invalid-argument-error :text "a must be an integer")
      (if (not (integerp b))
	  (error invalid-argument-error :text "b must be an integer")
	  (+ a b))))

;;;
(defun add1 (a b)
  (let ((failed-arguments nil))
    (handler-case (check-arg a)
      (invalid-argument-error ()
	(progn
	(format t "~a not an integer" a )
	(setf failed-arguments `t))))
     (handler-case (check-arg b)
      (invalid-argument-error ()
	(progn
	(format t "~a not an integer" b )
	(setf failed-arguments `t))))
    (unless failed-arguments
      (+ a b))))

;;;
(defun add2 (a b)
  (restart-case (check-arg a)
     (provide-default-value ()
       (format t "Provide new value of a: ")
       (setf a (parse-integer (read-line))))
     (evaluate-to-zero ()
       (setf a 0)))
 (restart-case (check-arg b)
     (provide-default-value ()
       (format t "Provide new value of b: ")
       (setf b (parse-integer (read-line))))
    (evaluate-to-zero ()
       (setf b 0)) )
  (+ a b))

(defun evaluate-to-zero (c)
  (invoke-restart 'evaluate-to-zero))

(defun provide-value (c)
  (invoke-restart 'provide-default-value))

(defun add3 (a b)
  (handler-bind ((invalid-argument-error #'provide-value))
     (add2 a b)))


























