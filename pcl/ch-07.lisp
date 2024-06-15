;;;; Chapter 7 Macros: Standard Control Constructs

;;; Spam-filtering program
(if (spam-p current-message)
    (file-in-spam-folder current-message)
    (update-spam-database current-message))

;;PROGN executes any number of forms in order and returns the value of the last form.
(if (spam-p current-message)
    (progn
      (file-in-spam-folder current-message)
      (update-spam-database current-message)))

;;;Defining WHEN as a macro
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

;;;UNLESS condition is false
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;;;COND is the better nested IF
(cond (a (do-x))
      (b (do-y))
      (t (do-z)))

;;;Looping

;;;DOLIST loops across the items of a list
(dolist (var list-form)
  body-form*)

;;; Example input
;;; (dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))

;;;DOTIMES high-level looping construct for counting loops
(dotimes (var count-form)
  body-form*)

;;; Example input
;;; (dotimes (i 4) (print i))

;;; Example of dotimes
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;;; DO examples
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

(do ()
    ((> (get-universal-time) *some-future-date*))
   (format t "Waiting~%")
   (sleep 60))

;;; LOOP with return
(loop
  (when (> (get-universal-time) *some-future-date*)
     (return))
  (format t "Waiting ...~%")
  (sleep 1))

;;; DO LOOP
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

;;;Output (1 2 3 4 5 6 7 8 9 10)

;;;LOOP Examples
(loop for x from 1 to 10 summing (expt x 2))

(loop for x across "the quick brown fox jumps over the lazy dog"
counting (find x "aeiou"))

(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))

;;;Loop for calendar
(defun test (a)
  (dotimes (x a)
    (if (zerop (mod (+ 1 x) 7))
      (format t "~3d~%" (+ 1 x)) 
      (format t "~3d" (+ 1 x)))))

;;;Sample of if condition
(defun condi ()
  (if-state (> 3 2)))

;;;Macro of IF
(defmacro if-state (condition)
  `(if ,condition
       ("Tama")
       ("Mali")))
					
;;;Macro of Cond
(defmacro cond-state (string)
  `(cond
     ((equal ,string "a") (print "First"))
     ((equal ,string "b") (print "Second"))
     ((equal ,string "c") (print "Third"))))

;;;Boolean Logic Operator

(defvar *eq1* (= 1 2))
(defvar *eq2* (= 3 3))

(defun wow ()
  (not *eq1*))

(defun wow1 ()
  (and *eq1* *eq2*))

(defun wow2 ()
  (or *eq1* *eq2*))















