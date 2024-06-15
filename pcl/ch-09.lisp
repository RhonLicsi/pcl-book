;; Chapter 9 Practical: Building a Unit Test Framework

;; For testing purposes (testcase)

(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

;; Input (test-+)

;; More improved version
(defun ttest-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; Refactoring


;;Declaring 
(defvar *test-name* nil)

;;Returning with a value
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  ;result)

;; For better printing
(defun tttest-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;;Macro check
(defmacro check (form)
  `(report-result ,form ',form))

;;Macro PROGN
(defmacro checker (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;; Defining GENSYMS macro
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;Combining Results with GENSYMS
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

;;Having macro check with combine results 
(defmacro check1 (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;;Abstraction Emerges
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-arithmetic ()
  (combine-results
    (ttttest-+)
    (test-*)))

(deftest ttttest-+ ()
  (check1
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(deftest test-* ()
  (check1
   (= (* 2 2) 4)
   (= (* 3 5) 15)))

(deftest test-math ()
  (test-arithmetic))


















