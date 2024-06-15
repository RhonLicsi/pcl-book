;; Chapter 8 Macros: Defining your own

;; A Sample Macro: do-primes
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; 
(do-primes (p 0 19)
  (format t "~d " p))

;; Without do-primes macro
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
   (format t "~d " p))

;; easy to understand do-primes

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;;backquoted version of do-primes

(defmacro do-primes-a ((var start end) &body body)
  (append '(do)
          (list (list (list var
                            (list 'next-prime start)
                            (list 'next-prime (list '1+ var)))))
          (list (list (list '> var end)))
          body))

;;Sample input of it
; (do-primes (p 0 19) (format t "~d " p))


(defmacro with-database-connection ((var &rest open-args) &body body)
  `(let ((,var (open-connection ,@open-args)))
     (unwind-protect (progn ,@body)
       (close-connection ,var))))











