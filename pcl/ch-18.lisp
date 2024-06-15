;;;; Chapter 18 A Few FORMAT Recipes

;;; Format
(format t "~{~a~^, ~}" list)

;; ~{ ... ~} loop control
;; ~a prints as a humable
;; ~^ conditional newline
;; , the string

;;;; Format Directives

;; ~$ prints floating-point values
;; ~% prints new line
;; ~5$ to print 5 decimals points
;; ~v$ then num to print num decimals
;; ~#$
;; ~,5f prints first 5 of decimal for float
;; ~d output integers
;; ~:d display comma in integers
;; ~@d positive sign
;; #\ for character

;;;; Example

(defun sample ()
  (format t "~$~%" pi)
  (format t "~5$~%" pi)
  (format t "~v$~%" 6 pi)
  (format t "~#$~%" pi)
  (format t "~,5f~%" pi)
  (format t "~d~%" 1000000)
  (format t "~:d~%" 1000000)
  (format t "~@d~%" 1000000)
  (format t "~@:d~%" 1000000))

;;;; Basic Formatting

(defun sample1 ()
  (format t "The value is: ~a~%" 10)
  (format t "The value is: ~a~%" "foo")
  (format t "The value is: ~a~%" (list 1 2 3))
  (format t "Syntax error. Unexpected character: ~:c" #\space )
  (format t "~12,'0d" 1000000)
  (format nil "~4,'0d-~2,'0d-~2,'0d" 2002 4 7)) ;; ~12 digits '0d put in the missing space

;;;; Other forms

(defun sample2 ()
  (format t "Hexadecimal: ~x~%" 1000000) ; Hexadecimal
  (format t "Octal: ~o~%" 1000000) ; Octal
  (format t "Binary: ~b~%" 1000000)) ; Binary

;;;; Floating-Point Directives

(defun sample3 ()
  (format t "~f~%" pi) ; prints into scientific notaion
  (format t "~,4f~%" pi) ; prints 4 digits into scientific notation
  (format t "~e~%" pi) ; prints into scientific notation in other form
  (format t "~,4e~%" pi) ; prints 4 digits into scientific notation in other form 
  (format t "~$~%" pi) ;monetary like ~F
  (format t "~2,4$~%" pi))

;;;; English-Language Directives

(defun sample4 ()
  (format t "~r~%" 1234) ; prints numbers to words
  (format t "~:r~%" 1234) ; prints numbers to words in ordinal form
  (format t "~@r~%" 1234) ; prints into roman numeral
  (format t "~:@r~%" 1234) ; prints into old-style roman numerals
  (format t "ball~p~%" 1) ; prints singular excepts 0
  (format t "ball~p~%" 2) ;  prints plural
  (format t "~r file~:p~%" 1) ; with : prints numbers to words 
  (format t "~r file~:p~%" 10) ; with : prints numbers to words
  (format t "~r famil~:@p~%" 1) ; with @ prints number to words and changes y into ies
  (format t "~r famil~:@p~%" 10)) ; with @ prints number to words and changes y into ies

(defun sample5 ()
  (format t "~(~a~)~%" "FOO") ; make it all lowercase
  (format t "~(~@r~)~%" 124) ; make it all lowercase
  (format t "~(~a~)~%" "tHe Quick BROWN foX") ; make it all lowercase
  (format t "~@(~a~)~%" "tHe Quick BROWN foX") ; make it first letter of first word uppercase
  (format t "~:(~a~)~%" "tHe Quick BROWN foX") ; make it first letter of each word uppercase
  (format t "~:@(~a~)~%" "tHe Quick BROWN foX"))  ; make it all uppercase

;;;; Conditional Formatting

(defun sample6 ()
  (format t "~[wala~;isa~;dalawa~]~%" 0) 
  (format t "~[wala~;isa~;dalawa~]~%" 1)
  (format t "~[wala~;isa~;dalawa~]~%" 2)
  (format t "~[wala~;isa~;dalawa~:;iba~]~%" 100) ; else
  (format t "~@[x = ~a ~]~@[y = ~a~]" 10 20))

;;;; Iteration

(defun sample7()
  (format t "~{~a~^, ~}~%" (list 1 2 3))  ; ^ for not printing comma in the last element
  (format t "~{~a~#[~;, and ~:;, ~]~}~%" (list 1 2 3))) ; with and

;;;; Hop, Skip. Jump

(defun sample8 ()
  (format t "I saw ~r el~:*~[ves~;f~:;ves~].~%" 0) ; prints num to words and if more than 1 or 0 print ves
  (format t "I saw ~r el~:*~[ves~;f~:;ves~].~%" 1)
  (format t "I saw ~r el~:*~[ves~;f~:;ves~].~%" 2))



