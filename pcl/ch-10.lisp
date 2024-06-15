;; Chapter 10 Numbers, Characters, and Strings

;; Conversions

; 10 ; 10
; 20/2 ; 10
; #xa ; 10

;123 → 123
;+123 → 123
;-123 → -123
;123. → 123
;2/3 → 2/3
;-2/3 → -2/3
;4/6 → 2/3
;6/3 → 2
;#b10101 → 21
;#b1010/1011 → 10/11
;#o777 → 511
;#xDADA → 56026
;#36rABCDEFGHIJKLMNOPQRSTUVWXYZ → 8337503854730415241050377135811259267835

(defun parse-numeric-string (str)
  (let ((parsed (ignore-errors (read-from-string str))))
    (if (numberp parsed)
        parsed
        str)))

;; Notes
;= STRING= STRING-EQUAL
;/= STRING/= STRING-NOT-EQUAL
;< STRING< STRING-LESSP
;> STRING> STRING-GREATERP
;<= STRING<= STRING-NOT-GREATERP
;>= STRING>= STRING-NOT-LESSP














