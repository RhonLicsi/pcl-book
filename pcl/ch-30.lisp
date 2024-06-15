;;;; Chapter 30 Practical: An HTML Generation Library, the Interpreter


(defpackage :com.gigamonkeys.html
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :with-html-output
           :in-html-style
	   :define-html-macro
           :html
	   :emit-html
           :&attributes))

(in-package :com.gigamonkeys.html)

(defvar *html-output* *standard-output*)

(defun emit-html (html)
  "An interpreter for the literal HTML language."
  (write-sequence html *html-output*))

(defmacro html (html)
  "A compiler for the literal HTML language."
  `(write-sequence ,html *html-output*))

(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
	   (and (consp (car form)) (funcall test (caar form))))))

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
      (parse-explicit-attributes-sexp sexp)
      (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
	for rest on (rest sexp) by #'cddr
	while (and (keywordp (first rest)) (second rest))
	when (second rest)
	  collect (first rest) into attributes and
	collect (second rest) into attributes
	end
	finally (return (values tag attributes rest))))

;; Single character and returns a string containing a character
(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

;;
(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	    for pos = (position-if #'needs-escape-p in :start start)
	    do (write-sequence in out :start start :end pos)
	    when pos do (write-sequence (escape-char (char in pos)) out)
	      while pos))))

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")
