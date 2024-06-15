;;;; Chapter 21 Programming in the Large: Packages and Symbols

;; cl:*package*

;; keyword:

;; For implementing and creating a package

(defpackage :com.gigamonkeys.email-db
  (:use :common-lisp) ; what language
  (:export :add ; what function to export
	   :sub)) 


(defun add (x y)
  (+ x y))

(defun sub (x y)
  (- x y))


;; To use packages export with other packages

(defpackage :com.gigamonkeys.try
  (:use :common-lisp :com.gigamonkeys.email-db)
  ; :import-from :name.file :name-of-function
  )

;; For reading the package in REPL

(in-package :com.gigamonkeys.email-db)





