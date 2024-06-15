
;;;; Chapter 15 Practical: A Portable Pathname Library

(defun foo () ; *Features* and Read-Time Conditionalization 
  #+allegro (do-one-thing)
  #+sbcl (do-another-thing)
  #+clisp (something-else)
  #+cmu (yet-another-version)
  #-(or allegro sbcl clisp cmu) (error "Not implemented"))

(defun do-another-thing () ;Sample
  (format t "hellow"))


(defpackage :rhon.licsi
  (:use :common-lisp)
  (:export
   :add))

(in-package :rhon.licsi)

(defun add ()
  (+ 3 6))

;;;; (in-package :cl-user) ;enter when going back to the cl-user


;;;; Creates a new pathname with specific components
(directory (make-pathname :name :wild :type :wild :defaults #P"" ))

;;;; Component checker
(defun comp-p (value)
  (and value (not (string= value "hi"))))

;;;; If there is no namefile and type it prints p
(defun dir-p (p)
  (and
    (not (comp-p (pathname-name p))) ; Name of file
    (not (comp-p (pathname-type p))) ; File type
    p))

;;;; Changes the pathname as part of the directory
(defun path-as-dir (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (dir-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

;;;; Making wild card pathnames into part of the directory
(defun dir-wild (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (path-as-dir dirname)))

;;;; Listing directories
(defun list-dir (dirname) ; directory name
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  z  (directory (dir-wild dirname)))


;;;;
(defun list-dir2 (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (dir-wild dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)))

    ;;; Other sample
    #+openmcl
    (directory wildcard :directories t)
    #+allegro
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))


;;;; Notes: probe-file is similar with file-exists-p

;;;; Sample Walking Directory Tree

; (defun walk-directory (dirname fn &key directories (test (constantly t)))
  ; (labels
     ; ((walk (name)
	; (cond
	  ; ((directory-pathname-p name)
	   ; (when (and directories (funcall test name))
	     ; (funcall fn name))
	    ;(dolist (x (list-directory name)) (walk x)))
	   ;((funcall test name) (funcall fn name)))))
   ; (walk (pathname-as-directory dirname))))





