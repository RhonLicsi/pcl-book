;;;; Chapter 14 Files and File I/O

;;; Template in opening file and read each line 
(defun open-file ()
  (let ((in (open "/home/rhonlicsi/Desktop/test-14" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))


(defun open-file2 ()
  (let ((in (open "/home/rhonlicsi/Desktop/Files/Notes" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))

(defun open-file3 ()
  (let ((in (open "/home/rhonlicsi/Desktop/try" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))

;;; Putting file into a parameter
(defparameter *perline* (open "/home/rhonlicsi/Desktop/test-14"))

;;; CL function in writing data

;; WRITE-CHAR single char in stream
;; WRITE-LINE string
;; TERPRI—short for “terminate print”—unconditionally prints a newline character
;; FRESH-LINE prints a newline character unless the stream is at the beginning of a line.

;;; Opening file and writing a string/override
(defun openadd ()
  (with-open-file (stream "/home/rhonlicsi/Desktop/test-14" :direction :output :if-exists :append)
    (write-string "Hello, world! 3" stream)
    (close stream))) ; If you don't want to show the output it closes it.

;;; Creating file with added text when compiling 
(with-open-file (stream "/home/rhonlicsi/Desktop/try" :direction :output)
    (format stream "Hello test run."))

;;; Pathnames as Filenames

(defun pathcheck ()
  (values (pathname-directory (pathname "/home/rhonlicsi/Desktop/try.txt")) ; Tells the directory
  (pathname-name (pathname "/home/rhonlicsi/Desktop/try.txt")) ; Name of the file
  (pathname-type (pathname "/home/rhonlicsi/Desktop/try.txt")))) ; Tells the file type

;;; Prints pathname
(defun print-path-name ()
 (pathname "/home/rhonlicsi/Desktop/try.txt")) 

;;;
(defun printcheck ()
  (values (namestring #p "/home/rhonlicsi/Desktop/try.txt") ; Tells the directory
	  (directory-namestring #p "/home/rhonlicsi/Desktop/try.txt")  ; Name of the file
	  (file-namestring #p "/home/rhonlicsi/Desktop/try.txt"))) ; Tells the file type

;;; Constructing New Pathnames

(defun makenew ()
  (make-pathname
   :directory '(:absolute "home" "rhonlicsi" "Desktop")
   :name "try2"
   :type "txt"))

  
;;;; Changing directory component

(defun newback ()
  (make-pathname :directory '(:relative "backups")
		 :defaults #p"/home/rhonlicsi/Desktop/try.txt"))

;;;; Merging Pathnames

(defun merge-path ()
  (merge-pathnames #p"Desktop/try.html" #p"/home/rhonlicsi/"))

(defun merge-path1 ()
  (merge-pathnames #p"Desktop/try.html" #p"rhonlicsi/"))

;;;; Enough-Namestring with Merge-Pathnames

(defun merge-all ()
  (merge-pathnames
   (enough-namestring #p"/change/home/rhonlicsi/Desktop/test.html" #p"/change/")
   #p"/www-backups/"))

;;;; Changing just name and type

(defun name-type ()
  (make-pathname :name "test" :type "txt"))

;;;; Other I/O

(defvar *state* "hello")

(defun read-input ()
  (with-input-from-string (s *state*)
    (read s)))

(defun comstring ()
   (with-output-to-string (out)
     (format out "hello, world ")
     (format out *state*)
     (format out "~s" (list 1 2 3))))













