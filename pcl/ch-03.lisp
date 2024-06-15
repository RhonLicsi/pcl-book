;; Chapter 3 Practical: A Simple Database

;; Types of List in Lisp

(list 1 2 3)

(list :a 1 :b 2 :c 3)

(getf (list :a 1 :b 2 :c 3) :a) 

;; Example of List Function
defun make-cd (title artist rating ripped)
(list :title title :artist artist :rating rating :ripped ripped))

;; Example input of the Function
;; (make-cd "Roses" "Kathy Mattea" 7 t)

;; Making a global variables
(defvar *db* nil)

;; Adding a record
(defun add-record (cd) (push cd *db*))

;; Example input
;; (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;; Looking the database contents input *db*
;; For making more readable format use dump-db

;; Readable format using function
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Other options for more human-readable form the use of ~a and ~t
;; Example input (format t "~a" "Dixie Chicks") and > (format t "~a:~10t~a" :artist "Dixie Chicks")


;; Making the dump-db into a one-liner function
(defun dump-db ()
(format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; Improving the User Interaction
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Combination of make-cd and prompt-read
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))

;; Parsing an Integer
(parse-integer (prompt-read "Rating"))

;; Parsing any non-numeric junk
(parse-integer (prompt-read "Rating") :junk-allowed t)

;; If it still has an error in Parsing to get a default value of 0
(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)

;; Making a Y-OR-N-P function
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;; Making a simple LOOP macro
defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; Saving and Loading the database
;; Save db

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; Saving the database with name
;; Sample input (save-db "~/my-cds.db")

;; Loading Function
(defun load-db (filename)
  (with-open-file (in filename) 
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Querying the Database
;; Sample input  (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
;; or (remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
;; Sample output ( 2 4 6 8 10)
;; For odd (remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

;; Removing artist name using a function
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

;; Selector function

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; Example of function that selects the artist

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;;List that has a parameters
(defun foo (&key a b c) (list a b c))

;; For finding selector function
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
       (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; Update for existing records function
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
         (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; Sample input  (update (where :artist "Dixie Chicks") :rating 11)

;; Delete rows for the database
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; Removing Duplication
(if title (equal (getf cd :title) title) t)

;; Making a comparison
(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

;; Sample Input
;;CL-USER> (make-comparison-expr :rating 10)
;;(EQUAL (GETF CD :RATING) 10)
;;CL-USER> (make-comparison-expr :title "Give Us a Break")
;;(EQUAL (GETF CD :TITLE) "Give Us a Break")

;; Making a comparison in a field
(defun make-comparison-expr (field value)
`(equal (getf cd ,field) ,value))

;; Making a comparison list
(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields)))

