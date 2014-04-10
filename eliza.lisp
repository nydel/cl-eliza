;; eliza.lisp
;; https://github.com/miercoledi/cl-eliza.git
;; simple eliza archetecture for commonlisp

;; dependencies
(ql:quickload '(:cl-ppcre :local-time))

(defpackage :eliza
  (:use :cl :cl-ppcre :local-time)
  (:export :random-elt :patterns :eliza-repl :search-string-for-pattern :process-string))

(in-package :eliza)

(defun random-elt (list)
  (nth (random (length list)) list))


(defparameter patterns 
;; lengthen this selection at will
;; -- this eliza will take as many variables as you like,
;; -- not just right-hand-side and left-hand-side;
;; -- see last pattern for example.
  '(("([\\w|\\s]*) is very ([\\w|\\s]*)"
     "are you sure that \\1 is \\2 or could it be otherwise?")
    ("i am a ([\\w|\\s]*)"
     "does being a \\1 define you?")
    ("i like ([\\w|\\s]*)"
     "what do you like about \\1"
     "is there anything you dislike about \\1"
     "other thatn \\1 what do you find to be important")
    ;; -- example pattern -- multiple variables
    ;; -- responds to "the rain in spain stays mainly on the plain"
    ("the ([\\w|\\s]*) in ([\\w|\\s]*) stays ([\\w|\\s]*) on the ([\\w|\\s]*)"
     "have you even been to \\2?"
     "i don't know much about \\1, or \\2, or \\3, or \\4."
     "so where's that bloody \\1?")))

(defun eliza-repl ()
  (let ((input (progn (print 'ELIZA>)
		      (read-line *standard-input* nil 'eof nil))))
    (cond ((or (null input)
	       (string-equal input "goodbye"))
	   nil)
	  (t
	   (format t "~&~a~&" (process-string input))
	   (eliza-repl)))))

(defun search-string-for-pattern (string)
  (loop for i in patterns
     when (scan (car i) string)
     return i))



(defun process-string (string)
  (let ((pattern (search-string-for-pattern string)))
    (cond (pattern
	   (regex-replace-all (car pattern)
			      string
			      (random-elt (cdr pattern))))
	  (t
	   nil))))
