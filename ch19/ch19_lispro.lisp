# ch19. A Query Compiler

# 19.1 The Database

(painter reynolds joshua english)

(dates reynolds 1723 1792)

(painter reynolds joshua 1723 1792 english)

(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
	  ',args))

> (fact painter reynolds joshua english)
(REYNOLDS JOSHUA ENGLISH)
> (fact painter canale antonio venetian)
(CANALE ANTONIO VENETIAN)
> (db-query 'painter)
((CANALE ANTONIO VENETIAN)
(REYNOLDS JOSHUA ENGLISH))
T

#19.2 Pattern-Matching Queries

#query# : (#symbol# #argument#*)
: (not #query#)
: (and #query#*)
: (or #query#*)
#argument# : ?#symbol#
: #symbol#
: #number#

(and (painter ?x ?y ?z)
     (dates ?x 1697 ?w))

(edible motor-oil not)

#19.3 A Query Interpreter

> (lookup 'painter '(?x ?y english))
(((?Y . JOSHUA) (?X . REYNOLDS)))

> (interpret-query '(and (painter ?x ?y ?z)
(dates ?x 1697 ?w)))
(((?W . 1768) (?Z . VENETIAN) (?Y . ANTONIO) (?X . CANALE))
((?W . 1772) (?Z . ENGLISH) (?Y . WILLIAM) (?X . HOGARTH)))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
			 `(,v (binding ',v ,binds)))
		     (vars-in query #'atom))
	 ,@body))))

(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lookup (car expr) (cdr expr) binds))))

(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
    (mapcan #'(lambda (b)
		(interpret-query (car clauses) b))
	    (interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
	      (interpret-query c binds))
	  clauses))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
    (list binds)))

(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
	      (aif2 (match x args binds) (list it)))
	  (db-query pred)))

(clear-db)

(fact painter hogarth william english)

(fact painter canale antonio venetian)

(fact painter reynolds joshua english)

(fact dates hogarth 1697 1772)

(fact dates canale 1697 1768)

(fact dates reynolds 1723 1792)

#19.4 Restrictions on Binding


(not (painter ?x ?y ?z))

(and (painter ?x ?y ?z) (not (dates ?x 1772 ?d)))

(and (not (dates ?x 1772 ?d)) (painter ?x ?y ?z)) ; wrong


> (with-answer (painter hogarth ?x ?y)
(princ (list ?x ?y)))
(WILLIAM ENGLISH)
NIL


> (with-answer (and (painter ?x _ _)
(dates ?x 1697 _))
(princ (list ?x)))
(CANALE)(HOGARTH)
NIL

> (with-answer (or (dates ?x ?y 1772)
(dates ?x ?y 1792))
(princ (list ?x ?y)))
(HOGARTH 1697)(REYNOLDS 1723)
NIL

> (with-answer (and (painter ?x _ english)
(dates ?x ?b _)
(not (and (painter ?x2 _ venetian)
(dates ?x2 ?b _))))
(princ ?x))
REYNOLDS
NIL

(or (painter ?x ?y ?z) (dates ?x ?b ?d))

#19.5 A Query Compiler

(defmacro with-answer (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
		 ,(compile-query query `(progn ,@body))))

(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defun compile-simple (q body)
  (let ((fact (gensym)))
    `(dolist (,fact (db-query ',(car q)))
       (pat-match ,(cdr q) ,fact ,body nil))))

(defun compile-and (clauses body)
  (if (null clauses)
      body
    (compile-query (car clauses)
		   (compile-and (cdr clauses) body))))

(defun compile-or (clauses body)
  (if (null clauses)
      nil
    (let ((gbod (gensym))
	  (vars (vars-in body #'simple?)))
      `(labels ((,gbod ,vars ,body))
	 ,@(mapcar #'(lambda (cl)
			(compile-query cl `(,gbod ,@vars)))
		    clauses)))))

(defun compile-not (q body)
  (let ((tag (gensym)))
    `(if (block ,tag
	   ,(compile-query q `(return-from ,tag nil))
	   t)
	 ,body)))


> (setq my-favorite-year 1723)
1723
> (with-answer (dates ?x my-favorite-year ?d)
(format t "~A was born in my favorite year.~%" ?x))
REYNOLDS was born in my favorite year.
NIL

(with-answer (painter ?x ?y ?z)
	     (format t "~A ~A is a painter.~%" ?y ?x))

(dolist (#:g1 (interpret-query '(painter ?x ?y ?z)))
  (let ((?x (binding '?x #:g1))
	(?y (binding '?y #:g1))
	(?z (binding '?z #:g1)))
    (format t "~A ~A is a painter.~%" ?y ?x)))

(with-gensyms (?x ?y ?z)
	      (dolist (#:g1 (db-query 'painter))
		(pat-match (?x ?y ?z) #:g1
			   (progn
			     (format t "~A ~A is a painter.~%" ?y ?x))
			   nil)))

> (with-answer (and (dates ?x ?b ?d)
(lisp (> (- ?d ?b) 70)))
(format t "~A lived over 70 years.~%" ?x))
CANALE lived over 70 years.
HOGARTH lived over 70 years.
NIL


> (with-answer (painter 'hogarth ?x ?y)
(princ (list ?x ?y)))
(WILLIAM ENGLISH)
NIL

> (with-answer (and (painter ?x _ 'english)
(dates ?x ?b _)
(not (and (painter ?x2 _ 'venetian)
(dates ?x2 ?b _))))
(princ ?x))
REYNOLDS
NIL


> (with-answer (and (painter ?x _ _)
(dates ?x _ ?d)
(lisp (< 1770 ?d 1800)))
(princ (list ?x ?d)))
(REYNOLDS 1792)(HOGARTH 1772)
NIL

