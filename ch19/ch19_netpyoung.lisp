;; 19. Query Compiler
;; If small macros are extensions to Lisp, large macro define sub-languges within it

;; 19.1 The database
(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defparameter *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
	  ',args))

(fact painter reynolds joshua english)
(fact painter canale antonio venetian)
(db-query 'painter)

;; 19.2 Pattern-Matching Queries
;; Query compiler accepts a query and generates a program which, when run, yields the same result.

;; 19.3 A Query Interpreter
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (val (gensym))
	  (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
	 (if (or ,val ,win)
	     (let ((it ,val)) ,@(cdr cl1))
	   (acond2 ,@(cdr clauses)))))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
		    (aif (assoc x binds)
			 (or (recbind (cdr it) binds)
			     it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

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

(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or  (interpret-or  (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t   (lookup (car expr) (cdr expr) binds))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
    (union (vars-in (car expr) atom?)
	   (vars-in (cdr expr) atom?))))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v) `(,v (binding ',v ,binds)))
		     (vars-in query #'atom))
	 ,@body))))


(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

(lookup 'painter '(?x ?y english))

(interpret-query '(and (painter ?x ?y ?z)
		   (dates ?x 1697 ?w)))

;; 19.4 Restriction on Binding
;; (not (painter ?x ?y ?z))
(with-answer (painter hogarth ?x ?y)
  (princ (list ?x ?y)))

(with-answer (and (painter ?x _ _)
		  (dates ?x 1697 _))
  (princ (list ?x)))

(with-answer (or (dates ?x ?y 1772)
		 (dates ?x ?y 1792))
  (print (list ?x ?y)))

(with-answer (and (painter ?x _ english)
		  (dates ?x ?b _)
		  (not (and (painter ?x2 _ venetian)
			    (dates ?x2 ?b _))))
  (print ?x))

;; 19.5 A Query Compiler
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

(defun compile-query (q body)
  (case (car q)
    (and (compile-and (cdr q) body))
    (or (compile-or (cdr q) body))
    (not (compile-not (cadr q) body))
    (lisp `(if ,(cadr q) ,body))
    (t (compile-simple q body))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defmacro with-answer2 (query &body body)
  `(with-gensyms ,(vars-in query #'simple?)
     ,(compile-query query
		     `(progn ,@body))))

(with-answer (painter ?x ?y ?z)
  (format t "~A ~A is a painter.~%" ?x ?y))

(with-answer2 (painter ?x ?y ?z)
  (format t "~A ~A is a painter.~%" ?x ?y))



(with-answer2 (and (dates ?x ?b ?d)
		   (lisp (> (- ?d ?b) 70)))
  (format t "~A lived over 70 years.~%" ?x))
