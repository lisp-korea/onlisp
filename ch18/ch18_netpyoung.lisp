;; 18.1
;; 18.2
(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))
	(if rest
	    `((,rest (subseq ,seq ,n)))
	    (let ((p (car pat))
		  (rec (destruc (cdr pat) seq atom? (1+ n))))
	      (if (funcall atom? p)
		  (cons `(,p (elt ,seq ,n))
			rec)
		  (let ((var (gensym)))
		    (cons (cons `(, var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))

(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
			 (if (consp (car b))
			     (car b)
			     b))
		     binds)
	 ,(dbind-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
			    binds)
		    body))))
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(dbind (a b c) #(1 2 3)
       (list a b c))
(dbind (a (b c) d) '(1 #(2 3) 4)
  (list a b c d))

(dbind (a (b . c) &rest d) '(1 "hello" 2 3 4)
  (list a b c d))

;; ---------------------------------------------------
(destruc '(a b c) 'seq #'atom)
(dbind-ex (destruc '(a b c) 'seq #'atom) '(body))
;; ---------------------------------------------------

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
		  (mapcan
		   #'(lambda (pat)
		       (incf row)
		       (setq col -1)
		       (mapcar #'(lambda (p)
				   `(,p (aref ,gar
					      ,row
					      ,(incf col))))
			       pat))
		   pats))
	 ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
			 `(,(car p) (aref ,gar ,@(cdr p))))
		     pat)
	 ,@body))))

(defparameter ar (make-array '(3 3)))
(loop for r from 0 to 2 do
     (loop for c from 0 to 2 do
	  (setf (aref ar r c) (+ c (* r 10)))))

(with-matrix ((a b c)
	      (d e f)
	      (g h i)) ar
  (list a b c d e f g h i))

(with-array ((a 0 0) (d 1 1) (i 2 2)) ar
  (list a d i))


;; ---------------------------------------------------
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))
(defstruct visitor name title firm)
(defparameter theo (make-visitor :name "Theodebert"
				 :title 'king
				 :firm 'franks))

(with-struct (visitor- name firm title) theo
  (list name firm title))
;; ---------------------------------------------------
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

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun binding (x binds)
  (labels ((recbind (x binds)
	     (aif (assoc x binds)
		  (or (recbind (cdr it) binds)
		      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(match '(p a b c a) '(p ?x ?y c ?x))
(match '(p ?x b ?y a) '(p ?y b c a))
(match '(a b c) '(a a a))
(match '(p ?x) '(p ?x))
(match '(a ?x b) '(_ 1 _))

;; ---------------------------------------------------
(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
	 (let ,(mapcar #'(lambda (v)
			   `(,v (binding ',v it)))
		       (vars-in then #'atom))
	   ,then)
	 ,else))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
	     (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)))


(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
	    (values ?x ?y)
	    nil))

(abab '(hi ho hi ho))
;; ---------------------------------------------------
(match '(f ?x) '?x)
