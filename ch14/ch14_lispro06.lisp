;14. Anaphoric Macros

(let ((result (big-long-calculation)))
  (if result
      (foo result)))

	
(if (big-long-calculation)
    (foo it))

;14.1 Anaphoric Variants

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(aif (big-long-calculation)
     (foo it))

(let ((it (big-long-calculation)))
  (if it (foo it) nil))

(awhen (big-long-calculation)
       (foo it)
       (bar it))

(awhile (poll *fridge*)
	(eat it))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym)) ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

(aand (owner x) (address it) (town it))

(let ((own (owner x)))
  (if own
      (let ((adr (address own)))
	(if adr (town adr)))))

(aif #first argument#
     #expansion for rest of arguments#)

(aif #c 1 #
     . . .
     (aif #c n #
	  t). . .)

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
			(case (length args)
			  (0 nil)
			  (1 (car args))
			  (t `(let ((it ,(car args)))
				,(self (cdr args))))))
	       args)))

(defmacro acond (&rest clauses)		; wrong
  (if (null clauses)
      nil
    (let ((cl1 (car clauses)))
      `(let ((it ,(car cl1)))
	 (if it
	     (progn ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

#'
(lambda (x) (* x 2))

(defun count-instances (obj lists)
  (labels ((instances-in (list)
			 (if list
			     (+ (if (eq (car list) obj) 1 0)
				(instances-in (cdr list)))
			   0)))
    (mapcar #'instances-in lists)))


	
> (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
(1 2 1 2)


(alambda (x) (if (= x 0) 1 (* x (self (1- x)))))

 	
(defun count-instances (obj lists)
  (mapcar (alambda (list)
		   (if list
		       (+ (if (eq (car list) obj) 1 0)
			  (self (cdr list)))
		     0))
	  lists))

> (ablock north-pole
(princ "ho ")
(princ it)
(princ it)
(return-from north-pole))
ho ho ho
NIL

;14.2 Failure

> (cdr '(a))
NIL

> (= 1 0)
NIL

> (find-if #'oddp '(2 4 6))
NIL

> (find-if #'null '(2 nil 6))
NIL

> (setq synonyms '((yes . t) (no . nil)))
((YES . T) (NO))
> (assoc 'no synonyms)
(NO)

> (member-if #'null '(2 nil 6))
(NIL 6)

> (setf edible (make-hash-table)
(gethash 'olive-oil edible) t
(gethash 'motor-oil edible) nil)
NIL
> (gethash 'motor-oil edible)
NIL
T

(defun edible? (x)
  (multiple-value-bind (val found?) (gethash x edible)
    (if found?
	(if val 'yes 'no)
      'maybe)))

> (mapcar #'edible? '(motor-oil olive-oil iguana))
(NO YES MAYBE)

> (get 'life 'meaning (gensym))
#:G618

(defun edible? (x)
  (aif2 (gethash x edible)
	(if it 'yes 'no)
	'maybe))

(defun our-load (filename)
  (do-file filename (eval it)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
	 (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
	 (aif2 ,test
	       (progn ,@body)
	       (setq ,flag nil))))))

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

;14.3 Referential Transparency

(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
		     (awhile2 (read2 ,str)
			      ,@body))))


(list x
      (setq x (not x))
      x)

(defmacro if (test then &optional else)
  `(let ((that ,test))
     (if that ,then ,else)))

(let ((that 'which))
  ...)

