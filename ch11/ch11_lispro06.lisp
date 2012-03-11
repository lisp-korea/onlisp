;11. Classic Macros 매크로의 정수

;11.1 Creating Context

(let ((x 'b)) (list x))

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
			 (if (consp x) (car x) x))
		     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
		   (if (consp x) (cadr x) nil))
	       binds)))

(our-let ((x 1) (y 2))
	 (+ x y))


((lambda (x y) (+ x y)) 1 2)

;; 뒤에 나오는 when-bind* 매크로를 선정의 후 테스트
(when-bind* ((x (find-if #'consp '(a (1 2) b)))
(y (find-if #'oddp x)))
(+ y 10))
;11


(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
	(x0 (gensym)) (y0 (gensym))
	(x1 (gensym)) (y1 (gensym)))
    ...))

(defmacro with-redraw ((var objs) &body body)
  (with-gensyms (gob x0 y0 x1 y1)
		...))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
	   (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(let ((sun-place 'park) (rain-place 'library))
  (if (sunny)
      (visit sun-place)
    (visit rain-place)))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
	(vars (mapcar #'(lambda (v) (cons v (gensym)))
		      (remove-duplicates
		       (mapcar #'car
			       (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
		      ,@body))
       (cond ,@(mapcar #'(lambda (cl)
			    (condlet-clause vars cl bodfn))
			clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
((= 1 1) (y (princ 'c)) (x (princ 'd)))
(t (x (princ 'e)) (z (princ 'f))))
(list x y z))
;CD
;(D C NIL)
;MAPPEND가 정의되어 있지 않다고 나온다.

;11.2 The with- Macro

(with-open-file (s "dump" :direction :output)
		(princ 99 s))

(setq x 'a)
;A
> (unwind-protect
(progn (princ "What error?")
(error "This error."))
(setq x 'b))
;What error?
;>>Error: This error.

x
;B


(let ((temp *db*))
  (setq *db* db)
  (lock *db*)
  (prog1 (eval-query q)
    (release *db*)
    (setq *db* temp)))


(with-db db
	 (eval-query q))

;Pure macro:

 	
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
	   (progn
	     (setq *db* ,db)
	     (lock *db*)
	     ,@body)
	 (progn
	   (release *db*)
	   (setq *db* ,temp))))))

;Combination of macro and function:

 	
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gbod #'(lambda () ,@body)))
       (declare (dynamic-extent ,gbod))
       (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
      (progn
	(setq *db* new-db)
	(lock *db*)
	(funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
	     ((zerop ,g) ,zero)
	     (t ,neg)))))


;11.3 Conditional Evaluation


(if t
    'phew
  (/ x 0))


(while (not sick)
  (if3 (cake-permitted)
       (eat-cake)
       (throw 'tantrum nil)
       (plead-insistently)))


;; 11.2 에 제시된 nif 실행 후 확인
(mapcar #'(lambda (x)
(nif x 'p 'z 'n))
'(0 1 -1))
;(Z P N)

(let ((x (foo)))
  (or (eql x (bar)) (eql x (baz))))

(member (foo) (list (bar) (baz)))

(in (foo) (bar) (baz))

(let ((#:g25 (foo)))
  (or (eql #:g25 (bar))
      (eql #:g25 (baz))))


(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		      choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
			   `',a)
		       args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
			  `(funcall ,fnsym ,c))
		      choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
			clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
	  ((inq key t otherwise) `(t ,@rest))
	  (t (error "bad >case clause")))))


(inq operator + - *)

(in operator '+ '- '*)

(member x (list a b) :test #'equal)


(in-if #'(lambda (y) (equal x y)) a b)
;NIL

;;확장


(some #'oddp (list a b))


(in-if #'oddp a b)
;(mac (in-if #'oddp 2 4 6))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))


;; 10.1 에도 나오는 유용한 매크로이다.
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;11.4 Iteration

(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

;; OPEN PATH
(do-tuples/o (x y) '(a b c d)
(princ (list x y)))
;(A B)(B C)(C D)
;NIL

;CLOSED PATH
(do-tuples/c (x y) '(a b c d)
(princ (list x y)))
;(A B)(B C)(C D)(D A)
;NIL

(do-tuples/o (x y) points (drawline x y))

(do-tuples/c (x y) points (drawline x y))

(do-tuples/o (x) '(a b c) (princ x))
;ABC
;NIL
(do-tuples/c (x) '(a b c) (princ x))
;ABC
;NIL

(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	       (mapc #'(lambda ,parms ,@body)
		     ,@(map0-n #'(lambda (n)
				    `(nthcdr ,n ,src))
				(1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
		    (let ((len (length parms)))
		      `(let ((,src ,source))
			 (when (nthcdr ,(1- len) ,src)
			   (labels ((,bodfn ,parms ,@body))
			     (do ((,rest ,src (cdr ,rest)))
				 ((not (nthcdr ,(1- len) ,rest))
				  ,@(mapcar #'(lambda (args)
						 `(,bodfn ,@args))
					     (dt-args len rest src))
				  nil)
			       (,bodfn ,@(map1-n #'(lambda (n)
						      `(nth ,(1- n)
							    ,rest))
						  len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
	      (map1-n #'(lambda (n)
			  (let ((x (+ m n)))
			    (if (>= x len)
				`(nth ,(- x len) ,src)
			      `(nth ,(1- x) ,rest))))
		      len))
	  (- len 2)))


(do-tuples/c (x y z) '(a b c d)
	     (princ (list x y z)))

(let ((#:g2 '(a b c d)))
  (when (nthcdr 2 #:g2)
    (labels ((#:g4 (x y z)
		   (princ (list x y z))))
      (do ((#:g3 #:g2 (cdr #:g3)))
	  ((not (nthcdr 2 #:g3))
	   (#:g4 (nth 0 #:g3)
		 (nth 1 #:g3)
		 (nth 0 #:g2))
	   (#:g4 (nth 1 #:g3)
		 (nth 0 #:g2)
		 (nth 1 #:g2))
	   nil)
	(#:g4 (nth 0 #:g3)
	      (nth 1 #:g3)
	      (nth 2 #:g3))))))

(do-tuples/c (x y z) '(a b c d)
(princ (list x y z)))
;(A B C)(B C D)(C D A)(D A B)
;NIL
(do-tuples/c (w x y z) '(a b c d)
(princ (list w x y z)))
;(A B C D)(B C D A)(C D A B)(D A B C)
;NIL

;11.5 Iteration with Multiple Values


;; mvdo*, mvdo-gen, mvdo-rebind-gen 매크로를 eval 하고 실행할 수 있다.
(mvdo* ((x 1 (1+ x))
((y z) (values 0 0) (values z x)))
((> x 5) (list x y z))
(princ (list x y z)))
;(1 0 0)(2 0 2)(3 2 3)(4 3 4)(5 4 5)
;(6 5 6)

(mvdo ((x 1 (1+ x))
((y z) (values 0 0) (values z x)))
((> x 5) (list x y z))
(princ (list x y z)))
;(1 0 0)(2 0 1)(3 1 2)(4 2 3)(5 3 4)
;(6 4 5)


(let ((w 0) (x 1) (y 2) (z 3))
(mvpsetq (w x) (values 'a 'b) (y z) (values w x))
(list w x y z))
;(A B 0 1)

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
	`(prog nil
	       ,label
	       (if ,(car test)
		   (return (progn ,@(cdr test))))
	       ,@body
	       ,@(mvdo-rebind-gen rebinds)
	       (go ,label)))
    (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
      (let ((var/s (caar binds)) (expr (cadar binds)))
	(if (atom var/s)
	    `(let ((,var/s ,expr)) ,rec)
	  `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
	((< (length (car rebinds)) 3)
	 (mvdo-rebind-gen (cdr rebinds)))
	(t
	 (cons (list (if (atom (caar rebinds))
			 'setq
		       'multiple-value-setq)
		     (caar rebinds)
		     (third (car rebinds)))
	       (mvdo-rebind-gen (cdr rebinds))))))

(mvdo* (((px py) (pos player) (move player mx my))
	((x1 y1) (pos obj1) (move obj1 (- px x1)
				  (- py y1)))
	((x2 y2) (pos obj2) (move obj2 (- px x2)
				  (- py y2)))
	((mx my) (mouse-vector) (mouse-vector))
	(win nil (touch obj1 obj2))
	(lose nil (and (touch obj1 player)
		       (touch obj2 player))))
       ((or win lose) (if win 'win 'lose))
       (clear)
       (draw obj1)
       (draw obj2)
       (draw player))

;;아래의 suffle 매크로를 eval후 실행 가능하다.

(shuffle '(a b c) '(1 2 3 4))
;(A 1 B 2 C 3 4)

(mappend #'mklist '((a b c) d (e (f g) h) ((i)) j))
;(A B C D E (F G) H (I) J)


(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
	 (syms (mapcar #'(lambda (p)
			   (mapcar #'(lambda (x) (gensym))
				   (mklist (car p))))
		       pairs)))
    (labels ((rec (ps ss)
		  (if (null ps)
		      `(setq
			,@(mapcan #'(lambda (p s)
				       (shuffle (mklist (car p))
						s))
				   pairs syms))
		    (let ((body (rec (cdr ps) (cdr ss))))
		      (let ((var/s (caar ps))
			    (expr (cadar ps)))
			(if (consp var/s)
			    `(multiple-value-bind ,(car ss)
				 ,expr
			       ,body)
			  `(let ((,@(car ss) ,expr))
			     ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
	((null y) x)
	(t (list* (car x) (car y)
		  (shuffle (cdr x) (cdr y))))))



;;11.6 Need for Macros

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
	(temps (mapcar #'(lambda (b)
			   (if (listp (car b))
			       (mapcar #'(lambda (x)
					   (gensym))
				       (car b))
			     (gensym)))
		       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
			       (list var (cadr b)))
			   binds
			   temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	     ,label
	     (if ,test
		 (return (progn ,@result)))
	     ,@body
	     (mvpsetq ,@(mapcan #'(lambda (b)
				     (if (third b)
					 (list (car b)
					       (third b))))
				 binds))
	     (go ,label)))))

(defun fnif (test then &optional else)
  (if test
      (funcall then)
    (if else (funcall else))))

(if (rich) (go-sailing) (rob-bank))

(fnif (rich)
      #'(lambda () (go-sailing))
      #'(lambda () (rob-bank)))

(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
      ((> x 5) (list x y z))
      (princ (list x y z)))

(let (#:g2 #:g3 #:g4)
  (mvpsetq #:g2 1
	   (#:g3 #:g4) (values 0 0))
  (prog ((x #:g2) (y #:g3) (z #:g4))
	#:g1
	(if (> x 5)
	    (return (progn (list x y z))))
	(princ (list x y z))
	(mvpsetq x (1+ x)
		 (y z) (values z x))
	(go #:g1)))

(dolist (b bananas)
  (peel b)
  (eat b))

(mapc #'(lambda (b)
	  (peel b)
	  (eat b))
      bananas)

(defun forever (fn)
  (do ()
      (nil)
    (funcall fn)))
