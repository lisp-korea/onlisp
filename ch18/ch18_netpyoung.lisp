;; 18. Destructuring
;; "Destructurin"은 할당작업을 일반화 시킨 것.
;; Access, Assignment를 병행한다.


;; ---------------------------------------------------
;; 18.1 Destructuring on Lists
(defparameter lst '(aa bb cc))

(let ((x (first lst))
      (y (second lst))
      (z (third lst)))
  (list x y z))

(destructuring-bind (x y z) lst
  (list x y z))
;; ---------------------------------------------------
;; 18.2 Other Structures

;;(destruc '(a b c) '#(AA BB CC) #'atom)
;;((A (ELT #(AA BB CC) 0))
;; (B (ELT #(AA BB CC) 1))
;; (C (ELT #(AA BB CC) 2)))

;; (destruc '(a (b c) d) '(1 #(2 3) 4))
;; ((A (ELT (1 #(2 3) 4) 0))
;;  ((#:G1023 (ELT (1 #(2 3) 4) 1))
;;   (B (ELT #:G1023 0))
;;   (C (ELT #:G1023 1)))
;;  (D (ELT (1 #(2 3) 4) 2)))
(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  " 패턴을 살펴봐서, 런타임시 대응되는 위치에 있는 객체에 대해 변수를 각각 연결시켜준다:
 세번째 인자는 패턴내용에서 패턴 구조를 판별하는데 사용되는 predicate이다."
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
			((eq (car pat) '&rest) (cadr pat))
			((eq (car pat) '&body) (cadr pat))
			(t nil))))	; ***
	(if rest
	    `((,rest (subseq ,seq ,n)))
	    (let ((p (car pat))
		  (rec (destruc (cdr pat) seq atom? (1+ n)))) ; ***
	      (if (funcall atom? p)
		  (cons `(,p (elt ,seq ,n)) ; *** `(,'A (ELT #(AA BB CC) 0))
			rec)		    ; ***
		  (let ((var (gensym)))
		    (cons (cons `(, var (elt ,seq ,n))
				(destruc p var atom?))
			  rec))))))))

;; (dbind-ex '((A (ELT #(AA BB CC) 0))
;;             (B (ELT #(AA BB CC) 1))
;;             (C (ELT #(AA BB CC) 2)))
;; 	  '((list a b c)))
;; (LET ((A (ELT #(AA BB CC) 0))
;;       (B (ELT #(AA BB CC) 1))
;;       (C (ELT #(AA BB CC) 2)))
;;   (PROGN (LIST A B C)))

;; (dbind-ex
;;  '((A (ELT (1 #(2 3) 4) 0))
;;    ((#:G1023 (ELT (1 #(2 3) 4) 1)) (B (ELT #:G1023 0)) (C (ELT #:G1023 1)))
;;    (D (ELT (1 #(2 3) 4) 2)))
;;  '(list a b c d))
(defun dbind-ex (binds body)
  "|destruc|가 생성한 트리구조에 |let|을 덧씌운다."
  (if (null binds)
      `(progn ,@body)			;
      `(let ,(mapcar #'(lambda (b)	; *** (let binds ,@body)
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
  "destructuring-bind를 제네럴하게 작성한 것이다."
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

;; (destructuring-bind (a b c) #(1 2 3)
;;   (list a b c))
;;   #(1 2 3), not a LIST.
;;  [Condition of type SIMPLE-TYPE-ERROR]

(dbind (a b c) #(1 2 3)
       (list a b c))

(dbind (a (b c) d) '(1 #(2 3) 4)
  (list a b c d))

(dbind (a (b . c) &rest d) '(1 "hello" 2 3 4)
  (list a b c d))
;; ---------------------------------------------------

;; (defmacro with-matrix (pats ar &body body)
;;   (let ((gar (gensym)))
;;     `(let ((,gar ,ar))
;;        (let ,(let ((row -1))
;; 		  (mapcan #'(lambda (pat)
;; 			      (incf row)
;; 			      (setq col -1)
;; 			      (mapcar #'(lambda (p)
;; 					  `(,p (aref ,gar ,row ,(incf col))))
;; 				      pat))
;; 			  pats))
;; 	 ,@body))))

;; loop version
(defmacro with-matrix (pats ar &body body)
  "배열의 순서에 기반"
  (let ((arr (gensym)))
    `(let ((,arr ,ar))
       (let ,(loop for pat in pats
		for row from 0
		append (loop for p in pat
			  for col from 0
			  collect `(,p (aref ,arr ,row ,col))))
	 ,@body))))

(defmacro with-array (pat ar &body body)
  "배열의 좌표에 기반"
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
			 `(,(car p) (aref ,gar ,@(cdr p))))
		     pat)
	 ,@body))))

;; ---------------------------------------------------
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
;; (mkstr 'visitor- 'name)
;; "VISITOR-NAME"
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; (symb 'visitor- 'name)
;; VISITOR-NAME
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
;; 18.3 Reference

(defclass <Tree> ()
  ((species :initarg :species)
   (age :initarg :age)
   (height :initarg :height)))

(defparameter my-tree
  (make-instance '<Tree> :species "참나무" :age 30 :height 15))

(with-slots (species age height) my-tree
  (list species age height))
;; ---------------------------------------------------
(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b) ;let -> symbol-macrolet
				     (if (consp (car b)) (car b)
					 b))
				 binds)
	 ,(wplac-ex (mapcan #'(lambda (b)
				(if (consp (car b)) (cdr b)))
			    binds)
		    body))))

(defmacro with-places (pat seq &body body)
  "call-by-name"
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(with-places (a b c) #(1 2 3)
  (list a b c))

(progn
  (defparameter lst '(1 (2 3) 4))
  (with-places (a (b . c) d) lst
    (setf a 'uno)
    (setf c '(tre))
    (format t "~%a b c d : ~s~%" (list a b c d)))
  (print lst))

(progn
  (defparameter lst '(1 (2 3) 4))
  (dbind (a (b . c) d) lst
    (setf a 'lol)
    (format t "~%a b c d : ~s~%" (list a b c d)))
  (print lst))


;; ---------------------------------------------------
;; 18.4 Matching
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
   ((binding x binds) (match it y binds)) ;****
   ((binding y binds) (match x it binds)) ;****
   ((varsym? x)       (values (cons (cons x y) binds) t))
   ((varsym? y)       (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds)) ;****
    (match (cdr x) (cdr y) it))				    ;****
   (t                 (values nil nil))))

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

;; CL-USER> (binding 'a '((a 1) (b 2) (c)))
;; (1) (A 1)
;; CL-USER> (binding 'b '((a 1) (b 2) (c)))
;; (2) (B 2)
;; CL-USER> (binding 'c '((a 1) (b 2) (c)))
;; NIL (C)
;; CL-USER> (binding 'd '((a 1) (b 2) (c)))
;; NIL NIL


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
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
	    collect `(,n (gensym)))
     ,@body))

(defun simple? (x)
  (or (atom x) (eq (car x) 'quote)))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
	`(= (length ,pat) ,(length rest))
	`(> (length ,pat) ,(- (length rest) 2)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
    (cond ((gensym? pat)		;****
	   `(let ((,pat ,expr))
	      (if (and (typep ,pat 'sequence)
		       ,(length-test pat rest))
		  ,then
		  ,else)))
	  ((eq pat '_) then)
	  ((var? pat)			;****
	   (let ((ge (gensym)))
	     `(let ((,ge ,expr))
		(if (or (gensym? ,pat) (equal ,pat ,ge))
		    (let ((,pat ,ge)) ,then)
		    ,else))))
	  (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
	(if (simple? (caar refs))
	    (match1 refs then else)	;****
	    (gen-match (car refs) then else)))))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)	;***
      (with-gensyms (gseq gelse)
	`(labels ((,gelse () ,else))
	   ,(gen-match (cons (list gseq seq)		   ;***
			     (destruc pat gseq #'simple?)) ;***
		       then
		       `(,gelse))))))

(defmacro if-match2 (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defun abab2 (seq)
  (if-match2 (?x ?y ?x ?y) seq
	    (values ?x ?y)
	    nil))

(abab2 "abab")
(abab2 #(1 2 1 2))

;; variable
(let ((n 3))
  (if-match2 (?x n 'n) '(1 3 n)
	     ?x))

;; &body
(if-match2 (?x &body ?body) '(hello (world) (nice))
	   (list ?x ?body))

;; pat-match(simple) -> match1(-> var?)
(if-match2 ?x 1
	   ?x)

;; 패턴(pat)이 단순(simple?)하지 않을때, with-gensyms를 이용하여 젠매치(gen-match)를 시도함
;; gen-match에서 패턴이 gensym으로 bind되므로, gensym?을 판별하여 length-test를 돌릴 수 있다.
;; pat-match(->not simple -> gen-match) -> match1(-> gensym? -> var?)
(if-match2 (?x ?y) '(1 2)
	   (list ?x ?y))


(let ((a 10))
  (if-match2 (?x 'a) '(1 a)
	     (print ?x)
	     nil))

(destruc '(?x 'a) 'gensym #'simple?)
;; ((?X (ELT GENSYM 0)) ('A (ELT GENSYM 1)))
(cons '(gensym seq) '((?X (ELT GENSYM 0)) ('A (ELT GENSYM 1))))
;; ((gensym seq) . ((?X (ELT GENSYM 0)) ('A (ELT GENSYM 1))))
(caadar (last
	 '((gensym seq) . ((?X (ELT GENSYM 0)) ('A (ELT GENSYM 1))))))
;; elt
;; length-test하고, binding하고.
