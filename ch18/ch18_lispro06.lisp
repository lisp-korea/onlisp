;18. Destructuring

;Destructuring is a generalization of assignment.
;The operators setq and setf do assignments to individual variables. Destructuring combines assignment with access

;18.1 Destructuring on Lists

(let ((x (first lst))
      (y (second lst))
      (z (third lst)))
  ...)


(destructuring-bind (x y z) lst
  ...)

(destructuring-bind ((first last) (month day year) . notes)
    birthday
  ...)

;18.2 Other Structures

(dbind (a b c) #(1 2 3)
(list a b c))
;(1 2 3)

(dbind (a (b c) d) '( 1 #(2 3) 4)
(list a b c d))
;(1 2 3 4)
(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
(list a b c d))
;(1 #\f "ribble" (2 3 4))
;문자열 처리를 할 수 있는 기능도 담고 있는 듯.
;아래의 매크로를 정의하면 별 문제 없이 평가 된다.

(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

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
	      (cons (cons `(,var (elt ,seq ,n))
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

(destruc '(a b c) 'seq #'atom)
;((A (ELT SEQ 0)) (B (ELT SEQ 1)) (C (ELT SEQ 2)))

(destruc '(a (b . c) &rest d) 'seq)
;((A (ELT SEQ 0))
;((#:G2 (ELT SEQ 1)) (B (ELT #:G2 0)) (C (SUBSEQ #:G2 1)))
;(D (SUBSEQ SEQ 2)))

(dbind-ex (destruc '(a (b . c) &rest d) 'seq) '(body))
;(LET ((A (ELT SEQ 0))
;(#:G4 (ELT SEQ 1))
;(D (SUBSEQ SEQ 2)))
;(LET ((B (ELT #:G4 0))
;(C (SUBSEQ #:G4 1)))
;(PROGN BODY)))

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

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
			 `(,f (,(symb name f) ,gs)))
		     fields)
	 ,@body))))

(setq ar (make-array '(3 3)))
;#2A((0 0 0) (0 0 0) (0 0 0))
;#<Simple-Array T (3 3) C2D39E>
(for (r 0 2)
(for (c 0 2)
(setf (aref ar r c) (+ (* r 10) c))))
;NIL
(with-matrix ((a b c)
(d e f)
(g h i)) ar
(list a b c d e f g h i))
;(0 1 2 10 11 12 20 21 22)

(defstruct visitor name title firm)
;VISITOR
;테오데베르트라는 왕이 있다네...
(setq theo (make-visitor :name "Theodebert"
:title 'king
:firm 'franks))
;#S(VISITOR NAME "Theodebert" TITLE KING FIRM FRANKS)
(with-struct (visitor- name firm title) theo
(list name firm title))
;("Theodebert" FRANKS KING)

;18.3 Reference

(with-slots (species age height) my-tree
	    ...)

(setq height 72)

(with-places (a b c) #(1 2 3)
(list a b c))
;(1 2 3)
;위의 with-struct나 with-array와 비교되는데, 별 문제 없이 잘 평가된다.

(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
    `(symbol-macrolet ,(mapcar #'(lambda (b)
				   (if (consp (car b))
				       (car b)
				     b))
			       binds)
       ,(wplac-ex (mapcan #'(lambda (b)
			      (if (consp (car b))
				  (cdr b)))
			  binds)
		  body))))

(let ((lst '(1 (2 3) 4)))
(with-places (a (b . c) d) lst
(setf a 'uno)
(setf c '(tre)))
lst)



;(UNO (2 TRE) 4)

;이탈리아 숫자라네요.
;; 1 uno 우노 
;; 2 due 두에
;; 3 tre 뜨레
;; 4 quatro 꽈뜨로
;; 5 cinque 친꾸에
;; 6 sei 세이
;; 7 sette 쎄떼
;; 8 otto 오또
;; 9 nove 노베
;; 10 dieci 디에치
;; 11 undici 운디치
;; 12 dodici 도디치
;; 13 tredici 뜨레디치
;; 14 quatordici 꽈또르디치
;; 15 quindici 뀐디치
;; 16 sedici 쎄디치
;; 17 diciasette 디챠쎄떼
;; 18 diciotto 디쵸또 
;; 19 diciannove 디챠노베
;; 20 venti 벤띠
;; 21 ventuno 벤뚜노
;; 22 ventidue벤띠두에 (이런식으로 조합하면되요)
;; 30 trenta 뜨렌따
;; 40 quaranta 꽈란따
;; 50 cinquanta 친�따
;; 60 sessanta 쎄싼다
;; 70 settanta 쎄딴따
;; 80 ottanta 오딴따
;; 90 novanta 노반따
;; 100 cento 첸또


(progn
  (defparameter lst '(1 (2 3) 4))
  (with-places (a (b . c) d) lst
    (setf a 'uno)
    (setf c '(tre))
    (format t "~%a b c d : ~s~%" (list a b c d)))
  (print lst))
; (UNO (2 TRE) 4)

(progn
  (defparameter lst '(1 (2 3) 4))
  (dbind (a (b . c) d) lst
    (setf a 'lol)
    (format t "~%a b c d : ~s~%" (list a b c d)))
  (print lst))

;(1 (2 3) 4)


(p ?x ?y c ?x)

;(p a b c a)

(p ?x b ?y a)

(p ?y b c a)

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

(match '(p a b c a) '(p ?x ?y c ?x))
((?Y . B) (?X . A))
;T
(match '(p ?x b ?y a) '(p ?y b c a))
((?Y . C) (?X . ?Y))
;T
(match '(a b c) '(a a a))
;NIL
;NIL

(match '(p ?x) '(p ?x))
;NIL
;T

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
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(match '(a ?x b) '(_ 1 _))
((?X . 1))
;T

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
	    (values ?x ?y)
	    nil))

(abab '(hi ho hi ho))
;HI
;HO

(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
		 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
    (with-gensyms (gseq gelse)
		  `(labels ((,gelse () ,else))
		     ,(gen-match (cons (list gseq seq)
				       (destruc pat gseq #'simple?))
				 then
				 `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
    (let ((then (gen-match (cdr refs) then else)))
      (if (simple? (caar refs))
	  (match1 refs then else)
	(gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
	 (cond ((gensym? pat)
		`(let ((,pat ,expr))
		   (if (and (typep ,pat 'sequence)
			    ,(length-test pat rest))
		       ,then
		     ,else)))
	       ((eq pat '_) then)
	       ((var? pat)
		(let ((ge (gensym)))
		  `(let ((,ge ,expr))
		     (if (or (gensym? ,pat) (equal ,pat ,ge))
			 (let ((,pat ,ge)) ,then)
		       ,else))))
	       (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
	`(= (length ,pat) ,(length rest))
      `(> (length ,pat) ,(- (length rest) 2)))))

(if-match (?x 'a) seq
	  (print ?x)
	  nil)

(destruc '(?x 'a) 'g #'simple?)

((?x (elt g 0)) ((quote a) (elt g 1)))

((g seq) (?x (elt g 0)) ((quote a) (elt g 1)))


(match1 '(((quote a) (elt g 1))) '(print ?x) '#else function#)

(if (equal (quote a) (elt g 1))
    (print ?x)
  #else function#)

(let ((n 3))
(if-match (?x n 'n '(a b)) '(1 3 n (a b))
?x))
;1

(if-match (?x 'a) seq
	  (print ?x))

(let ((?x '#:g1))
  (labels ((#:g3 nil nil))
    (let ((#:g2 seq))
      (if (and (typep #:g2 'sequence)
	       (= (length #:g2) 2))
	  (let ((#:g5 (elt #:g2 0)))
	    (if (or (gensym? x) (equal ?x #:g5))
		(let ((?x #:g5))
		  (if (equal 'a (elt #:g2 1))
		      (print ?x)
		    (#:g3)))
	      (#:g3)))
	(#:g3)))))

(abab "abab")
;#\a
;#\b
(abab #(1 2 1 2))
;1
;2

(if-match (?x (1 . ?y) . ?x) '((a b) #(1 2 3) a b)
(values ?x ?y))
(A B)
;#(2 3)