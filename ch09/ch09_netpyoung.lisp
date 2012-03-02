;; 9.1 Macro Argument Capture

;; capture�� ���� �߻��� �� �ִ� ������ ��
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

(for (x 1 5)
  (princ x))
;; (DO ((X 1 (1+ X))
;;      (LIMIT 5))
;;     ((> X LIMIT))
;;   (PRINC X))
;; 12345

(for (limit 1 5)
  (princ limit))
; caught ERROR:
;   The variable LIMIT occurs more than once in the LET.

(do ((limit 1 (1+ limit))		; limit - 1��°
     (limit 5))				; limit - 2��°
    ((> limit limit))			; limit�� limit�� ��.
  (princ limit))
;; (LET ((LIMIT 1) (LIMIT 5))		; let�ȿ� limit�� 2��
;; (TAGBODY
;;   (GO G855)
;;  G854
;;   (TAGBODY (PRINC LIMIT))
;;   (PSETQ LIMIT (1+ LIMIT))
;;  G855
;;   (UNLESS (> LIMIT LIMIT) (GO G854))
;;   (RETURN-FROM NIL (PROGN))))
; caught ERROR:
;   The variable LIMIT occurs more than once in the LET.

(let ((limit 5))
  (for (i 1 10)
    (when (> i limit)
      (princ i))))
; caught STYLE-WARNING:
;   The variable LIMIT is defined but never used.


;; 9.2 Free Symbol Capture
(defvar w nil)
(defmacro gripe (warning)
  `(progn (setq w (nconc w (list , warning))) ; �ܺο� ����� w�� �����ϰԵ�.
	  nil))
(defun sample-ratio (v w)		; ���⼭ w�� sample-ratio�� ��������, �ܺο��� ����Ǿ�����.
  (let ((vn (length v))
	(wn (length w)))		; w�� binding���� �ʾ���.
    (if (or (< vn 2) (< wn 2))
	(gripe "sample < 2")
	;; (PROGN (SETQ W (NCONC W (LIST "sample < 2"))) NIL) ; w�� binding���� �ʾ� free��.
	(/ vn wn))))

(let ((lst '(b)))
  (sample-ratio nil lst)
  lst)
;; (B "sample < 2")
w
;; nil

;; 9.3 When Capture Occurs
;; Free�� symbol: ǥ���Ŀ��� ������ ��������, ǥ���ĳ��� �̿� �����ϴ� binding�� �������� �ʴ� symbol.
;; Skeleton: ��ü expansion���� macro call�� ���ڷ� �� �ִ� �κ��� �� ��.
;; Capturable�� symbol:
;; 1. macro expansion�� skeleton���� free�̰ų�,
;; 2. macro�� �Ѱ��� �� skeleton�� �Ϻο� bind�� symbol.

;; 1) free ����
(let ((x y)		; free : y
      (z 10))
  ;; bind : x, z
  (list w x z))		; free : w

(let ((x x))		; free : x(second)
  x)
;; (let ((x second-x))
;;   x)

;; 2) skeleton ����
(defmacro foo (x y)
  `(/ (+ ,x 1) ,y))
(foo (- 5 2) 6)
;; (/ (+ (- 5 2) 1) 6)
;; (/ (+         1)  )	; ,x ,y�� �ش��ϴ� �� or ǥ������ ����.
;; (defmacro foo (   )	; Skeleton
;;   `(/ (+   1)  ))	; x y ,x ,y �� ����.


;; captuable :x
(defvar x nil)
(defmacro cap1 ()
  `(+ x 1))	     ; ���� gripe macro�� ������ ���׸� ����Ŵ
(defmacro cap1-1 (x)
  `(setq x (cap1)))
;; TODO

(defmacro cap2 (var)
  `(let ((x ...)
	 (,var ...))     ; macro call ǥ������ ���� ���� captuable
     ...))
(defmacro cap2-1 (var)
  `(let ((x 10)
	 (,var 20))
     (setq x 20)
     x))
(cap2-1 x)

(defmacro cap3 (var)
  `(let ((x ...))
     (let ((,var ...))
       ...)))
(defmacro cap3-1 (var)
  `(let ((x 10))
     (let ((,var 20))
       (setq ,var x)
       (list ,var x))))
(cap3-1 x)

(defmacro cap4 (var)
  `(let ((,var ...))
     (let ((x ...))
       ...)))
(defmacro cap4-1 (var)
  `(let ((,var 10))
     (let ((x 20))
       (list ,var x))))
(cap4-1 x)

(defmacro safe1 (var)
  `(progn
     (let ((x 1))
       (print x))
     (let ((,var 10))
       (print ,var))))
(safe1 x)

(defmacro cap5 (&body body)
  `(let ((x ...))
     ,@body))
(defmacro cap5-1 (&body body)
  `(let ((x 10))
     ,@body))
;; TODO


(defmacro safe2 (expr)
  `(let ((x ,expr))
     (cons x 1)))
(safe2 x)

(defmacro safe3 (var &body body)
  `(let ((,var ...))
     ,@body))
(defmacro safe3-1 (var &body body)
  `(let ((,var 10))
     ,@body))
;; TODO

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

(for (limit 1 5)	; 9.1�� ���� ����
  (princ limit))

(defun flatten (lis)
  "Removes nestings from a list."
  (cond ((atom lis) lis)
	((listp (car lis))
	 (append (flatten (car lis)) (flatten (cdr lis))))
	(t (append (list (car lis)) (flatten (cdr lis))))))

(defmacro pathological (&body body)
  (let* ((syms (remove-if (complement #'symbolp)
			  (flatten body)))
	 (var (nth (random (length syms)) ;symbol�� �������� �ϳ� ��Ƽ� �����.
		   syms)))
    `(let ((,var 99))
       ,@body)))

(pathological
  (print "hi")
  (print print))


;; vulnerable to capture
(defmacro before (x y seq)
  `(let ((seq ,seq))
     (< (position ,x seq)
	(position ,y seq))))
(before (progn (setq seq '(b a)) 'a)
	'b
	'(a b))
;; nil
;; correct version
(defmacro before (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
	(position yval seq))))
(before (progn (setq seq '(b a)) 'a)
	'b
	'(a b))
;; T
;; (LET ((SEQ '(A B)))
;;   (< (POSITION (PROGN (SETQ SEQ '(B A)) 'A)
;; 	       SEQ)
;;      (POSITION 'B SEQ)))

;; 9.4 Avoding Capture with Better Names
*package*
;;#<PACKAGE "COMMON-LISP-USER">

(defvar *warning* nil)
(defmacro gripe (warning)
  `(progn (setq *warning* (nconc *warning* (list , warning))) ; �ܺο� ����� w�� �����ϰԵ�.
	  nil))
(defun sample-ratio (v w)
  ;; ���⼭ w�� sample-ratio�� ��������, �ܺο��� ����Ǿ�����.
  (let ((vn (length v))
	(wn (length w)))	; w�� binding���� �ʾ���.
    (if (or (< vn 2) (< wn 2))
	(gripe "sample < 2")
	(/ vn wn))))
(sample-ratio '(1 2 3) '(4 5 6 7))
;; 3/4
(sample-ratio '(1 2 3) '(4))
;; nil
*warning*
;; ("sample < 2")

;; 9.5 Avoding Capture by Prior Evaluation
(before (progn (setq seq '(b a)) 'a)
	'b
	'(a b))
;; 9.3 ���� ����.

;; vulenable to capture
(defmacro for ((var start stop) &body body)
 `(do ((, var ,start (1+ ,var))
       (limit ,stop))
      ((> ,var limit))
    ,@body))
(for (limit 1 5)
  (princ limit))
;; error

;; correct
(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
	(count ,start (1+ count))
	(limit ,stop))
       ((> count limit))
     (funcall b count)))
(for (limit 1 5)
  (princ limit))

;; 9.6 Avodding Capture with Gensyms
(defmacro for ((var start stop) &body body)
 `(do ((, var ,start (1+ ,var))
       (blablabla ,stop))
      ((> ,var blablabla))
    ,@body))
(for (limit 1 5)
  (princ limit))
;; 12345
(for (blablabla 1 5)
  (princ blablabla))
;; error

(defmacro for-gensym ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((, var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))
(for-gensym (gstop 1 5)
  (princ gstop))
;; 12345
;; (DO ((GSTOP 1 (1+ GSTOP))
;;      (#:G977 5))
;;     ((> GSTOP #:G977))
;;   (PRINC GSTOP))


(gensym)
;; #:G47
(setq x (gensym))
;; #G:1006
(setq *gensym-counter* 1006
      y (gensym))
;; #G:1006
(eq x y)
;; nil

(setq y (setq x (gensym)))
x
;; #G:1007
y
;; #G:1007
(eq x y)
;; T

;; 9.7 Avoding Capture with Packages
(defpackage macros
  (:use :cl)
  (:export :for))
(in-package :macros)
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

(defpackage mycode
  (:use :cl :macros))
(in-package :mycode)
(for (limit 1 5)
  (princ limit))
;; (DO ((LIMIT 1 (1+ LIMIT))
;;      (MACROS::LIMIT 5))
;;     ((> LIMIT MACROS::LIMIT))
;;   (PRINC LIMIT))
;;  Capture in Other Name-Spaces

;; 9.8 Capture in Other Name-Spaces
(defun fn (x)
  (+ x 1))

(defmacro mac (x)
  `(fn ,x))

(mac 10)
;; 11
(labels ((fn (y) (1- y)))
  (mac 10))
;; 9

(block blabla
  (list 'a
	(do ((x 1 (1+ x)))		; block nil
	    (nil)
	  (if (> x 5)
	      (return-from nil x)
	      (princ x)))))
;; 12345
;; (A 6)

(block blabla
  (list 'a
	(do ((x 1 (1+ x)))		; block nil
	    (nil)
	  (if (> x 5)
	      (return-from blabla x)
	      (princ x)))))
;; 12345
;; 6

;; 9.9 Why Bother?
;; ...

;; 392p - 128 gentemp
(defpackage :tmp
  (:use :cl))
(in-package :tmp)
(gentemp)
;; T2
(find-symbol "T2")
;; T2
;; :INTERNAL
(defvar t2 10)
(in-package :cl-user)
(gentemp)
;; T3
(find-symbol "T2")
;; NIL
;; NIL
t2

(find-symbol "T2")
;; T2
;; :INTERNAL
tmp::t2
;; 10
