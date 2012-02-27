;; 9.1 Macro Argument Capture

;; capture에 의해 발생할 수 있는 오류의 예
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

(do ((limit 1 (1+ limit))		; limit - 1개째
     (limit 5))				; limit - 2개째
    ((> limit limit))			; limit과 limit을 비교.
  (princ limit))
;; (LET ((LIMIT 1) (LIMIT 5))		; let안에 limit이 2개
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
  `(progn (setq w (nconc w (list , warning))) ; 외부에 선언된 w를 참조하게됨.
	  nil))
(defun sample-ratio (v w)		; 여기서 w는 sample-ratio의 인자이자, 외부에도 선언되어있음.
  (let ((vn (length v))
	(wn (length w)))		; w는 binding되지 않았음.
    (if (or (< vn 2) (< wn 2))
	(gripe "sample < 2")
	;; (PROGN (SETQ W (NCONC W (LIST "sample < 2"))) NIL) ; w는 binding되지 않아 free임.
	(/ vn wn))))

(let ((lst '(b)))
  (sample-ratio nil lst)
  lst)
;; (B "sample < 2")
w
;; nil

;; 9.3 When Capture Occurs
;; Free한 symbol: 표현식에서 변수로 사용되지만, 표현식내에 이에 대응하는 binding을 생성하지 않는 symbol.
;; Skeleton: 전체 expansion에서 macro call의 인자로 들어가 있는 부분을 뺀 것.
;; Capturable한 symbol:
;; 1. macro expansion의 skeleton에서 free이거나,
;; 2. macro에 넘겨진 인 skeleton의 일부에 bind된 symbol.

;; 1) free 예제
(let ((x y)		; free : y
      (z 10))
  ;; bind : x, z
  (list w x z))		; free : w

(let ((x x))		; free : x(second)
  x)
;; (let ((x second-x))
;;   x)

;; 2) skeleton 예제
(defmacro foo (x y)
  `(/ (+ ,x 1) ,y))
(foo (- 5 2) 6)
;; (/ (+ (- 5 2) 1) 6)
;; (/ (+         1)  )	; ,x ,y에 해당하는 값 or 표현식이 없음.
;; (defmacro foo (   )	; Skeleton
;;   `(/ (+   1)  ))	; x y ,x ,y 가 없음.


;; captuable :x
(defvar x nil)
(defmacro cap1 ()
  `(+ x 1))	     ; 이전 gripe macro와 유사한 버그를 일으킴
(defmacro cap1-1 (x)
  `(setq x (cap1)))
;; TODO

(defmacro cap2 (var)
  `(let ((x ...)
	 (,var ...))     ; macro call 표현식의 인자 또한 captuable
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

(for (limit 1 5)	; 9.1과 같은 예제
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
	 (var (nth (random (length syms)) ;symbol중 무작위로 하나 잡아서 써버림.
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
  `(progn (setq *warning* (nconc *warning* (list , warning))) ; 외부에 선언된 w를 참조하게됨.
	  nil))
(defun sample-ratio (v w)
  ;; 여기서 w는 sample-ratio의 인자이자, 외부에도 선언되어있음.
  (let ((vn (length v))
	(wn (length w)))	; w는 binding되지 않았음.
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
;; 9.3 예제 참고.

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
