;9. Variable Capture

;변수를 호출하는 것에 대한 내용이다. 결국은 gen-sym을 사용하라는 결론
;2012년 3월 3일 토요일 10시 신촌토즈본점 5.5에서 스터디했음 by netpyoung

;9.1 Macro Argument Capture

(defmacro for ((var start stop) &body body) ; wrong
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

> (for (x 1 5)
(princ x))
12345
NIL


(for (limit 1 5)
     (princ limit))

(do ((limit 1 (1+ limit))
     (limit 5))
    ((> limit limit))
  (princ limit))

> (let ((limit 5))
(for (i 1 10)
(when (> i limit)
(princ i))))
NIL

;9.2 Free Symbol Capture

(defvar w nil)

(defmacro gripe (warning)		; wrong
  `(progn (setq w (nconc w (list ,warning)))
	  nil))

(defun sample-ratio (v w)
  (let ((vn (length v)) (wn (length w)))
    (if (or (< vn 2) (< wn 2))
	(gripe "sample < 2")
      (/ vn wn))))

(defun sample-ratio (v w)
  (let ((vn (length v)) (wn (length w)))
    (if (or (< vn 2) (< wn 2))
	(progn (setq w (nconc w (list "sample < 2")))
	       nil)
      (/ vn wn))))


> (let ((lst '(b)))
(sample-ratio nil lst)
lst)
(B "sample < 2")
> w
NIL

;;The 'nconc' function destructively concatenates a sequence of lists and returns the result of this concatentation. The destructive aspect of this operation means that the actual symbol values are used in the list-modifying operations, not copies

;9.3 When Capture Occurs

(let ((x y) (z 10))
  (list w x z))


(let ((x x))
  x)

(defmacro foo (x y)
  `(/ (+ ,x 1) ,y))


 	
(foo (- 5 2) 6)



 	
(/ (+ (- 5 2) 1) 6)


 	
(/ (+ 1) )

(defmacro cap1 ()
  '(+ x 1))


 	
(defmacro cap2 (var)
  `(let ((x ...)
	 (,var ...))
     ...))


 	
(defmacro cap3 (var)
  `(let ((x ...))
     (let ((,var ...))
       ...)))

(defmacro cap4 (var)
  `(let ((,var ...))
     (let ((x ...))
       ...)))


 	
(defmacro safe1 (var)
  `(progn (let ((x 1))
	    (print x))
	  (let ((,var 1))
	    (print ,var))))


 	
(defmacro cap5 (&body body)
  `(let ((x ...))
     ,@body))


 	
(defmacro safe2 (expr)
  `(let ((x ,expr))
     (cons x 1)))


 	
(defmacro safe3 (var &body body)
  `(let ((,var ...))
     ,@body))



 	
(defmacro for ((var start stop) &body body) ; wrong
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))


 	
(for (limit 1 5)
     (princ limit))



 	
(let ((limit 0))
  (for (x 1 10)
       (incf limit x))
  limit)

 	
(do ((x 1 (1+ x))
     (limit 10))
    ((> x limit))
  (incf limit x))



 	
(let ((x 1)) (list x))


 	
(defmacro pathological (&body body)	; wrong
  (let* ((syms (remove-if (complement #'symbolp)
			  (flatten body)))
	 (var (nth (random (length syms))
		   syms)))
    `(let ((,var 99))
       ,@body)))


(defmacro before (x y seq)
  `(let ((seq ,seq))
     (< (position ,x seq)
	(position ,y seq))))


 	
(defmacro before (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
	(position yval seq))))

;9.4 Avoiding Capture with Better Names

;9.5 Avoiding Capture by Prior Evaluation

> (before (progn (setq seq '(b a)) 'a)
'b
'(a b))
NIL

(let ((seq '(a b)))
  (< (position (progn (setq seq '(b a)) 'a)
	       seq)
     (position 'b seq)))


(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))


 	
(defmacro for ((var start stop) &body body)
  `(do ((b #'(lambda (,var) ,@body))
	(count ,start (1+ count))
	(limit ,stop))
       ((> count limit))
     (funcall b count)))

;9.6 Avoiding Capture with Gensyms

(defmacro for ((var start stop) &body body) ; wrong
  `(do ((,var ,start (1+ ,var))
	(xsf2jsh ,stop))
       ((> ,var xsf2jsh))
     ,@body))

(eq (gensym) ...

> (gensym)
#:G47

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

 	
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))



> (setq x (gensym))
#:G48
> (setq *gensym-counter* 48 y (gensym))
#:G48
> (eq x y)
NIL

;;9.7 Avoiding Capture with Packages

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

;;9.8 Capture in Other Name-Spaces

> (defun fn (x) (+ x 1))
FN
> (defmacro mac (x) `(fn ,x))
MAC
> (mac 10)
11
> (labels ((fn (y) (- y 1)))
(mac 10))
9

> (block nil
(list 'a
(do ((x 1 (1+ x)))
(nil)
(if (> x 5)
(return-from nil x)
(princ x)))))
12345
(A 6)

;9.9 Why Bother?

(before (progn (setq seq '(b a)) 'a)
	'b
	'(a b))