;10. Other Macro Pitfalls 매크로 함정?

;This chapter discusses four more problems to avoid when defining macros.
;
;10.1 Number of Evaluations

> (let ((x 2))
(for (i 1 (incf x))
(princ i)))
;123

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;위 매크로를 먼저 eval하고 첫번째 예제를 실행해야 한다.

(let ((x 5))
(for (i 1 (incf x))
(princ i)))
;123456

(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

(defmacro for ((var start stop) &body body)
(let ((gstop (gensym)))
`(do ((,gstop ,stop)
(,var ,start (1+ ,var)))
((> ,var ,gstop))
,@body)))

;10.2 Order of Evaluation

(setq x 10)
;10
(+ (setq x 3) x)
;6

(let ((x 1))
(for (i x (setq x 13))
(princ i)))
;12345678910111213


;10.3 Non-functional Expanders

(defmacro nil! (x)			; wrong
  (incf *nil!s*) ;; 횟수가 증가하지 않음
  `(setf ,x nil))

;;이 매크로를 정의하고, OUR+ 예제를 실행할 것
(defmacro string-call (opstring &rest args) ; wrong
  `(,(intern opstring) ,@args))

(defun our+ (x y) (+ x y))
;OUR+
(string-call "OUR+" 2 3)
;5

(defun et-al (&rest args)
  (nconc args (list 'et 'al)))

(et-al 'smith 'jones)
;(SMITH JONES ET AL)

(setq greats '(leonardo michelangelo))
;(LEONARDO MICHELANGELO)
(apply #'et-al greats)
;(LEONARDO MICHELANGELO ET AL)
greats
;(LEONARDO MICHELANGELO ET AL) 가 아닌 ET AL이 없는 결과가 나온다.

(defmacro echo (&rest args)
  `',(nconc args (list 'amen)))


(defun foo () (echo x))

(foo)
;(X AMEN AMEN)
(foo)
;(X AMEN AMEN AMEN)

(defmacro echo (&rest args)
  `'(,@args amen))

(defmacro crazy (expr) (nconc expr (list t)))

(defun foo () (crazy (list)))

(foo)
;(T T) 잉? 이것도 T 밖에 안 나온다.

;;10.4 Recursion

(defun our-length (x)
  (if (null x)
      0
    (1+ (our-length (cdr x)))))

(defun our-length (x)
  (do ((len 0 (1+ len))
       (y x (cdr y)))
      ((null y) len)))

(if (= x 0)
    (car y)
  (nthb (- x 1) (cdr y)))

(if (= x 0)
    (car y)
  (if (= (- x 1) 0)
      (car (cdr y))
    (nthb (- (- x 1) 1) (cdr (cdr y)))))

(defun ntha (n lst)
  (if (= n 0)
      (car lst)
    (ntha (- n 1) (cdr lst))))

(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
     (nthb (- ,n 1) (cdr ,lst))))

(defmacro nthc (n lst)
  `(do ((n2 ,n (1- n2))
	(lst2 ,lst (cdr lst2)))
       ((= n2 0) (car lst2))))

(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defun nth-fn (n lst)
  (if (= n 0)
      (car lst)
    (nth-fn (- n 1) (cdr lst))))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
		    (if (= n 0)
			(car lst)
		      (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))


(defmacro ora (&rest args)
  (or-expand args))

(defun or-expand (args)
  (if (null args)
      nil
    (let ((sym (gensym)))
      `(let ((,sym ,(car args)))
	 (if ,sym
	     ,sym
	   ,(or-expand (cdr args)))))))

(defmacro orb (&rest args)
  (if (null args)
      nil
    (let ((sym (gensym)))
      `(let ((,sym ,(car args)))
	 (if ,sym
	     ,sym
	   (orb ,@(cdr args)))))))

(let ((g2 x))
  (if g2
      g2
    (let ((g3 y))
      (if g3 g3 nil))))

