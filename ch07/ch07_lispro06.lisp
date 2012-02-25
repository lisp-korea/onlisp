;7. Macros

;7.1 How Macros Work

> (defmacro nil! (var)
(list 'setq var nil))
NIL!
82

;builds the expression specified by the definition above, then
;evaluates that expression in place of the original macro call.

;7.2 Backquote

;`(a b c) is equal to '(a b c).

;`(a b c) is equal to (list 'a 'b 'c).

;`(a ,b c ,d) is equal to (list 'a b 'c d).

;backquote와 comma에 주의해야 한다.

> (setq a 1 b 2 c 3)
3
> `(a ,b c)
(A 2 C)
> `(a (,b c))
(A (2 C))

> `(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b)))
(A B 3 ('6) (+ A B) 'C '((1 2)))

,x `(a ,,b c) `(a ,(b ,c) d) `(,,`a)

(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nil! (var)
  `(setq ,var nil))

> (mapcar #'(lambda (x)
(nif x 'p 'z 'n))
'(0 2.5 -8))
(Z P N)

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro nif (expr pos zero neg)
  (list 'case
	(list 'truncate (list 'signum expr))
	(list 1 pos)
	(list 0 zero)
	(list -1 neg)))

(case (truncate (signum x))
  (1 'p)
  (0 'z)
  (-1 'n))

> (setq b '(1 2 3))
(1 2 3)
> `(a ,b c)
(A (1 2 3) C)
> `(a ,@b c)
(A 1 2 3 C)

(when (eligible obj)
  (do-this)
  (do-that)
  obj)

(defmacro our-when (test &body body)
  `(if ,test
       (progn
	 ,@body)))

(if (eligible obj)
    (progn (do-this)
	   (do-that)
	   obj))

(defun greet (name)
  `(hello ,name))

;;7.3 Defining Simple Macros


(member x choices :test #'eq)

call: (memq x choices)
expansion: (member x choices :test #'eq)

(defmacro memq (obj lst)

(defmacro memq (obj lst)
 `(member

(defmacro memq (obj lst)
  `(member ,obj

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(while hungry
  (stare-intently)
  (meow)
  (rub-against-legs))

(do ()
    ((not hungry))
  (stare-intently)
  (meow)
  (rub-against-legs))


(defmacro while (test &body body)


(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;7.4 Testing Macroexpansion

(pprint (macroexpand-1 '(or x y)))

(mac (or x y))

> (defmacro while (test &body body)
`(do ()
((not ,test))
,@body))
WHILE
> (pprint (macroexpand '(while (able) (laugh))))
(BLOCK NIL
(LET NIL
(TAGBODY
#:G61
(IF (NOT (ABLE)) (RETURN NIL))
(LAUGH)
(GO #:G61))))
T
> (pprint (macroexpand-1 '(while (able) (laugh))))
(DO NIL
((NOT (ABLE)))
(LAUGH))
T


(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))


> (setq exp (macroexpand-1 '(memq 'a '(a b c))))
(MEMBER (QUOTE A) (QUOTE (A B C)) :TEST (FUNCTION EQ))
> (eval exp)
(A B C)

;;7.5 Destructuring in Parameter Lists

(defun foo (x y z)
  (+ x y z))

(foo 1 2 3)

> (destructuring-bind (x (y) . z) '(a (b) c d)
(list x y z))
(A B (C D))

(dolist (x '(a b c))
  (print x))

(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
	   ,list)
     (let ((,var nil))
       ,result)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(when-bind (input (get-user-input))
	   (process input))

(let ((input (get-user-input)))
  (when input
    (process input)))

(defmacro our-expander (name) `(get ,name 'expander))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
	     #'(lambda (,g)
		 (block ,name
		   (destructuring-bind ,parms (cdr ,g)
		     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
    expr))

;;7.6 A Model of Macros

(let ((op 'setq))
  (defmacro our-setq (var val)
    (list op var val)))



;;2012-02-25
;;7.7 Macros as Programs


;; lexical binding 의 유/무로 매크로가 결정된다.

(do ((w 3)
     (x 1 (1+ x))
     (y 2 (1+ y))
     (z))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))

;should expand into something like

 	
(prog ((w 3) (x 1) (y 2) (z nil))
      foo
      (if (> x 10)
	  (return (progn (princ z) y)))
      (princ x)
      (princ y)
      (psetq x (1+ x) y (1+ y))
      (go foo))



> (let ((a 1))
(setq a 2
      b a)
(list a b))

;; a가 1인데, setq로 a가 2가 되고, b가 a이므로 최종 결과는 b = a = 2 가 됨


(2 2)

;Here, because a is set first, b gets its new value, 2. A psetq is supposed to behave as if its arguments were assigned in parallel:


;;psetq 는 parallel setq로 동시에 평가가 일어남

> (let ((a 1))
(psetq a 2
       b a)
(list a b))

;; a는 1이고, a는 2로 됨과 동시에 b는 a가 되므로, 처음 정의된 a의 값 1이 정의됨
;; b = a = 1
;; a = 1 => a = 2

;; verilog나  VHDL을 보는 듯 함.

(2 1)


(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
	   ,label
	   (if ,test
	       (return (progn ,@result)))
	   ,@body
	   (psetq ,@(make-stepforms bindforms))
	   (go ,label))))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (consp b)
		  (list (car b) (cadr b))
		(list b nil)))
	  bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
	      (if (and (consp b) (third b))
		  (list (car b) (third b))
		nil))
	  bindforms))


;;7.8 Macro Style


(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
	    (our-and ,@(cdr args))))))

(defmacro our-andb (&rest args)
  (if (null args)
      t
    (labels ((expander (rest)
		       (if (cdr rest)
			   `(if ,(car rest)
				,(expander (cdr rest)))
			 (car rest))))
      (expander args))))

;;7.9 Dependence on Macros


> (defmacro mac (x) `(1+ ,x))
MAC
> (setq fn (compile nil '(lambda (y) (mac y))))
#<Compiled-Function BF7E7E>
> (defmacro mac (x) `(+ ,x 100))
MAC
> (funcall fn 1)
2


;;7.10 Macros from Functions


;Have a body consisting of a single expression.
;Have a parameter list consisting only of parameter names.
;Create no new variables (except the parameters).
;Are not recursive (nor part of a mutually recursive group).->10.4
;Have no parameter which occurs more than once in the body.->10.1
;Have no parameter whose value is used before that of another parameter occurring before it in the parameter list.->10.2
;Contain no free variables.
;-> free variable : setq로 정의되지 않은 variable
;dynamic variable로 볼 수 있으며, macro expression 내에 정의되지 않는 변수


(defun second (x) (cadr x))

(defmacro second (x) `(cadr ,x))

;;macro가 되면, backquote와 comma가 붙는다.

(defun noisy-second (x)
  (princ "Someone is taking a cadr!")
  (cadr x))

;could be duplicated by the following macro:

 	
(defmacro noisy-second (x)
  `(progn
     (princ "Someone is taking a cadr!")
     (cadr ,x)))

;두개 이상의 표현은 허용되지 않으므로 progn으로 묶여서 한 개의 표현으로 된다.

(defun sum (&rest args)
  (apply #'+ args))

;becomes

 	
(defmacro sum (&rest args)
  `(apply #'+ (list ,@args)))

;which in this case would be better rewritten:

 	
(defmacro sum (&rest args)
  `(+ ,@args))

(defun foo (x y z)
  (list x (let ((x y))
	    (list x z))))

;neither of the last two instances of x will refer to the parameter x. The second instance is not evaluated at all, and the third instance refers to a new variable established by the let. So only the first instance will get a comma:

 	
(defmacro foo (x y z)
  `(list ,x (let ((x ,y))
	      (list x ,z))))

;; x 두개는 파라미터 x를 참조하지만, 두 번째 x는 평가 되지 않기 때문에 comma가 안 붙는다. 세번째 x는 let에 의해 사용되는 지역 변수 이므로(새롭게 생기는) comma가 없다.

;;7.11 Symbol Macros

> (symbol-macrolet ((hi (progn (print "Howdy")
1)))
(+ hi 2))
"Howdy"
3