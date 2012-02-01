
;;2.1 Functions as Data

;;Lisp itself is a collection of functions
;;functions is that they are Lisp objects

;;2.2 Defining Functions
> (defun double (x) (* x 2))
DOUBLE
> (double 1)
2

> #'double
#<Interpreted-Function C66ACE>

> (eq #'double (car (list #'double)))
T

(lambda (x) (* x 2))

> #'(lambda (x) (* x 2))
#<Interpreted-Function C674CE>

> (double 3)
6

> ((lambda (x) (* x 2)) 3)
6

> (setq double 2)
2
> (double double)
4

> (symbol-value 'double)
2

> (symbol-function 'double)
#<Interpreted-Function C66ACE>

> (setq x #'append)
#<Compiled-Function 46B4BE>
> (eq (symbol-value 'x) (symbol-function 'append))
T

(defun double (x) (* x 2))

(setf (symbol-function 'double)
      #'(lambda (x) (* x 2)))

;;2.3 Functional Arguments

(+ 1 2)
(apply #'+ '(1 2))
(apply (symbol-function '+) '(1 2))
(apply #'(lambda (x y) (+ x y)) '(1 2))

(apply #'+ 1 '(2))

(funcall #'+ 1 2)

> (mapcar #'(lambda (x) (+ x 10))
'(1 2 3))
(11 12 13)
> (mapcar #'+
'(1 2 3)
'(10 100 1000))
(11 102 1003)

> (sort '(1 4 2 5 6 7 3) #'<)
(1 2 3 4 5 6 7)

> (remove-if #'evenp '(1 2 3 4 5 6 7))
(1 3 5 7)

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
    (if (funcall fn (car lst))
	(our-remove-if fn (cdr lst))
      (cons (car lst) (our-remove-if fn (cdr lst))))))

;;2.4 Functions as Properties

(defun behave (animal)
  (case animal
    (dog (wag-tail)
	 (bark))
    (rat (scurry)
	 (squeak))
    (cat (rub-legs)
	 (scratch-carpet))))

(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
	  (wag-tail)
	  (bark)))


;;2.5 Scope


;;; scope은 전역 지역을 나타내는 개념?

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

;; With dynamic scope 동적 scope이면, 할당 당시의 y 값을 받는다.
;; Lisp의 처음 구현에서 사용된 Dynamic Scope이 있습니다. 변수가 선언된 코드의 블럭 외부에서 변수가 호출될 수 있는 유효 범위를 dynamic scope라고 합니다. 이 방식으로 선언된 변수를 흔히 public 변수라고 합니다.
;; 출처 : http://nol2soft.com/2011/08/01/nested-scopes-lexicalstatic-and-dynamic-scopes/

(let ((y 5))
(scope-test 3))
;(3 5)

;; In a lexically scoped Lisp
;; Lexical Scope란 일반적으로 많은 프로그램 언어에서 변수가 선언된 코드의 블럭에서 호출될 수 있는 변수의 유효 범위를 의미합니다. 이 범위는 코드가 컴파일될때 결정됩니다. 흔히 이 방식으로 선언된 변수를 private 변수라고도 합니다.

(let ((y 5))
(scope-test 3))
;(3 7)

;; http://rookiecj.tistory.com/20 의 예제

(setq regular 5)
(defun check-regular () regular)
(check-regular)
(let ((regular 6)) (check-regular))

(defvar *regular* 5)
(defun check-special () *regular*)
(let ((*regular* 6)) (check-special))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
	  lst))

> (list+ '(1 2 3) 10)
(11 12 13)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

> (setq add2 (make-adder 2)
add10 (make-adder 10))
#<Interpreted-Function BF162E>
> (funcall add2 5)
7
> (funcall add10 3)
13

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
	  (setq n x)
	(+ x n))))

> (setq addx (make-adderb 1))
#<Interpreted-Function BF1C66>
> (funcall addx 3)
4

> (funcall addx 100 t)
100
> (funcall addx 3)
103

> (setq cities (make-dbms '((boston . us) (paris . france))))
(#<Interpreted-Function 8022E7>
#<Interpreted-Function 802317>
#<Interpreted-Function 802347>)

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

> (funcall (car cities) 'boston)
US
> (funcall (second cities) 'london 'england)
LONDON
> (funcall (car cities) 'london)
ENGLAND

(defun lookup (key db)
  (funcall (car db) key))

;; 2.7 Local Functions

> (mapcar #'(lambda (x) (+ 2 x))
'(2 5 7 3))
(4 7 9 5)

> (mapcar #'copy-tree '((a b) (c d e)))
((A B) (C D E))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
	  lst))

(#name# #parameters# . #body#)
;;(cons 1 2) looks like: (1 . 2) 표현식의 params와 body의 pair를 나타내는 예로 보여짐

#'(lambda #parameters# . #body#)


> (labels ((inc (x) (1+ x)))
(inc 3))
4


(let ((x 10) (y x))
  y)

(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
			 (if (consp lst)
			     (+ (if (eq (car lst) obj) 1 0)
				(instances-in (cdr lst)))
			   0)))
    (mapcar #'instances-in lsts)))

> (count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
(1 2 1 2)


;; 2.8 Tail-Recursion

(defun our-length (lst)
  (if (null lst)
      0
    (1+ (our-length (cdr lst)))))

(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
    (our-find-if fn (cdr lst))))

(defun our-length (lst)
  (labels ((rec (lst acc)
		(if (null lst)
		    acc
		  (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(proclaim '(optimize speed))

(defun triangle (n)
  (labels ((tri (c n)
		(declare (type fixnum n c))
		(if (zerop n)
		    c
		  (tri (the fixnum (+ n c))
		       (the fixnum (- n 1))))))
    (tri 0 n)))

;; 2.9 Compilation

> (defun foo (x) (1+ x))
FOO

> (compiled-function-p #'foo)
NIL

 	
> (compile 'foo)
FOO

 	
> (compiled-function-p #'foo)
T

 	
> (compile nil '(lambda (x) (+ x 2)))
#<Compiled-Function BF55BE>

 	
> (progn (compile 'bar '(lambda (x) (* x 3)))
(compiled-function-p #'bar))
T

 	
> (let ((y 2))
(defun foo (x) (+ x y)))

> (compile 'make-adder)
MAKE-ADDER
> (compiled-function-p (make-adder 2))
T

(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

 	
(defun foo (lst)
  (+ (50th lst) 1))

 	
(defun foo (lst)
  (+ (nth 49 lst) 1))


;;2.10 Functions from Lists

