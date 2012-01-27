;;;
;;;

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(let ((y 5))
  (scope-test 3))

;; dynamic scoping
;; (3 5)


;; lexical scoping
;; (3 7)

;; 2.6 Closure
(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

(list+ '(1 2 3) 10)

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
               add10 (make-adder 10))

(funcall add2 5)
(funcall add10 3)

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))
(setq addx (make-adderb 1))

(setq cities (make-dbms '((boston . us) (paris . france))))

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car)))))


;; 2.7 Local Functions
(mapcar #'(lambda (x) (+ 2 x))
        '(2 5 7 3))

(mapcar #'copy-tree '((a b) (c d e)))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

(defun my-list+ (lst n)
  (defun add-internal (x)
    (+ x n))
  (mapcar #'add-internal lst))
;; Why doesn't it work?

;;*** lambda는 재귀함수를 정의하는데 사용할 수 없다. 
;;*** mapcar 에 local binding과 recursive가 가능한 함수를 넘겨줄 수 있을까?

(labels ((inc (x) (1+ x)))
  (inc 3))
;; ==> 4

(let ((x 10) (y x))
  y)
;; == Unbound variable x

(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

;; 2.8 Tail Recursion
(defun our-length (lst)
  (declare (NOTINLINE OUR-LENGTH))
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(our-length '((a b) (a b c) (a b c d) (a b c d e)))
;; ==> No tail-recursion

(defun our-find-if (fn lst)
  (declare (NOTINLINE OUR-FIND-IF))
  (if (funcall fn (car lst))
      (car lst)
      (our-find-if fn (cdr lst))))
;; ==> tail-recursion because the value of the recursive call is immediately returned.

(our-find-if #'evenp '(1 3 5 8 11 12))
(our-find-if #'(lambda (X) (not (null X))) '(nil nil (1 2 3) (4 5)))


(defun fact (n)
  (declare (NOTINLINE FACT))
  (if (eq n 1)
      1
      (* n (fact (- n 1)))))

;;*** 많은 CL 컴파일러가 꼬리재귀를 loop 로 변경할 수 있다.
;;*** 꼬리재귀가 아닌 함수는 accumulator를 사용하는 로컬 함수를 포함하는 방법으로 꼬리재귀로 변경할수 있다.

(defun fact_tail_rec(n acc)
  (declare (NOTINLINE FACT_TAIL_REC))
  (if (eq n 1)
      acc
      (fact_tail_rec (- n 1) (* acc n))))

(defun fact_tail(n)
  (declare (NOTINLINE FACT_TAIL))
  (fact_tail_rec n 1))

(defun our-length-tail (lst)
  (declare (NOTINLINE OUR-LENGTH-TAIL))
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(our-length '((a b) (a b c) (a b c d) (a b c d e)))
(our-length-tail '((a b) (a b c) (a b c d) (a b c d e)))

(proclaim '(optimize speed)) ;; ensure that the compile


(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

;; write a function in whatever way seems most natural, and the transform it into a tail-recursive


;; 2.9 Compilation
;;*** compiled-function-p
;;*** compile
;;*** inline
(defun foo(x) (1+ x))

;; 2.10 Function from Lists
;;*** Lisp programs can write Lisp programs

;; 3. Functional Programming
;; 3.1 Functional Design
;;*** side-effect를 만들지 않고 값을 리턴하는 방법으로 동작하는 프로그램 만들기
;;*** side-effects
;;**** descructive change to objects(by rplaca)
;;**** assignments to variables
;;*** side-effect가 적을수록 프로그램은 읽기 쉽고 테스트가 쉽고 디버깅이 쉽다.

(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(setq lst '(a b c))
(bad-reverse lst)
;; lst
;; It shows the Common Lisp idiom for swapping two values


(setq lst '(a b c))
(good-reverse lst)

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))
;; good-reverse is more efficient O(n) instead of O(n^2)

(reverse lst)
(setq lst (reverse lst))

;; bad-reverse doesn't cons. it operates on the original list

(nreverse lst) ;; O(n) destructive revsersing function

;; 함수가 destructive 하더라도 side-effect를 만든다는 의미는 아니다.
;; nconc : is like append but destroys all arguments but the last.
(setq x '(a b c)) 
(setq y '(d e f)) 
(nconc x y)

(setq x (nconc x y))

;; functional programming은 side-effect를 절대 만들지 말라는게 아니라 필요이상으로 쓰지 말라는 것이다.

;; 많은 다른언어들이 두개이상의 값을 리턴하기 위해 side-effect를 만들어 낸다.

(truncate 26.21875)

(= (truncate 26.21875) 26)

(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))

(defun powers(x)
  (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (powers 4)
  (list base root square))

;; functional programming은 일반적으로 좋은 아이디어다. 특히나 lisp에서는 더욱 그러한데 lisp은 function programming을 지원하기 위해 진화해왔다.

;; 3.2 Imperative Outside-in
;; function program은 원하는 바를 알려주고 imperative program은 하려는 바를 알려준다.o

(defun fun(x)
  (list 'a (expt (car x) 2)))

(defun imp(x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

;; 왜 컴파일러가 당신에게 해주는 일은 직접 하려 하는가?
;; lisp을 배우는데 방해가 되는 것중 하나는 functional style로 프로그래밍을 하는것을 배우는 일이다.

;; sqr->(expt y 2)
(list 'a (expt y 2))
(list 'a (expt (car x 2))

;; 4.3 Functioal Interfaces
;; 누구도 소유하지 않는 것을 수정하는 것은 해가 되지 않는다.
(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))

;; Lisp convention
;; invovation은 리턴값으로 얻어진 객체를 소유하지만 인자로 넘어온 객체는 소유하지 않는다.


(defun ok (x)
  (nconc (list 'a x) (list 'c)))

(defun not-ok (x)
  (nconc (list 'a) x (list 'c)))

;; global variable을 사용하는데 있어 또하나의 문제는 program의 locality를 떨어드린다는 것이다.

(defun anything (x)
  (+ x *anything*))

(defun f (x)
  (let ((val (g x)))
    ))

;; avoid writing function whose return values incorporate quoted objects
(defun exclaim (expression)
  (append expression '(oh my)))

(exclaim '(lions and tigers and bears))

(nconc * '(goodness))

(exclaim '(fixnums and bignums and floats))

(defun exclaim (expression)
  (append expression (list 'oh 'my)))


;; 3.4 Interactive Programming
;; functional style은 순전히 심미적인 이유에서 선택하는것이 아니라 일을 쉽게 만들어주기 때문이다.

;; Experienced Lisp Programmer
;; 1. 소수의 함수에 side-effect를 격리시키고 프로그램의 많은 부분을 functional style로 만든다.
;; 2. 어느 함수가 side-effect를 만든다면 적어도 functional interface를 만드려고 한다.
;; 3. 각 함수에 하나의 잘 정의된 목적을 부여한다.

;; functional programming이란 같은 프로그램을 더 빠르게 작성하는게 아니라 다른 프로그램을 작성하는 일이다.


