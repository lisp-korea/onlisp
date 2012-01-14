
;; ch01
(setf a 1)
(setf b 10)

(do ((x a (+ 1 x)))
    ((> x b))
  (print x))

(for (x a b)
     (print x)

)

(defmacro for ((var from to) &body body)
  `(
    ,var
    ,from
    ,to
    ,@body)
  )


 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ch 2
;; Functions



;;===========================================================
;; 2.1 Function as Data


;;===========================================================
;; 2.2 Defining Functions
;; p10
(defun my-double (x) (* x 2))

(my-double 3)

#'my-double

(eq #'my-double (car (list #'my-double)))





(lambda (x) (* x 2))

#'(lambda (x) (* x 2))





(my-double 3)

((lambda (x) (* x 2)) 3)






(setq my-double 2)

(my-double my-double)







(symbol-value 'my-double)

(symbol-function 'my-double)





(setq x #'append)

(eq (symbol-value 'x) (symbol-function 'append))





(defun my-double (x) (* x 2))

(setf (symbol-function 'my-double)
      #'(lambda (x) (* x 2)))





;;===========================================================
;; 2.3 Functional Arguments

(+ 1 2)

(apply #'+ '(1 2))

(apply (symbol-function '+) '(1 2))

(apply #'(lambda (x y) (+ x y)) '(1 2))






(apply #'+ 1 '(2))

(funcall #'+ 1 2)






(mapcar #'(lambda (x) (+ x 10))
	'(1 2 3))


(defun app-double (fn arg)
  (print fn)
  (print arg)
  (fn arg))



(mapcar #'+ 
	'(1 2 3)
	'(10 100 1000))





(sort '(1 4 2 5 6 7 3) #'>)

(remove-if #'evenp '(1 2 3 4 5 6 7))






(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
	  (our-remove-if fn (cdr lst))
	  (cons (car lst) (our-remove-if fn (cdr lst))))))

(our-remove-if #'evenp '(1 2 3 4 5 6 7))





;;===========================================================
;; 2.4 Function as Properties

;; case-do & modify for new one
(defun behave (animal)
  (case animal
    (dog (wag-tail)
	 (bark))
    (rat (scurry)
	 (squeak))
    (cat (rub-legs)
	 (scratch-carpet))
    (human (speak))
    ))

(defun wag-tail () (print "wag-tail"))
(defun bark () (print "bark"))
(defun scurry () (print "scurry"))
(defun squeak () (print "squeak"))
(defun rub-legs () (print "rub-legs"))
(defun scratch-carpet () (print "scratch-carpet"))

(defun speak () (print "speak"))

(behave 'dog)
(behave 'rat)
(behave 'cat)
(behave 'human)





(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
	  (wag-tail)
	  (bark)))

(behave 'dog)





(setf (get 'all 'behavior)
      #'(lambda ()
	  (bark)
	  (scurry)
	  (scratch-carpet)))

(behave 'all)






;;===========================================================
;; 2.5 Scope

(let ((y 7))
  (defun scope-test (x)
    (declare (special y))
    (list x y)))

(let ((y 5))
  (scope-test 3))

(let ((y 6))
  (scope-test 3))


;; 취소하는 방법은?





;;===========================================================
;; 2.6 Closures

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
	  lst))

(list+ '(1 2 3) 10)






(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(new-id)

(reset-id)






(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
      add10 (make-adder 10))

;; class 선언, instance 생성
;; 비슷.

(funcall add2 5)

(funcall add10 3)





(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
	  (setq n x)
	  (+ x n))))

(setq addx (make-adderb  1))

(funcall addx 3)



(funcall addx 10 t)

(funcall addx 3)








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

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (car cities) 'boston)

(funcall (second cities) 'london 'england)

(funcall (car cities) 'london)



(defun lookup (key db)
  (funcall (car db) key))

(lookup 'paris cities)






;;===========================================================
;; 2.7 Local Functions

(labels ((inc (x) (1+ x)))
  (inc 3))

(let ((x 10)
      (y x))
  y)





(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
;                                     ^^^
		    (instances-in (cdr lst)))
;                    ^^^^^^^^^^^^
		 0)))
    (mapcar #'instances-in lsts)))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
;                ^^^




      
;;===========================================================
;; 2.8 Tail Recursion

(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))


(our-length '(a b c d))




(defun our-find-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
	  (car lst)
	  (our-find-if fn (cdr lst)))))

(our-find-if #'(lambda (x) (eq x 1))
	     '(2 1 4))

(our-find-if #'(lambda (x) (eq x 1))
	     '(2 3 4))





(defun our-length (lst)
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(our-length '(a b c d))


(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(our-length '(a b c d))






(proclaim '(optimize speed))






(defun triangle (n)
  (labels ((tri (c n)
	     (declare (type fixnum n c))
	     (if (zerop n)
		 c
		 (tri (the fixnum (+ n c))
		      (the fixnum (- n 1))))))
    (tri 0 n)))






;;===========================================================
;; 2.9 Compilation

(defun foo (x) (1+ x))

(compile 'foo)

(compiled-function-p #'foo)




(compile nil '(lambda (x) (+ x 2)))

(progn (compile 'bar '(lambda (x) (* x 3)))
       (compiled-function-p #'bar))




;; can't compile
(let ((y 2))
  (defun foo (x) (+ x y)))



(compiled-function-p (make-adder 2))

(compile 'make-adder)

(compiled-function-p (make-adder 2))




(defun 50th (lst) (nth 49 lst))

(proclaim '(inline 50th))

(defun foo (lst)
  (+ (50th lst) 1))
;;-->
(defun foo (lst)
  (+ (nth 49 lst) 1))




;;===========================================================
;; 2.10 Functions from Lists


(defmacro define-print-self (name args &body body)
  `(defun ,name ,args
     (print ',name)
     ,@body))


(define-print-self add-2 (x) (+ x 2))
;;->
;;(defun add-2 (x)
;;  (print 'add-2)
;;  (+ x 2))

(add-2 3)

(DEFUN ADD-2 (X) (PRINT 'ADD-2) (+ X 2))



(define-print-self add-3 (x) (+ x 3))

(add-3 2)




;;;;;;;;;;;;;;;;

(defun a ()
  (print "a"))

(defun b ()
  (print "b"))

(define-print-self c ()
  )
