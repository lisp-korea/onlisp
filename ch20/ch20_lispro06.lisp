#20. Continuations

#20.1 Scheme Continuations

(let ((f #'(lambda (x) (1+ x))))
  (funcall f 2))

(let ((f (lambda (x) (1+ x))))
  (f 2))

(defun foo (x) (1+ x))

(define foo (lambda (x) (1+ x)))

(define (foo x) (1+ x))

(/ (- x 1) 2)

(lambda (val) (/ val 2))

(define (f1 w)
  (let ((y (f2 w)))
    (if (integer? y) (list 'a y) 'b)))

(define (f2 x)
  (/ (- x 1) 2))

(lambda (val)
  (let ((y (/ val 2)))
    (if (integer? y) (list 'a y) 'b)))

(call-with-current-continuation
 (lambda (cc)
   ...))


> (define frozen)
FROZEN
> (append '(the call/cc returned)
(list (call-with-current-continuation
(lambda (cc)
(set! frozen cc)
'a))))
(THE CALL/CC RETURNED A)

> (frozen 'again)
(THE CALL/CC RETURNED AGAIN)

> (frozen 'thrice)
(THE CALL/CC RETURNED THRICE)

> (+ 1 (frozen 'safely))
(THE CALL/CC RETURNED SAFELY)


> (define froz1)
FROZ1
> (define froz2)
FROZ2
> (let ((x 0))
(call-with-current-continuation
(lambda (cc)
(set! froz1 cc)
(set! froz2 cc)))
(set! x (1+ x))
x)
1

> (froz2 ())
2
> (froz1 ())
3

(define t1 '(a (b (d h)) (c e (f i) g)))

(define t2 '(1 (2 (3 6 7) 4 5)))

> (dft t1)
ABDHCEFIG()

> (dft-node t1)
A

> (restart)
B

. . .
> (restart)
G
> (restart)
DONE

> (dft2 t1)
ABDHCEFIG()

(define (dft tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (write tree))
	(else (dft (car tree))
	      (dft (cdr tree)))))

(define *saved* ())

(define (dft-node tree)
  (cond ((null? tree) (restart))
	((not (pair? tree)) tree)
	(else (call-with-current-continuation
	       (lambda (cc)
		 (set! *saved*
		       (cons (lambda ()
			       (cc (dft-node (cdr tree))))
			     *saved*))
		 (dft-node (car tree)))))))

(define (restart)
  (if (null? *saved*)
      'done
    (let ((cont (car *saved*)))
      (set! *saved* (cdr *saved*))
      (cont))))

(define (dft2 tree)
  (set! *saved* ())
  (let ((node (dft-node tree)))
    (cond ((eq? node 'done) ())
	  (else (write node)
		(restart)))))

> (set! *saved* ())
()
> (let ((node1 (dft-node t1)))
(if (eq? node1 'done)
'done
(list node1 (dft-node t2))))
(A 1)
> (restart)
(A 2)
. . .
> (restart)
(B 1)
. . .


#20.2 Continuation-Passing Macros


(=defun add1 (x) (=values (1+ x)))

(progn (defmacro add1 (x)
	 `(=add1 *cont* ,x))
       (defun =add1 (*cont* x)
	 (=values (1+ x))))

(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

> (=values (1+ n))

(funcall *cont* (1+ n))

(funcall #'(lambda (*cont* n) (=values (1+ n))) *cont* 2)

(funcall #'identity (1+ n))

> (=defun bar (x)
(=values (list 'a (add1 x))))
BAR
> (bar 5)
(A 6)

> (=defun message ()
(=values 'hello 'there))
MESSAGE


> (=defun baz ()
(=bind (m n) (message)
(=values (list m n))))
BAZ
> (baz)
(HELLO THERE)


(let ((*cont* #'(lambda (m n)
		  (=values (list m n)))))
  (message))

(let ((*cont* #'(lambda (m n)
		  (funcall *cont* (list m n)))))
  (=message *cont*))

#'
(lambda (m n)
    (funcall *cont* (list m n)))


> (let ((f #'identity))
(let ((g #'(lambda (x) (funcall f (list 'a x)))))
#'(lambda (x) (funcall g (list 'b x)))))
#<Interpreted-Function BF6326>
> (funcall * 2)
(A (B 2))

(=defun foo (x)
	(=bind (y) (bar x)
	       (format t "Ho ")
	       (=bind (z) (baz x)
		      (format t "Hum.")
		      (=values x y z))))


> (=defun add1 (x)
(=values (1+ x)))
ADD1
> (let ((fn (=lambda (n) (add1 n))))
(=bind (y) (=funcall fn 9)
(format nil "9 + 1 = ~A" y)))
"9 + 1 = 10"


(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(setq *saved* nil)

(=defun dft-node (tree)
	(cond ((null tree) (restart))
	      ((atom tree) (=values tree))
	      (t (push #'(lambda () (dft-node (cdr tree)))
		       *saved*)
		 (dft-node (car tree)))))

(=defun restart ()
	(if *saved*
	    (funcall (pop *saved*))
	  (=values 'done)))

(=defun dft2 (tree)
	(setq *saved* nil)
	(=bind (node) (dft-node tree)
	       (cond ((eq node 'done) (=values nil))
		     (t (princ node)
			(restart)))))

> (setq t1 '(a (b (d h)) (c e (f i) g))
t2 '(1 (2 (3 6 7) 4 5)))
(1 (2 (3 6 7) 4 5))
> (dft2 t1)
ABDHCEFIG
NIL

> (=bind (node1) (dft-node t1)
(if (eq node1 'done)
'done
(=bind (node2) (dft-node t2)
(list node1 node2))))
(A 1)
> (restart)
(A 2)
. . .
> (restart)
(B 1)
. . .

#20.3 Code-Walkers and CPS Conversion

(=bind (x) (fn y)
       (list 'a x(list 'a				; wrong
      (=bind (x) (fn y) x))))

(defun rev (x)
  (if (null x)
      nil
    (append (rev (cdr x)) (list (car x)))))

(defun rev2 (x)
  (revc x #'identity))

(defun revc (x k)
  (if (null x)
      (funcall k nil)
    (revc (cdr x)
	  #'(lambda (w)
	      (funcall k (append w (list (car x))))))))

#'
(lambda (w)
    (identity (append w (list (car x)))))

