;12. Generalized Variables

;This chapter looks at the implications of setf, and then shows some examples of macros which can be built upon it.

;12.1 The Concept

> (setq lst '(a b c))
(A B C)
> (setf (car lst) 480)
480
> lst
(480 B C)

(progn (rplaca lst 480) 480)

(defmacro toggle (obj)			; wrong
  `(setf ,obj (not ,obj)))

> (let ((lst '(a b c)))
(toggle (car lst))
lst)
(NIL B C)

(defvar *friends* (make-hash-table))

(setf (gethash 'mary *friends*) (make-hash-table))

(setf (gethash 'john (gethash 'mary *friends*)) t)

(setf (gethash x (gethash y *friends*))
      (not (gethash x (gethash y *friends*))))

(defmacro friend-of (p q)
  `(gethash ,p (gethash ,q *friends*)))

(toggle (friend-of x y))

;12.2 The Multiple Evaluation Problem

(defmacro toggle (obj)			; wrong
  `(setf ,obj (not ,obj)))

(toggle (nth (incf i) lst))

(setf (nth (incf i) lst)
      (not (nth (incf i) lst)))

> (let ((lst '(t nil t))
(i -1))
(toggle (nth (incf i) lst))
lst)
(T NIL T)

(> (let ((lst '(t nil t))
(i -1))
(toggle (nth (incf i) lst))
lst)
(T NIL T)

(define-modify-macro toggle () not)

> (let ((lst '(t nil t))
(i -1))
(toggle (nth (incf i) lst))
lst)
(NIL NIL T)

(defmacro allf (val &rest args)
  (with-gensyms (gval)
		`(let ((,gval ,val))
		   (setf ,@(mapcan #'(lambda (a) (list a gval))
				    args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
		args)))

(define-modify-macro toggle2 () not)

;12.3 New Utilities

(setf x 1 y 2)

(setf x nil y nil z nil)

(nilf x y z)

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

(nconc x y)

(setq x (nconc x y))

(define-modify-macro conc1f (obj) conc1)

;12.4 More Complex Utilities

(setf x (+ x y))

(incf x y)

(setf (obj-dx o) (* (obj-dx o) factor))

(_f * (obj-dx o) factor)

(defmacro _f (op place &rest args)	; wrong
  `(setf ,place (,op ,place ,@args)))

(incf (aref a (incf i)))

> (get-setf-method '(aref a (incf i)))
(#:G4 #:G5)
(A (INCF I))
(#:G6)
(SYSTEM:SET-AREF #:G6 #:G4 #:G5)
(AREF #:G4 #:G5)

(let* ((#:g4 a)
       (#:g5 (incf i)))
  ...)

(let* ((#:g4 a)
       (#:g5 (incf i))
       (#:g6 (1+ (aref #:g4 #:g5))))
  ...)

(let* ((#:g4 a)
       (#:g5 (incf i))
       (#:g6 (1+ (aref #:g4 #:g5))))
  (system:set-aref #:g6 #:g4 #:g5))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    `(let* (,@(mapcar #'list vars forms)
	       (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (with-gensyms (gn glst)
		  `(let* ((,gn ,n)
			  ,@(mapcar #'list vars forms)
			  (,glst ,access)
			  (,(car var) (nthcdr ,gn ,glst)))
		     (prog1 (subseq ,glst 0 ,gn)
		       ,set)))))

(_f memoize (symbol-function 'foo))

(defmacro conc1f (lst obj)
  `(_f nconc ,lst (list ,obj)))

> (setq x '(1 2 (a b) 3))
(1 2 (A B) 3)
> (pull 2 x)
(1 (A B) 3)
> (pull '(a b) x :test #'equal)
(1 3)
> x
(1 3)

(defmacro pull (obj seq &rest args)	; wrong
  `(setf ,seq (delete ,obj ,seq ,@args)))

(define-modify-macro pull (obj &rest args)
  (lambda (seq obj &rest args)
    (apply #'delete obj seq args)))

> (let ((lst '(1 2 3 4 5 6)))
(pull-if #'oddp lst)
lst)
(2 4 6)

> (setq x '(a b c d e f))
(A B C D E F)
> (popn 3 x)
(A B C)
> x
(D E F)

(if (> y x) (rotatef x y))

> (setq x 1 y 2 z 3)
3
> (sortf > x y z)
3
> (list x y z)
(3 2 1)

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
			    (multiple-value-list
			     (get-setf-method p)))
			places))
	 (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
		    (mapcan #'(lambda (m)
				(append (first m)
					(third m)))
			    meths)
		    (mapcan #'(lambda (m)
				(append (second m)
					(list (fifth m))))
			    meths))
       ,@(mapcon #'(lambda (rest)
		      (mapcar
		       #'(lambda (arg)
			   `(unless (,op ,(car rest) ,arg)
			      (rotatef ,(car rest) ,arg)))
		       (cdr rest)))
		  temps)
       ,@(mapcar #'fourth meths))))

(sortf bigger (cake 'moe) (cake 'larry) (cake 'curly))

(sortf > x (aref ar (incf i)) (car lst))

(let* ((#:g1 x)
       (#:g4 ar)
       (#:g3 (incf i))
       (#:g2 (aref #:g4 #:g3))
       (#:g6 lst)
       (#:g5 (car #:g6)))
  (unless (> #:g1 #:g2)
    (rotatef #:g1 #:g2))
  (unless (> #:g1 #:g5)
    (rotatef #:g1 #:g5))
  (unless (> #:g2 #:g5)
    (rotatef #:g2 #:g5))
  (setq x #:g1)
  (system:set-aref #:g2 #:g4 #:g3)
  (system:set-car #:g6 #:g5))

(defmacro _f (op place &rest args)
  (let ((g (gensym)))
    (multiple-value-bind (vars forms var set access)
	(get-setf-method place)
      `(let* ((,g ,op)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (funcall ,g ,access ,@args)))
	 ,set))))

> (let ((x 2))
(_f nif x 'p 'z 'n)
x)
P

;12.5 Defining Inversions

(defsetf symbol-value set)

(defsetf car (lst) (new-car)
  `(progn (rplaca ,lst ,new-car)
	  ,new-car))

(let* ((#:g2 x)
       (#:g1 y))
  (progn (rplaca #:g2 #:g1)
	 #:g1))

(defvar *cache* (make-hash-table))

(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
	(values x y)
      (cdr (assoc key *world*)))))

(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

(defun (setf car) (new-car lst)
  (rplaca lst new-car)
  new-car)

((a . 2) (b . 16) (c . 50) (d . 20) (f . 12))

> (retrieve 'c)
50

> (setf (retrieve 'n) 77)
77
> (retrieve 'n)
77
T

