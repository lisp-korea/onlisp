;;5. Returning Functions


;;The utilities in this chapter operate on functions.

;5.1 Common Lisp Evolves


(remove-if-not #'pred lst)
(remove-if #'(lambda (x) (not (pred x))) lst)
(remove-if-not #'pred lst)
(remove-if (complement #'pred) lst)
(defun joiner (obj)
  (typecase obj
    (cons #'append)
    (number #'+)))
(defun join (&rest args)
  (apply (joiner (car args)) args))
(defun make-adder (n)
  #'(lambda (x) (+ x n)))

> (setq add3 (make-adder 3))
#<Interpreted-Function BF1356>
> (funcall add3 2)
5
(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

> (remove-if (complement #'oddp) '(1 2 3 4 5 6))
(1 3 5)

;5.2 Orthogonality

(setf (get 'ball 'color) 'red)
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))


(def! #'remove-if #'delete-if)

(delete-if #'oddp lst)

(funcall (! #'remove-if) #'oddp lst)

((! remove-if) oddp lst)

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	    (setf (gethash args cache)
		  (apply fn args)))))))

;5.3 Memoizing

> (setq slowid (memoize #'(lambda (x) (sleep 5) x)))
#<Interpreted-Function C38346>
> (time (funcall slowid 1))
Elapsed Time = 5.15 seconds
1
> (time (funcall slowid 1))
Elapsed Time = 0.00 seconds
1

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
    #'identity))

;5.4 Composing Functions

(compose #'list #'1+)

#'(lambda (x) (list (1+ x)))

> (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
4

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	(if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
	  (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
    (let ((chain (apply #'fun fns)))
      #'(lambda (x)
	  (or (funcall fn x) (funcall chain x))))))

(defun complement (pred)
  (compose #'not pred))

(mapcar #'(lambda (x)
	    (if (slave x)
		(owner x)
	      (employer x)))
	people)

(mapcar (fif #'slave #'owner #'employer)
	people)

(find-if #'(lambda (x)
	     (and (signed x) (sealed x) (delivered x)))
	 docs)

(find-if (fint #'signed #'sealed #'delivered) docs)

;;5.5 Recursion on Cdrs


(defun our-length (lst)
  (if (null lst)
      0
    (1+ (our-length (cdr lst)))))

(defun our-every (fn lst)
  (if (null lst)
      t
    (and (funcall fn (car lst))
	 (our-every fn (cdr lst)))))

(defun lrec (rec &optional base)
  (labels ((self (lst)
		 (if (null lst)
		     (if (functionp base)
			 (funcall base)
		       base)
		   (funcall rec (car lst)
			    #'(lambda ()
				(self (cdr lst)))))))
    #'self))

(lrec #'(lambda (x f) (1+ (funcall f))) 0)

(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)

; copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

; remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

; find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

; some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

;;5.6 Recursion on Subtrees

(a b c) = (a . (b . (c . nil)))
(a b (c d)) = (a . (b . ((c . (d . nil)) . nil)))

(a . b) (a b c) (a b (c d))

> (setq x '(a b)
listx (list x 1))
((A B) 1)
> (eq x (car (copy-list listx)))
T

> (eq x (car (copy-tree listx)))
NIL

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
    (cons (our-copy-tree (car tree))
	  (if (cdr tree) (our-copy-tree (cdr tree))))))

(defun count-leaves (tree)
  (if (atom tree)
      1
    (+ (count-leaves (car tree))
       (or (if (cdr tree) (count-leaves (cdr tree)))
	   1))))

> (count-leaves '((a b (c d)) (e) f))
10

> (flatten '((a b (c d)) (e) f ()))
(A B C D E F)

(defun flatten (tree)
  (if (atom tree)
      (mklist tree)
    (nconc (flatten (car tree))
	   (if (cdr tree) (flatten (cdr tree))))))

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
    (or (rfind-if fn (car tree))
	(if (cdr tree) (rfind-if fn (cdr tree))))))

> (rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5))
3

(ttrav #'cons #'identity)

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
		 (if (atom tree)
		     (if (functionp base)
			 (funcall base tree)
		       base)
		   (funcall rec (self (car tree))
			    (if (cdr tree)
				(self (cdr tree)))))))
    #'self))


; our-copy-tree
(ttrav #'cons)

; count-leaves
(ttrav #'(lambda (l r) (+ l (or r 1))) 1)

; flatten
(ttrav #'nconc #'mklist)

(trec #'(lambda (o l r) (nconc (funcall l) (funcall r)))
      #'mklist)

(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)))

(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		   base)
	       (funcall rec tree
			#'(lambda ()
			    (self (car tree)))
			#'(lambda ()
			    (if (cdr tree)
				(self (cdr tree))))))))
    #'self))

;;5.7 When to Build Functions

(find-if #.(compose #'oddp #'truncate) lst)
