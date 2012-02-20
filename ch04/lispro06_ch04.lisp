
;;Utility 함수는 라이브러리 함수처럼 범용적이고 사용성이 높은 함수를 말한다.
;;Alexandria is a collection of portable public domain utilities
;;자주 사용되는 Alexandria 도 이런 집합이라 한다.

;;4.1 Birth of a Utility


(defun all-nicknames (names)
  (if (null names)
      nil
    (nconc (nicknames (car names))
	   (all-nicknames (cdr names)))))

(mapcan #'nicknames people)

(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town)))

(defun find-books (towns)
  (if (null towns)
      nil
    (let ((shops (bookshops (car towns))))
      (if shops
	  (values (car towns) shops)
	(find-books (cdr towns))))))

(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
	  (values (car lst) val)
	(find2 fn (cdr lst))))))

(find2 #'bookshops towns)


;;4.2 Invest in Abstraction



(> (length x) (length y))

(mapcar fn (append x y z))

;;4.3 Operations on Lists

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;> (last1 "blub")
;>>Error: "blub" is not a list.
;Broken at LAST...

(= (length lst) 1)

(mapcan #'(lambda (d) (mklist (lookup d)))
	data)


(> (length x) (length y))

> (filter #'(lambda (x) (if (numberp x) (1+ x)))
'(a 1 2 b 3 c d 4))
(2 3 4 5)

(defun longer (x y)
  (labels ((compare (x y)
		    (and (consp x)
			 (or (null y)
			     (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
      (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;list 를 묶거나 푸는 함수들이 인상적이다.

> (group '(a b c d e f g) 2)
((A B) (C D) (E F) (G))

> (flatten '(a (b c) ((d e) f)))
(A B C D E F)

> (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
(1 (3 (5)) 7 (9))


;;4.4 Search


(defun flatten (x)
  (labels ((rec (x acc)
		(cond ((null x) acc)
		      ((atom x) (cons x acc))
		      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
		(cond ((null tree) (nreverse acc))
		      ((consp (car tree))
		       (rec (cdr tree)
			    (cons (rec (car tree) nil) acc)))
		      (t (rec (cdr tree)
			      (if (funcall test (car tree))
				  acc
				(cons (car tree) acc)))))))
    (rec tree nil)))

> (before 'b 'd '(a b c d))
(B C D)


(< (position 'b '(a b c d)) (position 'd '(a b c d)))

(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
	  (values (car lst) val)
	(find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

> (before 'a 'b '(a))
(A)


> (after 'a 'b '(b a d))
(A D)
> (after 'a 'b '(a))
NIL

> (duplicate 'a '(a b c a d))
(A D)

> (split-if #'(lambda (x) (> x 4))
'(1 2 3 4 5 6 7 8 9 10))
(1 2 3 4)
(5 6 7 8 9 10)

> (most #'length '((a b) (a b c) (a) (e f g)))
(A B C)
3

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
    (let* ((wins (car lst))
	   (max (funcall fn wins)))
      (dolist (obj (cdr lst))
	(let ((score (funcall fn obj)))
	  (when (> score max)
	    (setq wins obj
		  max score))))
      (values wins max))))

(defun best (fn lst)
  (if (null lst)
      nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
	(if (funcall fn obj wins)
	    (setq wins obj)))
      wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
    (let ((result (list (car lst)))
	  (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
	(let ((score (funcall fn obj)))
	  (cond ((> score max)
		 (setq max score
		       result (list obj)))
		((= score max)
		 (push obj result)))))
      (values (nreverse result) max))))

> (best #'> '(1 2 3 4 5))
5


> (mostn #'length '((a b) (a b c) (a) (e f g)))
((A B C) (E F G))

;;;4.5 Mapping

> (map0-n #'1+ 5)
(1 2 3 4 5 6)

> (mapa-b #'1+ -2 0 .5)
(-1 -0.5 0.0 0.5 1.0)

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
	 a
	 #'(lambda (x) (> x b))
	 #'(lambda (x) (+ x step))))


(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
    (apply #'mapcar
	   #'(lambda (&rest args)
	       (apply #'rmapcar fn args))
	   args)))

(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(mapcar #'sqrt (append list1 list2))

(mapcars #'sqrt list1 list2)

> (rmapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))
123456789
(1 2 (3 4 (5) 6) 7 (8 9))

> (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
(11 (22 (33) 44))

(mapa-b #'fn a b c)

(collect (#Mfn (scan-range :from a :upto b :by c)))

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
       (format *query-io* "~A~%" (funcall fn in))))))

;; 4.6 I/O


> (readlist)
Call me "Ed"
(CALL ME "Ed")

> (prompt "Enter a number between ~A and ~A.~%>> " 1 10)
Enter a number between 1 and 10.
>> 3
3

> (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
Entering break-loop.
>> (+ 2 3)
5
>> :q
:Q


;;4.7 Symbols and Strings

> (mkstr pi " pieces of " 'pi)

> (symb 'ar "Madi" #\L #\L 0)
|ARMadiLL0|

(defun mkstr (&rest args)
  (with-output-to-string (s)
			 (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1
				      :initial-element c)))
       (symbol-name sym)))



> (let ((s (symb '(a b))))
(and (eq s '|(A B)|) (eq s '\(A\ B\))))
T


> (explode 'bomb)
(B O M B)


;;4.8 Density