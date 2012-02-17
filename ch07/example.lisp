(defmacro nil! (var) 
  (list 'setq var nil))

(defvar x 10)

x

(nil! x)

x


(setq a 1 b 2 c 3)

`(a ,b c)

`(a (,b c))

`(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b)))

(defmacro nil! (var) 
  (list 'setq var nil))


(defmacro nil! (var) 
  `(setq ,var nil))


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

(setq b '(1 2 3))

`(a ,b c)

`(a ,@b c)


(defmacro our-when (test &body body) 
  `(if ,test
	   (progn
         ,@body)))


(defun greet (name) 
  `(hello ,name))



(defmacro memq (obj lst)
  `(member ,obj ,lst :test #’eq))


(defmacro while (test &body body) 
  `(do ()
       ((not ,test))
     ,@body))

(defvar n 0)
(while (< n 3) (format t "~A~%" n)(setq n (1+ n)))


(pprint (macroexpand-1 `(or x y)))

(pprint (macroexpand `(while (able) (laugh))))

(pprint (macroexpand-1 `(while (able) (laugh))))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(mac `(while (able) (laugh)))


(destructuring-bind (x (y) . z) 
	`(a (b) c d) (list x y z))


(dolist (x '(a b c)) 
  (print x))


(defmacro our-dolist ((var list &optional result) &body body) 
  `(progn
	 (mapc #'(lambda (,var) ,@body) ,list)
	 (let ((,var nil)) ,result)))

(our-dolist (x '(a b c)) 
  (print x))

(defmacro when-bind ((var expr) &body body) 
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(when-bind (input (get-user-input)) (process input))


(defmacro our-expander (name) 
  `(get ,name 'expander))
(defmacro our-defmacro (name parms &body body) 
  (let ((g (gensym)))
	`(progn
	   (setf (our-expander ',name)
			 #'(lambda (,g) (block ,name
							  (destructuring-bind ,parms (cdr ,g) ,@body))))
	   ',name)))
(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
	  (funcall (our-expander (car expr)) expr) expr))