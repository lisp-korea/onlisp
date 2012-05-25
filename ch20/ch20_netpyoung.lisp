(defparameter *cont* #'identity)

(defmacro =lambda (params &body body)
  `#'(lambda (*cont* ,@params) ,@body))

(defmacro =defun (name params &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
      (defmacro ,name ,params
	`(,',f *cont* ,,@params))
      (defun ,f (*cont* ,@params) ,@body))))

(defmacro =bind (params expr &body body)
  `(let ((*cont* #'(lambda ,params ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(=defun add1 (x)
  (=values (1+ x)))

(print (add1 10))

(funcall #'(lambda (*cont* n) (=values (1+ n)))
	 *cont* 2)

(funcall #'identity (1+ 2))

(=defun bar (x)
  (=values (list 'a (add1 x))))
(bar 5)

(=defun message ()
  (=values 'hello 'there))

(=defun baz ()
  (=bind (m n) (message)
    (=values (list m n))))


;; (baz)

(=bind (m n) (message) (list m n))
;; (LET ((*CONT* #'(LAMBDA (M N) (LIST M N))))
;;   (=MESSAGE *CONT*))

(defparameter *func*
  (let ((f #'identity))
    (let ((g #'(lambda (x)
		 (funcall f (list 'a x)))))
      #'(lambda (x)
	  (funcall g (list 'b x))))))

(funcall *func* 2)

(let ((fn (=lambda (n)
	    (add1 n))))
  (=bind (y) (=funcall fn 9)
    (format nil "9 + 1 = ~A" y)))
;;

(defun dft (tree)
  (cond ((null tree) nil)
	((atom tree) (princ tree))
	(t (dft (car tree))
	   (dft (cdr tree)))))

(defparameter *saved* nil)

(=defun restart! ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))

(=defun dft-node (tree)
  (cond ((null tree) (restart!))
	((atom tree) (=values tree))
	(t (push #'(lambda () (dft-node (cdr tree)))
		 *saved*)
	   (dft-node (car tree)))))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) nil)	;(=values nil))
	  (t (princ node)
	     (restart!)))))

(defparameter t1 '(a
		   (b (d h))
		   (c e (f i) g)))

(defparameter t2 '(1
		   (2 (3 6 7) 4 5)))


(dft2 t1)
;; ABDHCEFIG

(=bind (node1) (dft-node t1)
  (if (eq node1 'done)
      'done
      (=bind (node2) (dft-node t2)
	(list node1 node2))))

(restart!)
;; 
(defun rev (x)
  (if (null x)
      nil
      (append (rev (cdr x)) (list (car x)))))

(rev '(1 2 3))

(defun revc (x k)
  (if (null x)
      (funcall k nil)
      (revc (cdr x)
	    #'(lambda (w)
		(funcall k (append w (list (car x))))))))

(defun rev2 (x)
  (revc x #'identity))

(rev2 '(1 2 3))
