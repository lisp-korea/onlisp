;; 17.1 Macro Characters

;; figure 17.1

(set-macro-character #\'
					 #'(lambda (stream char)
						 (list 'quote (read stream t nil t))))

(defmacro q (obj)
  `(quote ,obj))

(eq 'a (q a))

(eq (q (q a))
	''a)

;; 17.2 Dispatching Macro Characters

;; figure 17.2
(set-dispatch-macro-character #\# #\?
							  #'(lambda (stream char1 char2)
								  `#'(lambda (&rest ,(gensym))
									   ,(read stream t nil t))))


(mapcar #?2 '(a b c))

(eq (funcall #?'a) 'a)

(eq (funcall #?#'oddp) (symbol-function 'oddp))



;; 17.3 Delimiters

;; figure 17.3
(set-macro-character #\]
					 (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
							  #'(lambda (stream char1 char2)
								  (let ((accum nil)
										(pair (read-delimited-list #\] stream t)))
									(do ((i (ceiling (car pair)) (1+ i)))
									    ((> i (floor (cadr pair)))
										 (list 'quote (nreverse accum)))
									  (push i accum)))))


;; figure 17.4
(defmacro defdelim (left right params &body body)
  `(ddfn ,left ,right #'(lambda ,params ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
	(set-macro-character right rpar)
	(set-dispatch-macro-character #\# left
								  #'(lambda (stream char1 char2)
									  (apply fn
											 (read-delimited-list right stream t))))))
	
									  
;; from page 64
(defun mapa-b (fn a b &optional (step 1)) 
  (do ((i a (+ i step))
	   (result nil))
	  ((> i b) (nreverse result))
	(push (funcall fn i) result)))

(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

;; from page 202

(defmacro fn (expr)
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr)
		  (eq (car expr) 'lambda))
	  expr
	  (if (eq (car expr) 'compose)
		  (build-compose (cdr expr))
		  (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
	`(lambda (,g)
	   (,op ,@(mapcar #'(lambda (f)
						  `(,(rbuild f) ,g))
					  fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
	`(lambda (,g)
	   ,(labels ((rec (fns)
					  (if fns
						  `(,(rbuild (car fns))
							 ,(rec (cdr fns)))
						  g)))
				(rec fns)))))
						  

;; figure 17.5
(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

(funcall #{list 1+} 7)


;; 17.4 When What Happens

(defmacro quotable ()
  `(list 'able))

(quotable)

