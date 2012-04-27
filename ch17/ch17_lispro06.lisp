;17. Read-Macros
;17.1 Macro Characters

(set-macro-character #\'
		     #'(lambda (stream char)
			 (list 'quote (read stream t nil t))))


;''a

;becomes

(quote (quote a))

;whereas if we had tried to define an abbreviation for quote using a normal macro,

(defmacro q (obj)
  `(quote ,obj))

;it would work in isolation,

(eq 'a (q a))
;T

;but not when nested. For example,

(q (q a))

;would expand into

(quote (q a))
;(Q A)

(set-dispatch-macro-character #\# #\?
			      #'(lambda (stream char1 char2)
				  `#'(lambda (&rest ,(gensym))
				       ,(read stream t nil t))))

;17.2 Dispatching Macro Characters

(mapcar #?2 '(a b c))
(2 2 2)

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
			      #'(lambda (stream char1 char2)
				  (let ((accum nil)
					(pair (read-delimited-list #\] stream t)))
				    (do ((i (ceiling (car pair)) (1+ i)))
					((> i (floor (cadr pair)))
					 (list 'quote (nreverse accum)))
				      (push i accum)))))

(eq (funcall #?'a) 'a)
;T
(eq (funcall #?#'oddp) (symbol-function 'oddp))
; REPL에서 (eq x y)라고 나온다.
;T

;17.3 Delimiters

#[2 7]
;(2 3 4 5 6 7)

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
				  #'(lambda (stream char1 char2)
				      (apply fn
					     (read-delimited-list right stream t))))))

(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

(let ((f1 (compose #'list #'1+))
(f2 #'(lambda (x) (list (1+ x)))))
(equal (funcall f1 7) (funcall f2 7)))
;T

(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

#.(compose #'list #'1+)

;17.4 When What Happens

(defmacro quotable ()
  '(list 'able))

(defmacro quotable ()
  (quote (list (quote able))))

