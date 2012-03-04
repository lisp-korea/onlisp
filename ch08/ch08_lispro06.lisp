;;8. When to Use Macros
;;8.1 When Nothing Else Will Do

(defun 1+ (x) (+ 1 x))

(defmacro 1+ (x) `(+ 1 ,x))


 	
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


(progn (rplaca x 'a) 'a).

(defmacro foo (x)
  `(+ ,x y))


;;8.2 Macro or Function?

(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))


;The Pros

;1. Computation at compile-time. A macro call involves computation at two times: when the macro is expanded, and when the expansion is evaluated. All the macroexpansion in a Lisp program is done when the program is compiled, and every bit of computation which can be done at compile-time is one bit that won't slow the program down when it's running. If an operator could be written to do some of its work in the macroexpansion stage, it will be more efficient to make it a macro, because whatever work a smart compiler can't do itself, a function has to do at runtime. Chapter 13 describes macros like avg which do some of their work during the expansion phase.
;2. Integration with Lisp. Sometimes, using macros instead of functions will make a program more closely integrated with Lisp. Instead of writing a program to solve a certain problem, you may be able to use macros to transform the problem into one that Lisp already knows how to solve. This approach, when possible, will usually make programs both smaller and more efficient: smaller because Lisp is doing some of your work for you, and more efficient because production Lisp systems generally have had more of the fat sweated out of them than user programs. This advantage appears mostly in embedded languages, which are described starting in Chapter 19.
;3. Saving function calls. A macro call is expanded right into the code where it appears. So if you write some frequently used piece of code as a macro, you can save a function call every time it's used. In earlier dialects of Lisp, programmers took advantage of this property of macros to save function calls at runtime. In Common Lisp, this job is supposed to be taken over by functions declared inline.

;The Cons

;1. Functions are data, while macros are more like instructions to the compiler. Functions can be passed as arguments (e.g. to apply),returned by functions, or stored in data structures. None of these things are possible with macros. In some cases, you can get what you want by enclosing the macro call within a lambda-expression. This works, for example, if you want to apply or funcall certain macros:

 (funcall #'(lambda (x y) (avg x y)) 1 3)
;2

; However, this is an inconvenience. It doesn't always work, either: even if, like avg, the macro has an &rest parameter, there is no way to pass it a varying number of arguments.

;2. Clarity of source code. Macro definitions can be harder to read than the equivalent function definitions. So if writing something as a macro would only make a program marginally better, it might be better to use a function instead.
;3. Clarity at runtime. Macros are sometimes harder to debug than functions. If you get a runtime error in code which contains a lot of macro calls, the code you see in the backtrace could consist of the expansions of all those macro calls, and may bear little resemblance to the code you originally wrote. And because macros disappear when expanded, they are not accountable at runtime. You can't usually use trace to see how a macro is being called. If it worked at all, trace would show you the call to the macro's expander function, not the macro call itself.
;4. Recursion. Using recursion in macros is not so simple as it is in functions. Although the expansion function of a macromay be recursive, the expansion itself may not be. Section 10.4 deals with the subject of recursion in macros.

;8.3 Applications for Macros

(defmacro nil! (x)
  `(setf ,x nil))

(do ()
    ((not #condition#))
  . #body of code#)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(while #condition#
  . #body of code#)


(defun foo (x) (* x 2))

(setf (symbol-function 'foo)
      #'(lambda (x) (* x 2)))

(defmacro our-defun (name parms &body body)
  `(progn
     (setf (symbol-function ',name)
	   #'(lambda ,parms (block ,name ,@body)))
     ',name))

(defun move-objs (objs dx dy)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (incf (obj-x o) dx)
      (incf (obj-y o) dy))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
	      (max x1 xb) (max y1 yb)))))

(defun scale-objs (objs factor)
  (multiple-value-bind (x0 y0 x1 y1) (bounds objs)
    (dolist (o objs)
      (setf (obj-dx o) (* (obj-dx o) factor)
	    (obj-dy o) (* (obj-dy o) factor)))
    (multiple-value-bind (xa ya xb yb) (bounds objs)
      (redraw (min x0 xa) (min y0 ya)
	      (max x1 xb) (max y1 yb)))))

(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
	(x0 (gensym)) (y0 (gensym))
	(x1 (gensym)) (y1 (gensym)))
    `(let ((,gob ,objs))
       (multiple-value-bind (,x0 ,y0 ,x1 ,y1) (bounds ,gob)
	 (dolist (,var ,gob) ,@body)
	 (multiple-value-bind (xa ya xb yb) (bounds ,gob)
	   (redraw (min ,x0 xa) (min ,y0 ya)
		   (max ,x1 xb) (max ,y1 yb)))))))

(defun move-objs (objs dx dy)
  (with-redraw (o objs)
	       (incf (obj-x o) dx)
	       (incf (obj-y o) dy)))

(defun scale-objs (objs factor)
  (with-redraw (o objs)
	       (setf (obj-dx o) (* (obj-dx o) factor)
		     (obj-dy o) (* (obj-dy o) factor))))

