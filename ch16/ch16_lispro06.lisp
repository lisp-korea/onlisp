;16. Macro-Defining Macros
;16.1 Abbreviations

;The simplest use of macros is as abbreviations.

;(let ((a (car x)) (b (cdr x))) ...)

;(destructuring-bind (a . b) x ...)

; 18장에 나오는데, car, cdr을 하지 않고, destructuring을 통해 리스트나 배열에서 원하는 요소를 추출할 수 있다.

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		    `(abbrev ,@pair))
		(group names 2))))

(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

(abbrev mvbind multiple-value-bind)

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))



(defmacro mvbind (&rest args)
  (let ((name 'multiple-value-bind))
    `(,name ,@args)))

`(defmacro ,short (&rest args)
   (let ((name ',long))
     `(,name ,@args)))

`(defmacro ,short (&rest args)
   `(,',long ,@args))



(abbrevs dbind destructuring-bind
	 mvbind multiple-value-bind
	 mvsetq multiple-value-setq)

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
		props)))

;16.2 Properties

(setf (get o p) v)

(setf (get 'ball1 'color) 'red)

(defmacro color (obj)
  `(get ,obj 'color))

;;특정 심볼이나 대표성을 갖는 객체에 속성 등을 할당할 수 있다.

> (color 'ball1)
;RED

> (setf (color 'ball1) 'green)
;GREEN

(defmacro weight (obj)
  `(get ,obj 'weight))

(propmacro color)

(defmacro color (obj)
  `(get ,obj 'color))

(defmacro color (obj)
  (let ((p 'color))
    `(get ,obj ',p)))

`(defmacro ,propname (obj)
   (let ((p ',propname))
     `(get ,obj ',p)))

`(defmacro ,propname (obj)
   `(get ,obj ',',propname))

;16.3 Anaphoric Macros

(let ((res (complicated-query)))
  (if res
      (foo res)))

(aif (complicated-query)
     (foo it))

(let ((o (owner x)))
  (and o (let ((a (address o)))
	   (and a (city a)))))

(aand (owner x) (address it) (city it))

(defmacro a+ (&rest args)
  (a+expand args nil))

(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(a+expand (cdr args)
		      (append syms (list sym)))))
    `(+ ,@syms)))

(defmacro alist (&rest args)
  (alist-expand args nil))

(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(alist-expand (cdr args)
			  (append syms (list sym)))))
    `(list ,@syms)))

(defun mass-cost (menu-price)
  (a+ menu-price (* it .05) (* it 3)))

(mass-cost 7.95)
9.54

(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(anaphex (cdr args)
		     (append expr (list sym)))))
    expr))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(a+ menu-price (* it .05) (* it 3))

(let* ((#:g2 menu-price) (it #:g2))
  (let* ((#:g3 (* it 0.05)) (it #:g3))
    (let* ((#:g4 (* it 3)) (it #:g4))
      (+ #:g2 #:g3 #:g4))))

(alist 1 (+ 2 it) (+ 2 it))
(1 3 5)

(defanaph a+)

(defanaph alist)

(defmacro a+ (&rest args)
  (anaphex args '(+)))

(defun anaphex2 (op args)
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defmacro aif (&rest args)
  (anaphex2 'if args))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro asetf (&rest args)
  (anaphex3 'setf args))

(defanaph alist)

(defanaph aif :rule :first)

(defanaph asetf :rule :place)

(defmacro defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
	 (body (case rule
		 (:all `(anaphex1 args '(,opname)))
		 (:first `(anaphex2 ',opname args))
		 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(anaphex1 (cdr args)
		      (append call (list sym)))))
    call))

(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))



(defmacro incf (place &optional (val 1))
  `(asetf ,place (+ it ,val)))

(defmacro pull (obj place &rest args)
  `(asetf ,place (delete ,obj it ,@args)))


