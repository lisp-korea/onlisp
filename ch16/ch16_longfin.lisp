;; 16.1 Abbreviations

(defvar x '(1 2))

(destructuring-bind (a b) x (+ a b)) ;; too long...

(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

;; figure 16.1
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
	 `(,',long ,@args)))

;; from page 47...
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
		(let ((rest (nthcdr n source)))
		  (if (consp rest)
		      (rec rest (cons (subseq source 0 n) acc))
		    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro abbrevs (&rest names)
  `(progn
	,@(mapcar #'(lambda (pair)
				  `(abbrev ,@pair))
			  (group names 2))))

;; 16.2 Properties

(setf (get 'ball1 'color) 'red)

(defmacro color (obj)
  `(get ,obj 'color))

(color 'ball1)

(setf (color 'ball1) 'green)


(defmacro weight (obj)
  `(get ,obj 'weight))


;; figure 16.2
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
	 `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
	 ,@(mapcar #'(lambda (p) `(propmacro ,p))
			   props)))


(propmacro color)
(propmacro weight)


;; 16.3 Anaphoric Macros


;; aif sample

(let ((res (complicated-query)))
  (if res
	  (foo res)))

;; is

(aif (complicated-query)
	 (foo it))


;; aand sample

(let ((o (owner x)))
  (and o (let ((a (address o)))
		   (and a (city a)))))

;; is

(aand (owner x) (address it) (city it))


;; figure 16.3

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

(a+ menu-price (* it .05) (* it 3))

(alist 1 (+ 2 it) (+ 2 it))

;; figure 16.4

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


(defanaph a+)
(defanaph alist)


;; 1. it only works for operators whose arguments are all evaluated.(can't make aif)
;; 2. *it* is always bound to successive arguments.(can't make awhen)
;; 3. it won't work for a macro like setf.(can't make asetf)

(defun anaphex2 (op args)
  `(let ((it ,(car args)))
	 (,op it ,@(cdr args))))

(defmacro aif (&rest args)
  (anaphex2 'it args))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

;; from page 173

(defmacro _f (op place &rest args) 
  (multiple-value-bind (vars forms var set access) 
	                   (get-setf-expansion place)
	`(let* (,@(mapcar #'list vars forms)
			(,(car var) (,op ,access ,@args))) 
	   ,set)))


(defmacro asetf (&rest args)
  (anaphex3 'setf args))


;; figure 16.5

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
  `(let ((it ,(car args)))
	 (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))


(defanaph alist)
(defanaph aif :rule :first)
(defanaph asetf :rule :place)


(defmacro new-incf (place &optional (val 1))
  `(asetf ,place (+ it ,val)))

(defmacro pull (obj place &rest args)
  `(asetf ,place (delete,obj it ,@args)))

