;; 10.1 Number of Evaluations

;; correct version
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;; subject multiple evaluations
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; Incorrect Order of evaluation은 다음번에
(let ((x 2))
  (for (i 1 (incf x))
    (princ i)))

;; ===================================
;; 10.2 Order of Evaluation

(setq x 10)
;; right <- left
(+ (setq x 3) x)

;; Incorrect Order of evaluation
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop)
	  (,var ,start (1+ ,var)))
	 ((> ,var ,gstop))
       ,@body)))

(let ((x 1))
  (for (i x (setq x 13))
    (princ i)))

;; correct version
(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(let ((x 1))
  (for (i x (setq x 13))
    (princ i)))

;; 10.3 Non-functional Expanders
(defparameter *nil!s* 0)
;; *nil!s* = 0
(defmacro nil! (x)
  (incf *nil!s*)
  `(setf ,x nil))
;; *nil!s* = 0

(nil! hello)
;; *nil!s* = 2

(nil! lol)
;; *nil!s* = 4

;; string-intern *package*
(defmacro string-call (opstring &rest args)
  `(,(intern opstring) ,@args))
(defun our+ (x y) (+ x y))
(string-call "OUR+" 2 3)

(defun et-al (&rest args)
  (nconc args (list 'et 'al)))
(et-al 'smith 'jones)

(setq greats '(leonardo michelangelo))
;; (LEONARDO MICHELANGELO)
(apply #'et-al greats)
;; (LEONARDO MICHELANGELO ET AL)
greats
;; (LEONARDO MICHELANGELO)


(defmacro echo (&rest args)
  `',(nconc args (list 'amen)))

(defun foo () (echo x))
;; CL-USER> (foo)
;; (X AMEN)
;; CL-USER> (foo)
;; (X AMEN)

(defmacro echo (&rest args)
  `'(,@args amen))
;; CL-USER> (foo)
;; (X AMEN)
;; CL-USER> (foo)
;; (X AMEN)

(defmacro crazy (expr) (nconc expr (list t)))
(defun foo () (crazy (list)))
;; CL-USER> (foo)
;; (T)
;; CL-USER> (foo)
;; (T)


(defun our-lenth (x)
  "recursive"
  (if (null x)
      0
      (1+ (our-lenth (cdr x)))))

(our-lenth '(1 2 3 4 a))
;; 5

(defun our-lenth (x)
  "iterative"
  (do ((len 0 (1+ len))
       (y x (cdr y)))
      ((null y) len)))

(our-lenth '(1 2 3 4 a))
;; 5


;; (if (= x 0)
;;     (car x)
;;     (nthb (1- x) (cdr y)))

(defun ntha (n lst)
  (if (= n 0)
      (car lst)
      (ntha (1- n) (cdr list))))

(defmacro nthb (n lst)
  `(if (= ,n 0)
       (car ,lst)
       (nthb (1- ,n) (cdr ,lst))))
(compile 'nthb)
(defun our-add (x y) (+ x y))
(compile our-add)
