(defparameter *paths* nil)
(defconstant failsym '@)

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

(defun cb (fn choices)
  (if choices
      (progn
	(if (cdr choices)
	    (push #'(lambda () (cb fn (cdr choices)))
		  *paths*))
	(funcall fn (car choices)))
      (fail)))

(defmacro choose (&rest choices)
  (if choices
      `(progn
	 ,@(mapcar #'(lambda (c)
		       `(push #'(lambda () ,c) *paths*))
		   (reverse (cdr choices)))
	 ,(car choices))
      '(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun do2 (x)
  (choose (+ x 2)
	  (* x 2)
	  (expt x 2)))

;; (do2 3)
;; 5, 6, 9, @, @...

(choose-bind x '(마쉬마로 콜라겐 화이트보드)
  (format nil "으싸으싸 ~A." x))
(fail)


(let ((x 2))
  (choose
   (+ x 1)
   (+ x 100)))
(fail)

;; (let ((x 2))
;;   (PROGN (PUSH #'(LAMBDA () (+ X 100)) *PATHS*)
;; 	 (+ X 1))
;;   )

;; =================
(load "../ch20/ch20_netpyoung.lisp") ;; for =defun and others

;; http://www.cliki.net/On%20Lisp
(defparameter *cont* #'values)		; fucking #'identity

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
	`(the sum of ,n1 ,n2)
	(fail))))

(parlor-trick 7)
(fail)
;; (THE SUM OF 2 5)

(choose-bind first-name '(henry william)
  (choose-bind last-name '(james higgins)
    (=values (list first-name last-name))))
(choose (=values 1) (=values 2))
(fail)

;; cuts
(defun kids (n)
  (case n
    (a '(b c))
    (b '(d e))
    (c '(d f))
    (f '(g))))

(=defun descent (n1 n2)
  (format t "~a ~a~%" n1 n2)
  (cond ((eq n1 n2) (=values (list n2)))
	((kids n1) (choose-bind n (kids n1)
		     (=bind (p) (descent n n2)
		       (=values (cons n1 p)))))
	(t (fail))))
(defparameter *paths* nil)

;; (descent 'a 'g) ; wtf fall in recursive hall
;; (descent 'a 'd)

