;; 13.1
(defun f_avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro m_avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(f_avg pi 4 5)
(m_avg pi 4 5) ;; (/ (+ PI 4 5) 3)

(defun f_most-of (&rest args)
  (let ((all 0)
	(hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro m_most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
	(hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
			 `(and ,a (> (incf ,hits) ,need)))
		     args)))))

(f_most-of t t t nil)
(m_most-of t t t nil)
;; (LET ((#:G1166 0))
;;   (OR (AND T (> (INCF #:G1166) 2)) (AND T (> (INCF #:G1166) 2))
;;       (AND T (> (INCF #:G1166) 2)) (AND NIL (> (INCF #:G1166) 2))))

(defun f_nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
		 syms)
     ,@body))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n) (mapa-b fn 0 n))
(defun map1-n (fn n) (mapa-b fn 1 n))


(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
		(let ((var (gensym)))
		  `(let ((,var (pop ,glst)))
		     ,(nthmost-gen var (reverse syms)))))
	    (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
	(if (and (not long?) (null else))
	    `(setq ,(car vars) ,var)
	    `(if (> ,var ,(car vars))
		 (setq ,@(mapcan #'list
				 (reverse vars)
				 (cdr (reverse vars)))
		       ,(car vars) ,var)
		 ,else)))))

(defmacro m_nthmost (n lst)
  (if (and (integerp n) (< n 20))
      (with-gensyms (glst gi)
	(let ((syms (map0-n #'(lambda (x) (gensym)) n)))
	  `(let ((,glst ,lst))
	     (unless (< (length ,glst) ,(1+ n))
	       ,@(gen-start glst syms)
	       (dolist (,gi ,glst)
		 ,(nthmost-gen gi syms t))
	       ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))


(f_nthmost 2 '(2 6 1 5 3 4))
(m_nthmost 2 '(2 6 1 5 3 4))
;; skip expand


