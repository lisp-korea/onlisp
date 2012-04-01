;13. Computation at Compile-Time
;13.1 New Utilities
> (avg pi 4 5)
4.047...

> (most-of t t t nil)
T

(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defun most-of (&rest args)
  (let ((all 0)
	(hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
	(hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
			  `(and ,a (> (incf ,hits) ,need)))
		      args)))))

(let ((count 0))
  (or (and (a) (> (incf count) 1))
      (and (b) (> (incf count) 1))
      (and (c) (> (incf count) 1))))

(defun nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

(defmacro nthmost (n lst)
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

> (nthmost 2 '(2 6 1 5 3 4))
4

(nthmost 2 nums)

(let ((#:g7 nums))
  (unless (< (length #:g7) 3)
    (let ((#:g6 (pop #:g7)))
      (setq #:g1 #:g6))
    (let ((#:g5 (pop #:g7)))
      (if (> #:g5 #:g1)
	  (setq #:g2 #:g1 #:g1 #:g5)
	(setq #:g2 #:g5)))
    (let ((#:g4 (pop #:g7)))
      (if (> #:g4 #:g1)
	  (setq #:g3 #:g2 #:g2 #:g1 #:g1 #:g4)
	(if (> #:g4 #:g2)
	    (setq #:g3 #:g2 #:g2 #:g4)
	  (setq #:g3 #:g4))))
    (dolist (#:g8 #:g7)
      (if (> #:g8 #:g1)
	  (setq #:g3 #:g2 #:g2 #:g1 #:g1 #:g8)
	(if (> #:g8 #:g2)
	    (setq #:g3 #:g2 #:g2 #:g8)
	  (if (> #:g8 #:g3)
	      (setq #:g3 #:g8)
	    nil))))
    #:g3))

;13.2 Bezier Curves Example

;x = (x 3 - 3x 2 + 3x 1 - x 0 )u 3 + (3x 2 - 6x 1 + 3x 0 )u 2 + (3x 1 - 3x 0 )u + x 0
;y = (y 3 - 3y 2 + 3y 1 - y 0 )u 3 + (3y 2 - 6y 1 + 3y 0 )u 2 + (3y 1 - 3y 0 )u + y 0

;13.3 Applications

(defconstant *segs* 20)

(defconstant *du* (/ 1.0 *segs*))

(defconstant *pts* (make-array (list (1+ *segs*) 2)))

(defmacro genbez (x0 y0 x1 y1 x2 y2 x3 y3)
  (with-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
		`(let ((,gx0 ,x0) (,gy0 ,y0)
		       (,gx1 ,x1) (,gy1 ,y1)
		       (,gx3 ,x3) (,gy3 ,y3))
		   (let ((cx (* (- ,gx1 ,gx0) 3))
			 (cy (* (- ,gy1 ,gy0) 3))
			 (px (* (- ,x2 ,gx1) 3))
			 (py (* (- ,y2 ,gy1) 3)))
		     (let ((bx (- px cx))
			   (by (- py cy))
			   (ax (- ,gx3 px ,gx0))
			   (ay (- ,gy3 py ,gy0)))
		       (setf (aref *pts* 0 0) ,gx0
			     (aref *pts* 0 1) ,gy0)
		       ,@(map1-n #'(lambda (n)
				      (let* ((u (* n *du*))
					     (u^2 (* u u))
					     (u^3 (expt u 3)))
					`(setf (aref *pts* ,n 0)
					       (+ (* ax ,u^3)
						  (* bx ,u^2)
						  (* cx ,u)
						  ,gx0)
					       (aref *pts* ,n 1)
					       (+ (* ay ,u^3)
						  (* by ,u^2)
						  (* cy ,u)
						  ,gy0))))
				  (1- *segs*))
		       (setf (aref *pts* *segs* 0) ,gx3
			     (aref *pts* *segs* 1) ,gy3))))))

