;; 
;; 12.1
(defparameter lst '(a b c))

(setf (car lst) 480)
;; (SB-KERNEL:%RPLACA LST 480)
lst

(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))

(let ((lst '(a b c)))
  (toggle (car lst))
  lst)

(defvar *friends* (make-hash-table))
(setf (gethash 'mary *friends*) (make-hash-table))
(setf (gethash 'john (gethash 'mary *friends*)) t)

;; (setf (gethash x (gethash y *friends*))
;;      (not (gethash x (gethash y *friends*))))

(defmacro friend-of (p q)
  `(gethash ,p (gethash ,q *friends*)))
;; (toggle (friend-of x y))

;;
;; 12.2
(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))
(defparameter i 1)
(toggle (nth (incf i) lst))
;; (SETF (NTH (INCF I) LST)
;;       (NOT (NTH (INCF I) LST)))
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

(define-modify-macro toggle () not)
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

;;(toggle (nth (incf i) lst))
;; (LET* ((#:G944 (INCF I))
;;        (#:LST941 LST)
;;        (#:NEW940 (NOT (NTH #:G944 #:LST941))))
;;   (SB-KERNEL:%SETNTH #:G944 #:LST941 #:NEW940))
;; 
;; 12.3
(setf x 1
      y 2)

(setf x nil
      y nil
      z nil)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
		 syms)
     ,@body))

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args)))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))

(defmacro tf (&rest args)
  `(allf t ,@args))

(define-modify-macro toggle2 () not)

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
	       args)))

(toggle (nth (incf i) lst))
;; (PROGN (TOGGLE2 (NTH (INCF I) LST)))
;; (PROGN (LET* ((#:G970 (INCF I)) (#:LST967 LST) (#:NEW966 (NOT (NTH #:G970 #:LST967))))
;;          (SB-KERNEL:%SETNTH #:G970 #:LST967 #:NEW966)))

(nilf x y z)
;; (ALLF NIL X Y Z)
;; (LET ((#:G934 NIL))
;;   (SETF X #:G934
;;         Y #:G934
;;         Z #:G934))

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))
;;
;; 12.4
(setf x (+ x y))
(incf x y)


(defmacro _f (op place &rest args)
  `(setf ,place (,op ,place ,@args)))

(defparameter a (make-array 10))
(defparameter i 3)
(incf (aref a (incf i)))
;; (LET* ((#:A1044 A)
;;        (#:G1043 (INCF I))
;;        (#:G1045 1)
;;        (#:NEW1040 (+ (AREF #:A1044 #:G1043) #:G1045)))
;;   (SB-KERNEL:%ASET #:A1044 #:G1043 #:NEW1040))
(get-setf-expansion '(aref a (incf i)))
;; (#:A1039 #:G1038)
;; (A (INCF I))
;; (#:NEW1035)
;; (SB-KERNEL:%ASET #:A1039 #:G1038 #:NEW1035)
;; (AREF #:A1039 #:G1038)

;;
;; 12.4
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))

(_f * i 3 4 5)
;; (LET* ((#:NEW1054 (* I 3 4 5)))
;;   (SETQ I #:NEW1054))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

(let ((lst '(1 2 3 4 5 6)))
  (pull-if #'oddp lst)
  lst)


(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
	      ,@(mapcar #'list vars forms)
	      (,glst ,access)
	      (,(car var) (nthcdr ,gn ,glst)))
	 (prog1 (subseq ,glst 0 ,gn)
	   ,set)))))

(let ((x '(a b c d e f)))
  (popn 3 x)
  x)

(let ((x 10)
      (y 20))
  (if (> y x) (rotatef x y))
  x)


(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
			    (multiple-value-list (get-setf-expansion p)))
			places))
(let ((x 1) (y 2) (z 3))
  (sortf #'> x y z)
  (list x y z))
(/ 2 3)
;; (multiple-value-list (floor 1 2))

