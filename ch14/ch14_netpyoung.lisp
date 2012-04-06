;; 14
(defun big-long-calculation () :Hello)
(defun foo (a) (print a))

(let ((result (big-long-calculation)))
  (if result
      (foo result)))
;; (if (big-long-calculation)
;;     (foo it))
;; 14.1

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
	 ,then-form
	 ,else-form)))
(aif (big-long-calculation)
     (foo it))
;; (LET ((IT (BIG-LONG-CALCULATION)))
;;   (IF IT
;;       (FOO IT)
;;       NIL))


(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(awhen (big-long-calculation)
  (foo it))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(let ((count 0))
  (defun something ()
    (when (< (incf count) 3)
      count)))

(awhile (something)
  (foo it))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(aand 1 2 3)

(defmacro test (a b ret)
  `(let ((,a ,(car b)))
     (if ,a (progn
	      (let ((it ,a))
		,@(cdr b))
	      (return-from ,ret)))))

(let ((sym (gensym))
      (letblock (gensym)))
  (defmacro acond (&rest clauses)
    (let ((forms 
	   (loop for cl1 in clauses
	      collect `(test ,sym ,cl1 ,letblock))))
      `(block ,letblock ,@forms))))


(acond ((= 1 1) (princ 1))
       ((= 2 0) (princ 2)))
;; (BLOCK #:G926
;;   (TEST #:G925 ((= 1 1) (PRINC 1)) #:G926)
;;   (TEST #:G925 ((= 2 0) (PRINC 2)) #:G926))

;; (BLOCK #:G926
;;   (LET ((#:G925 (= 1 1)))
;;     (IF #:G925
;;         (PROGN
;;          (LET ((IT #:G925))
;;            (PRINC 1))
;;          (RETURN-FROM #:G926))))
;;   (LET ((#:G925 (= 2 0)))
;;     (IF #:G925
;;         (PROGN
;;          (LET ((IT #:G925))
;;            (PRINC 2))
;;          (RETURN-FROM #:G926)))))
