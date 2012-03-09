;;;
;;; File: ch11_okie.lisp
;;;
;;; Created: Friday, March  9 2012
;;; 
;;;
;;;

;; 11. Classic Macro
;; 11.1 Creating Context
;; 이장에서 다루는 Context는 2가지 
;; a. lexical environment
;; b. the state of the world, (the values of special variables, the contents of data structures, the state of things outside lisp)


(defmacro mac (expr)
           `(pprint (macroexpand-1 ',expr)))

(let ((x 'b)) (list x))

(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x) (cadr x) nil))
              binds)))

(let ((x 1) (y 2))
  (+ x y))

(our-let ((x 1) (y 2))
         (+ x y))

(mac (our-let ((x 1) (y 2))
         (+ x y)))


;; Macros which bind variables
(when-bind* ((x (find-if #'consp '(a (1 2) b)))
             (y (find-if #'oddp x)))
            (+ y 10))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

;; simplifed by with-gensyms

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro with-redraw ((var objs) &body body)
  (let ((gob (gensym))
        (x0 (gensym)) (y0 (gensym))
                (x1 (gensym)) (y1 (gensym)))
    ...))

(defmacro with-redraw ((var objs) &body body)
  (with-gensyms (gob x0 y0 x1 y1)
    ...))


;; combination of cond and let
(let ((sun-place 'park) (rain-place 'library))
  (if (sunny)
      (visit sun-place)
      (visit rain-place)))


(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car
                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))


(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (car bindform) vars))
                        (cdr bindform))))
          (cdr cl)))

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t          (x (princ 'e)) (z (princ 'f))))
         (list x y z))

(mac (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (y (princ 'c)) (x (princ 'd)))
          (t          (x (princ 'e)) (z (princ 'f))))
         (list x y z)))


;; 11.2 The with-Macro
;; 일반적으로 context를 생성하는 macro는 with로 시작한다.

(with-open-file (s "dump" :direction :output)
  (princ 99 s))

(setq x 'a)

(unwind-protect
     (progn (princ "What error?")
            (error "This error."))
  (setq x 'b))

;; A typical with- macro
;  Pure macro:

(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
            (progn
              (setq *db* ,db)
              (lock *db*)
              ,@body)
         (progn
           (release *db*)
           (setq *db* ,temp))))))

;; Combination of macro and function:
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gbod #'(lambda () ,@body)))
       (declare (dynamic-extent ,gbod))
       (with-db-fn *db* ,db ,gbod))))

(defun with-db-fn (old-db new-db body)
  (unwind-protect
       (progn
         (setq *db* new-db)
         (lock *db*)
         (funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))

(with-db db
  (eval-query q))


;; 11.3 Conditional Evaluation

(if t
    'phew
    (/ x 0))

 (while (not sick)
   (if3 (cake-permitted)
        (eat-cake)
        (throw 'tantrum nil)
        (plead-insistently)))

;; Macros for conditional evaluation
(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (?       ,?-case)
     (t       ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))

(mapcar #'(lambda (x)
            (nif x 'p 'z 'n))
        '(0 1 -1))

(let ((x (foo)))
  (or (eql x (bar)) (eql x (baz))))

(member (foo) (list (bar) (baz)))

;; Macros for conditional evaluation
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

;; ex
(in (foo) (bar) (baz))

(let ((#:g25 (foo)))
  (or (eql #:g25 (bar))
      (eql #:g25 (baz))))

(inq operator + - *)
;; ==>
(in operator '+ '- '*)

(member x (list a b) :test #'equal)
;; ==>
(in-if #'(lambda (y) (equal x y)) a b)

(some #'oddp (list a b))
;; ==>
(in-if #'oddp a b)

;; 11.4 Iteration
(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;; Macro for iteration by subsequences
(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc #'(lambda ,parms ,@body)
                  ,@(map0-n #'(lambda (n)
                                `(nthcdr ,n ,src))
                            (1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar #'(lambda (args)
                                    `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n #'(lambda (n)
                                         `(nth ,(1- n)
                                               ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))

;; Expansion of a call to do-tuples/c
(do-tuples/c (x y z) '(a b c d)
             (princ (list x y z)))

(let ((#:g2 '(a b c d)))
  (when (nthcdr 2 #:g2)
    (labels ((#:g4 (x y z)
               (princ (list x y z))))
      (do ((#:g3 #:g2 (cdr #:g3)))
          ((not (nthcdr 2 #:g3))
           (#:g4 (nth 0 #:g3)
                 (nth 1 #:g3)
                 (nth 0 #:g2))
           (#:g4 (nth 1 #:g3)
                 (nth 0 #:g2)
                 (nth 1 #:g2))
           nil)
        (#:g4 (nth 0 #:g3)
              (nth 1 #:g3)
              (nth 2 #:g3))))))

;; ex
(do-tuples/o (x y) '(abcd)
             (princ (list x y)))

(do-tuples/c (x y) '(abcd)
             (princ (list x y)))

(do-tuples/o (x y) points (drawline x y))

(do-tuples/c (x y) points (drawline x y))

(do-tuples/o (x) '(a b c) (princ x))

(do-tuples/c (x) '(a b c) (princ x))

(do-tuples/c (x y z) '(abcd)
             (princ (list x y z)))

(do-tuples/c (wxyz)'(abcd)
             (princ (list w x y z)))

;; 11.5 Iteration with Multiple Values

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t(cons (list (if (atom (caar rebinds))
                          'setq
                          'multiple-value-setq)
                      (caar rebinds)
                      (third (car rebinds)))
                (mvdo-rebind-gen (cdr rebinds))))))

(mvdo* ((x 1 (1+ x))
        ((y z) (values 0 0) (values z x)))
       ((> x 5) (list x y z))
       (princ (list x y z)))

(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
      ((> x 5) (list x y z))
      (princ (list x y z)))

(let ((w 0) (x 1) (y 2) (z 3))
  (mvpsetq (w x) (values 'a 'b) (y z) (values w x))
  (list wxyz))


;; A game of squash
(mvdo* (((px py) (pos player)               (move player mx my))
        ((x1 y1) (pos obj1)            (move obj1 (- px x1)
                                             (- py y1)))
        ((x2 y2) (pos obj2)            (move obj2 (- px x2)
                                             (- py y2)))
        ((mx my) (mouse-vector) (mouse-vector))
        (win       nil                 (touch obj1 obj2))
        (lose      nil                 (and (touch obj1 player)
                                            (touch obj2 player))))
    ((or win lose) (if win 'win 'lose))
  (clear)
  (draw obj1)
  (draw obj2)
  (draw player))

(shuffle '(a b c) '(1 2 3 4))
(mappend #'mklist '((a b c) d (e (f g) h) ((i)) j))

;; Multiple value version of psetq
(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar #'(lambda (p)
                           (mapcar #'(lambda (x) (gensym))
                                   (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                     ,@(mapcan #'(lambda (p s)
                                   (shuffle (mklist (car p))
                                            s))
                               pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

;; Multiple value binding version of do
(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x)
                                           (gensym))
                                       (car b))
                               (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
                      (mappend #'mklist (mapcar #'car binds))
                      (mappend #'mklist temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan #'(lambda (b)
                                 (if (third b)
                                     (list (car b)
                                           (third b))))
                             binds))
          (go ,label)))))

;; Expansion of a call to mvdo
(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
      ((> x 5) (list x y z))
      (princ (list x y z)))

;;expands into:

(let (#:g2 #:g3 #:g4)
  (mvpsetq #:g2 1
           (#:g3 #:g4) (values 0 0))
  (prog ((x #:g2) (y #:g3) (z #:g4))
     #:g1
     (if (> x 5)
         (return (progn (list x y z))))
     (princ (list x y z))
     (mvpsetq x (1+ x)
              (y z) (values z x))
     (go #:g1)))

;; 11.6 Need of Macros

(defun fnif (test then &optional else)
  (if test
      (funcall then)
      (if else (funcall else))))

;;We would protect the then and else arguments by expressing them as
;;closures, so instead of

(if (rich) (go-sailing) (rob-bank))

(fnif (rich)
      #'(lambda () (go-sailing))
      #'(lambda () (rob-bank)))

(dolist (b bananas)
  (peel b)
  (eat b))

;;has the same side-effects as

(mapc #'(lambda (b)
          (peel b)
          (eat b))
      bananas)

(defun forever (fn)
  (do ()
      (nil)
    (funcall fn)))
