(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))
;; from ch 20...
(setq *cont* #'identity)

(defmacro =lambda (parms &body body) 
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))
 
(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

(=defun message ()
  (=values 'hello 'there))

(=defun baz ()
  (=bind (m n) (message)
	(=values (list m n))))

;; (PROGN
;;  (DEFMACRO BAZ () `(=BAZ *CONT*))
;;  (DEFUN =BAZ (*CONT*) (LET ((*CONT* #'(LAMBDA (M N) (FUNCALL *CONT* (LIST M N)))))
;;                         (MESSAGE))))

(baz)


;; from ch 22

(defparameter *paths* nil)
(defconstant failsym '@)

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
 
(defun cb (fn choices)
  (if choices
     (progn
       (if (cdr choices)
           (push #'(lambda () (cb fn (cdr choices)))
                 *paths*))
       (funcall fn (car choices)))
     (fail)))
 
(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))


(=defun two-numbers()
  (choose-bind n1 '(0 1 2 3 4 5)
	(choose-bind n2 '(0 1 2 3 4 5)
	  (=values n1 n2))))

;; (PROGN
;;  (DEFMACRO TWO-NUMBERS () `(=TWO-NUMBERS *CONT*))
;;  (DEFUN =TWO-NUMBERS (*CONT*)
;;    (CB #'(LAMBDA (N1) (CB #'(LAMBDA (N2) (=VALUES N1 N2)) '(0 1 2 3 4 5)))
;;        '(0 1 2 3 4 5))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
	(if (= (+ n1 n2) sum)
		`(the sum of ,n1 ,n2)
		(fail))))

;; (PROGN
;;  (DEFMACRO PARLOR-TRICK (SUM) `(=PARLOR-TRICK *CONT* ,SUM))
;;  (DEFUN =PARLOR-TRICK (*CONT* SUM)
;;    (LET ((*CONT*
;;           #'(LAMBDA (N1 N2) (IF (= (+ N1 N2) SUM) `(THE SUM OF ,N1 ,N2) (FAIL)))))
;;      (=TWO-NUMBERS *CONT*))))

;; CL-USER> (parlor-trick 7)
;; (THE SUM OF 2 5)
;; CL-USER> (fail)
;; (THE SUM OF 3 4)
;; CL-USER> (fail)
;; (THE SUM OF 4 3)
;; CL-USER> (fail)
;; (THE SUM OF 5 2)
;; CL-USER> (fail)
;; @
;; CL-USER> 



;; Ch 23. Parsing with ATNs
;; ATN = Augmented Transition Networks


;; (defnode s
;; 	(cat noun s2
;; 		 (str subj *)))
;; (defnode s2
;; 	(cat verb s3
;; 		 (setr v *)))
;; (defnode s3
;; 	(up `(sentence
;; 		  (subject ,(getr subj))
;; 		  (verb ,(getr v)))))


;; 노드를 정의
(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))
 
;; 하위 노드로 내려감
(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
     (,next pos ,(compile-cmds cmds))))

;; 유형에 해당하는 경우 다음 단어로 이동
(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail) ;; 다른 선택지로...
       (let ((* (nth pos *sent*)))
         (if (member ',cat (types *))
             (,next (1+ pos) ,(compile-cmds cmds))
             (fail)))))

;; 건너뜀
(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))


(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

;; test...
(compile-cmds '((setr a b)
				(progn (princ "ek!"))
				(setr c d)))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs))))

(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result) result (car result))))

(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs)) 
         (cdr ,regs)))

(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))

(defmacro pushr (key val regs)
  `(set-register ',key
                 (cons ,val (cdr (assoc ',key (car ,regs))))
                 ,regs))

(defmacro with-parses (node sent &body body)
  (with-gensyms (pos regs)
    `(progn
       (setq *sent* ,sent)
       (setq *paths* nil)
       (=bind (parse ,pos ,regs) (,node 0 '(nil))
         (if (= ,pos (length *sent*))
             (progn ,@body (fail))
             (fail))))))

(defun types (w)
  (cdr (assoc w '((spot noun)
				  (runs verb)))))

(defnode s
	(cat noun s2
		 (setr subj *)))
;; (PROGN
;;  (DEFMACRO S (POS REGS) `(=S *CONT* ,POS ,REGS))
;;  (DEFUN =S (*CONT* POS REGS) (PROGN
;;                               (IF (= (LENGTH *SENT*) POS) (FAIL)
;;                                   (LET ((* (NTH POS *SENT*)))
;;                                     (IF (MEMBER 'NOUN (TYPES *)) (S2 (1+ POS) (SETR SUBJ * REGS)) (FAIL)))))))

(defnode s2
	(cat verb s3
		 (setr v *)))
;; (PROGN
;;  (DEFMACRO S2 (POS REGS) `(=S2 *CONT* ,POS ,REGS))
;;  (DEFUN =S2 (*CONT* POS REGS) (PROGN (CAT VERB S3 (SETR V *)))))

(defnode s3
	(up `(sentence
		  (subject ,(getr subj))
		  (verb ,(getr v)))))
;; (PROGN
;;  (DEFMACRO S3 (POS REGS) `(=S3 *CONT* ,POS ,REGS))
;;  (DEFUN =S3 (*CONT* POS REGS)
;;    (PROGN (UP `(SENTENCE (SUBJECT ,(GETR SUBJ)) (VERB ,(GETR V)))))))

(with-parses s'(spot runs)
  (format t "Parsing: ~A~%" parse))
;; (PROGN
;;  (SETQ *SENT* '(SPOT RUNS))
;;  (SETQ *PATHS* NIL)
;;  (LET ((*CONT*
;;         #'(LAMBDA (PARSE #:G1412 #:G1413)
;;             (IF (= #:G1412 (LENGTH *SENT*))
;;                 (PROGN (FORMAT T "Parsing: ~A~%" PARSE) (FAIL)) (FAIL)))))
;;    (S 0 '(NIL))))


(defun types (word)
  (case word 
    ((do does did) '(aux v))
    ((time times) '(n v))
    ((fly flies) '(n v))
    ((like) '(v prep))
    ((liked likes) '(v))
    ((a an the) '(det))
    ((arrow arrows) '(n))
    ((i you he she him her it) '(pron))))

;; sub-network for strings of modifiers
(defnode mods
  (cat n mods/n
    (setr mods *)))

(defnode mods/n
  (cat n mods/n
    (pushr mods *))
  (up `(n-group ,(getr mods))))

;; test...
(with-parses mods '(time arrow)
  (format t "Parsing: ~A~%" parse))


;; Noun phrase sub-network
(defnode np
  (cat det np/det
    (setr det *))
  (jump np/det
    (setr det nil))
  (cat pron pron
    (setr n *)))

(defnode pron
  (up `(np (pronoun ,(getr n)))))

(defnode np/det
  (down mods np/mods
    (setr mods *))
  (jump np/mods
    (setr mods nil)))

(defnode np/mods
  (cat n np/n
    (setr n *)))

(defnode np/n
  (up `(np (det ,(getr det))
           (modifiers ,(getr mods))
           (noun ,(getr n))))
  (down pp np/pp
    (setr pp *)))

(defnode np/pp
  (up `(np (det ,(getr det))
           (modifiers ,(getr mods))
           (noun ,(getr n))
           ,(getr pp))))

;; test
(with-parses np '(it)
  (format t "Parsing: ~A~%" parse))


;; Prepositional phrase sub-networks
(defnode pp
  (cat prep pp/prep
    (setr prep *)))

(defnode pp/prep
  (down np pp/np
    (setr op *)))
 
(defnode pp/np
  (up `(pp (prep ,(getr prep))
           (obj ,(getr op)))))

;; test
(with-parses np '(arrows)
  (pprint parse))


(with-parses np '(a time fly like him)
  (pprint parse))


;; Sentence network
(defnode s
  (down np s/subj
    (setr mood 'decl)
    (setr subj *))
  (cat v v
    (setr mood 'imp)
    (setr subj '(np (pron you)))
    (setr aux nil)
    (setr v *)))

(defnode s/subj
  (cat v v
    (setr aux nil)
    (setr v *)))

(defnode v
  (up `(s (mood ,(getr mood))
          (subj ,(getr subj))
          (vcl (aux ,(getr aux))
               (v ,(getr v)))))
  (down np s/obj
    (setr obj *)))

(defnode s/obj
  (up `(s (mood ,(getr mood))
          (subj ,(getr subj))
          (vcl (aux ,(getr aux))
               (v ,(getr v)))
          (obj ,(getr obj)))))


(with-parses s '(time flies like an arrow)
  (pprint parse))

(with-parses s '(flies)
  (pprint parse))
