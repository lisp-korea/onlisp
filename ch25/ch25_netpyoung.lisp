;; 25. Object-Oriented Lisp
;; 이번장은 Lisp에서의 Object-oriented 프로그래밍에 대해 다룸.
;; CLOS ( Common Lisp Object System)
;; lisp는 object-oriented 프로그래밍을 작성하기 위한 연산모음을 가지고 있는데 이를 통틀어 CLOS라 부름.

;; ----------------------------------------------------------------
;; 25.1 Plus ça Change
;;- "plus ca change, plus c'est la meme chose"
;;- "the more it changes, the more it's the same thing"
;;- "변해봤자 그게그거"

;;; normal ver
(defparameter a '(rectangle 10 20))
(defparameter b '(circle 10))

(defun area (x)
  (cond ((rectangle-p x)
	 (* (width x) (height x)))	; width * height
	((circle-p    x)
	 (* pi (expt (radius x) 2)))))	; pi * r^2

(defun rectangle-p (x) (eq (first x) 'rectangle))
(defun circle-p    (x) (eq (first x) 'circle))

(defun width  (x) (second x))
(defun height (x) (third x))

(defun radius (x) (second x))

(area a)
(area b)

;;; clos ver
(defclass <Rectangle> ()
  ((w :accessor w :initarg :w)
   (h :accessor h :initarg :h)))

(defclass <Circle> ()
  ((r :accessor r :initarg :r)))

(defmethod area! ((rectangle <Rectangle>))
  (with-slots (w h) rectangle
    (* w h)))

(defmethod area! ((circle <Circle>))
  (with-slots (r) circle
    (* pi (expt r 2))))

(defparameter aa (make-instance '<Rectangle> :w 10 :h 20))
(defparameter bb (make-instance '<Circle> :r 10))

(area! aa)
(area! bb)

;;- "Lisp is an object-oriented language, yes, but not because it has adopted the object-oriented model.
;;- "리스프는 객체지향언어지만, 객체지향 모델을 취했기에(has adopted) 객체지향언어가 아니다."
;; ----------------------------------------------------------------
;; 25.2 Objects in Plain Lisp
;;- 이번 단락에선, 객체지향 프로그래밍에 필요한 "최소한의 코드"를 구현할 것이다.

;;객체란
;;1. 오브젝트는 프로퍼티를 지닌다.
;;2. 메시지에 반응한다.
;;3. 부모로부터 프로퍼티와 메소드를 상속받는다.

;; Lisp에서 프로퍼티를 갖게하는 방법.
;;- 해쉬테이블 이용.
;;- 함수를 이용(함수는 데이터객체이기에, 프로퍼티를 저장할 수 있다)

(defmethod print-object ((x hash-table) stream)
  (format stream "#<~A>" (gethash 'name x)))

(defparameter *obj* (make-hash-table))
;; 프로퍼티.
(setf (gethash 'name *obj*) '*obj*)
(setf (gethash 'color *obj*) 'Green)
(gethash 'color *obj*)

;; 메소드 호출.
(setf (gethash 'move *obj*)
      #'(lambda (obj x) (format nil "~A가 ~A만큼 이동." obj x)))
(funcall (gethash 'move *obj*) *obj* 10)

;; 메시지 패싱.
(defun tell (obj message &rest args)
  (apply (gethash message obj) obj args))
(tell *obj* 'move 10)

;; 상속.
(defparameter *obj-fater* (make-hash-table))
(setf (gethash 'name *obj-fater*) '*obj-fater*)
(setf (gethash 'color *obj-fater*) 'Blue)
(setf (gethash 'money *obj-fater*) 10000)

(setf (gethash 'parent *obj*) *obj-fater*)

;; single
(defun rget-1 (obj prop)			;object, property
  (multiple-value-bind (val win) (gethash prop obj)
    (if win
	(values val win)
	(let ((parent (gethash 'parent obj)))
	  (and parent (rget-1 parent prop))))))
(rget-1 *obj* 'color)
(rget-1 *obj* 'money)

;; mother?
(defparameter *obj-mother* (make-hash-table))
(setf (gethash 'name *obj-mother*) '*obj-mother*)
(setf (gethash 'color *obj-mother*) 'Yellow)
(setf (gethash 'money *obj-mother*) 2000000)

;; (setf (gethash 'parent *obj*) (list *obj-fater* *obj-mother*)) ;parent
;; (rget-1 *obj* 'money) >> Error
(setf (gethash 'parents *obj*) (list *obj-fater* *obj-mother*)) ;parents**

;; multiple - figure 25.1
(defun rget-2 (obj prop)
  (some2 #'(lambda (x) (gethash prop x))
	 (get-ancestors obj)))

(defun get-ancestors (obj)
  "객체(obj)의 부모`를 얻어옴"
  (labels ((get-all (x)
	     (append (list x)
		     (mapcan #'get-all (gethash 'parents x)))))
    (stable-sort (delete-duplicates (get-all obj))
		 #'(lambda (x y)
		     (member y (gethash 'parents x))))))

(defun some2 (fn lst)
  "some과는 달리 반환시 values를 이용"
  (unless (atom lst)
    (multiple-value-bind (val win) (funcall fn (first lst))
      (if (or val win)
	  (values val win)
	  (some2 fn (cdr lst))))))

(get-ancestors *obj*) ; '(obj father mother)
(rget-2 *obj* 'money) ; 10000

(defparameter *obj-grand-mother* (make-hash-table))
(setf (gethash 'name *obj-grand-mother*) '*obj-grand-mother*)
(setf (gethash 'color *obj-grand-mother*) 'White)
(setf (gethash 'parents *obj-fater*) (list *obj-grand-mother*))

(get-ancestors *obj*) ; '(obj father mother grand-mother)

(setf (gethash 'parents *obj-mother*) (list *obj-grand-mother*)) ; ??!
(get-ancestors *obj*) ; '(obj father mother grand-mother)
;; ----------------------------------------------------------------
;; (setf 악당         (make-hash-table)
;;       애국자       (make-hash-table)
;;       애국적인-악당 (make-hash-table))

;; (setf (gethash 'name 악당)     '악당
;;       (gethash 'name 애국자) '애국자
;;       (gethash 'name 애국적인-악당) '애국적인-악당)

;; (setf (gethash 'serves 악당)   '자신
;;       (gethash 'serves 애국자) '국가
;;       (gethash 'parents 애국적인-악당) (list 악당 애국자))
;; (rget-2 애국적인-악당 'serves)

;; (ancestors 애국적인-악당)
;; 애국적인-악당, 악당, 애국자
;; (rget-2 애국적인-악당 'serves)
;; 자신, T
;; (defparameter 악당이지만-애국자 (obj 애국자 악당))
;; (rget-2 악당이지만-애국자 'serves)
;; 국가, T
;; ----------------------------------------------------------------
(defun make-obj (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (gethash 'parents obj) parents)
    (ancestors obj)
    obj))

(defun ancestors (obj)
  (or (gethash 'ancestors obj)
      (setf (gethash 'ancestors obj)
	    (get-ancestors obj))))

(defun rget-3 (obj prop)
  (some2 #'(lambda (x) (gethash prop x))
	 (ancestors obj)))
;; && tell(메시지 콜)의 syntax가 lisp와는 궁합이 안맞는다.
;; (defun tell (obj message &rest args)
;;    (apply (gethash message obj) obj args))
;; (tell obj 'move 10)
;; (tell (tell obj 'find-owner) 'find-owner)
;; (find-owner (find-owner obj))
(defmacro defprop (name &optional (field-or-method :field))
  `(progn
     (defun ,name (obj &rest args)
       ,(if (eq field-or-method :method)
	    `(run-methods obj ',name args)
	    `(rget-3 obj ',name)))
     (defsetf ,name (obj) (val)
       `(setf (gethash ',',name ,obj) ,val))))

(defun run-methods (obj name args)
  (let ((method (rget-3 obj name)))
    (if method
	(apply method obj args)
	(error "No ~A method for ~A." name obj))))
;;
;; (progn
;;     (setq 악당 (make-obj))
;;     (setq 애국자 (make-obj))
;;     (setq 애국적인-악당 (make-obj 악당 애국자))
;;     (defprop serves)
;;     (setf (serves 악당)   '자신)
;;     (setf (serves 애국자) '국가)
;;     (serves 애국적인-악당))
;; 자신, T
;; ----------------------------------------------------------------
;;

;; after 메소드 호출후에도 primary method의 반환값을 이용.
;; run-methods, rget 수정.
;; call-next

;; symb in Figure 4-8
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defstruct meth around before primary after) ;method

(defmacro meth- (field obj)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (and (meth-p ,gobj)
	    (,(symb 'meth- field) ,gobj)))))
;; (meth- around *obj*)
;; (LET ((#:G896 *OBJ*))
;;   (AND (METH-P #:G896) (METH-AROUND #:G896)))

(defun run-methods (obj name args)
  (let ((pri (rget obj name :primary)))
    (unless pri (error "No primary ~A method for ~A." name obj))
    (let ((around-method (rget obj name :around)))
      (if around-method
	  (apply around-method obj args)
	  (run-core-methods obj name args pri)))))

(defun run-core-methods (obj name args &optional primary-method)
  (multiple-value-prog1
      (progn (run-befores obj name args)
	     (apply (or primary-method (rget obj name :primary))  obj args))
    (run-afters obj name args)))
;; (multiple-value-prog1 3 2 1)
;; 3

(defun rget (obj prop &optional meth (skip 0))
  (some2 #'(lambda (x)
	     (multiple-value-bind (val win) (gethash prop x)
	       (when win
		 (case meth
		   (:around  (meth- around  val))
		   (:primary (meth- primary val))
		   (t        (values val win))))))
	 (nthcdr skip (ancestors obj))))

;; (cdr '((1 2) (3 4) (5 6)))      ; ((3 4) (5 6))
;; (nthcdr 1 '((1 2) (3 4) (5 6))) ; ((3 4) (5 6))
;; (nthcdr 2 '((1 2) (3 4) (5 6))) ; ((5 6))
;; (nthcdr 0 '((1 2) (3 4) (5 6))) ; ((1 2) (3 4) (5 6))

(defun run-befores (obj prop args)
  (dolist (anc (ancestors obj))
    (let ((before-method (meth- before (gethash prop anc))))
      (when before-method (apply before-method obj args)))))

(defun run-afters (obj prop args)
  (labels ((rec (lst)
	     (when lst
	       (rec (rest lst))
	       (let ((after-method (meth- after (gethash prop (first lst)))))
		 (when after-method (apply after-method (first lst) args))))))
    (rec (ancestors obj))))

;;;;;;;;;;;;;;;
(defmacro defmeth ((name &optional (type :primary))
			   obj params &body body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (defprop ,name :method)
       (unless (meth-p (gethash ',name ,gobj))
	 (setf (gethash ',name ,gobj) (make-meth)))
       (setf (,(symb 'meth- type) (gethash ',name ,gobj))
	     ,(build-meth name type gobj params body)))))

(defun build-meth (name type gobj params body)
  (let ((gargs (gensym)))
    `#'(lambda (&rest ,gargs)
	 (labels ((call-next ()		; ***
		    ,(if (or (eq type :primary) (eq type :around))
			 `(cnm ,gobj ',name (rest ,gargs) ,type)
			 '(error "Illegal call-next.")))
		  (next-p ()
		   ,(case type
			  (:around `(or (rget ,gobj ',name :around 1)
					(rget ,gobj ',name :primary)))
			  (:primary `(rget ,gobj ',name :primary 1))
			  (t nil))))
	   (apply #'(lambda ,params ,@body) ,gargs)))))

(defun cnm (obj name args type)
  (case type
    (:around (let ((around-method (rget obj name :around 1)))
	       (if around-method
		   (apply around-method obj args)
		   (run-core-methods obj name args))))
    (:primary (let ((primary-method (rget obj name :primary 1)))
		(if primary-method
		    (apply primary-method obj args)
		    (error "No next method."))))))

(defmacro undefmeth ((name &optional (type :primary)) obj)
  `(setf (,(symb 'meth- type) (gethash ',name ,obj))
	 nil))
;; 
(defparameter rectangle (make-obj))

(defprop height)
(defprop width)
(defmeth (area) rectangle (r)
  (* (height r) (width r)))
;; (LET ((#:G1945 RECTANGLE))
;;   (DEFPROP AREA :METHOD)
;;   (UNLESS (METH-P (GETHASH 'AREA #:G1945))
;;     (SETF (GETHASH 'AREA #:G1945) (MAKE-METH)))
;;   (SETF (METH-PRIMARY (GETHASH 'AREA #:G1945))
;;           #'(LAMBDA (&REST #:G1946)
;;               (LABELS ((CALL-NEXT ()
;;                          (CNM #:G1945 'AREA (REST #:G1946) :PRIMARY))
;;                        (NEXT-P ()
;;                          (RGET #:G1945 'AREA :PRIMARY 1)))
;;                 (APPLY #'(LAMBDA (R) (* (HEIGHT R) (WIDTH R))) #:G1946)))))
(let ((myrec (make-obj rectangle)))
  (setf (height myrec) 2
	(width  myrec) 3)
  (area myrec))
;; 6

;;;;;;;
(defparameter filesystem (make-obj))
(defmeth (backup :before) filesystem (fs)
  (format t "Remember to mount the tape.~%"))
(defmeth (backup)         filesystem (fs)
  (format t "Oops, delete all your files.~%") ;;;;;;
  'done)
(defmeth (backup :after) filesystem (fs)
  (format t "Well, that was easy.~%"))	;;;;;;

(backup (make-obj filesystem))

(defmeth (backup :around) filesystem (fs)
  ;; Around 메소드가 primary method를 뒤덮음.
  (time (call-next)))
;; 'xx)
;;  (list 'xx (call-next)))

(backup (make-obj filesystem))

(undefmeth (backup :around) filesystem)
(backup (make-obj filesystem))
;; ----------------------------------------------------------------
(defmacro children (obj) `(gethash 'children ,obj))

(defun get-parents (obj) (gethash 'parents obj))

(defun set-parents (obj parents)
  (dolist (parent (get-parents obj))
    (setf (children parent) (delete obj (children parent))))
  (setf (gethash 'parents obj) parents)
  (dolist (parent parents)
    (pushnew obj (children parent)))
  (maphier #'(lambda (obj) (setf (gethash 'ancestors obj)
				 (get-ancestors obj)))
	   obj)
  parents)

(defsetf parents set-parents)

(defun maphier (fn obj)
  (funcall fn obj)
  (dolist (child (children obj))
    (maphier fn child)))

(defun make-obj (&rest parents)
  (let ((obj (make-hash-table)))
    (setf (parents obj) parents)
    obj))
;;
(defmacro defcomb (name op)
  `(progn
     (defprop ,name :method)
     (setf (get ',name 'mcombine)
	   ,(case op
		  (:standard nil)
		  (:progn '#'(lambda (&rest args) (first (last args))))
		  (t      op)))))

(defun run-core-methods (obj name args &optional primary-method)
  (let ((comb (get name 'mcombine)))
    (if comb
	(if (symbolp comb)
	    (funcall (case comb
		       (:and #'comb-and)
		       (:or  #'comb-or))
		     obj name args (ancestors obj))
	    (comb-normal comb obj name args))
	(multiple-value-prog1
	    (progn (run-befores obj name args)
		   (apply (or primary-method (rget obj name :primary))
			  obj args))
	  (run-afters obj name args)))))

(defun comb-normal (comb obj name args)
  (apply comb
	 (mapcan #'(lambda (x)
		     (let* ((pm  (meth- primary (gethash name x)))
			    (val (when pm (apply pm obj args))))
		       (when val (list val))))
		 (ancestors obj))))

(defun comb-and (obj name args ancs &optional (last t))
  (if (null ancs)
      last
      (let ((pm (meth- primary (gethash name (first ancs)))))
	(if pm
	    (let ((new (apply pm obj args)))
	      (and new (comb-and obj name args (rest ancs) new)))
	    (comb-and obj name args (rest ancs) last)))))

(defun comb-or (obj name args ancs)
  (and ancs
       (let ((pm (meth- primary (gethash name (first ancs)))))
	 (or (and pm (apply pm obj args))
	     (comb-or obj name args (rest ancs))))))

;;
(defparameter citrus (make-obj))
(defparameter orange (make-obj citrus))
(defparameter my-orange (make-obj orange))

(defmeth (props) citrus    (c) '(round acidic))
(defmeth (props) orange    (c) '(orange sweet))
(defmeth (props) my-orange (c) '(dented)) ;;

(defcomb props
    #'(lambda (&rest args) (reduce #'union args)))

(props my-orange)

(defcomb props :standard)
(props my-orange)

;; ----------------------------------------------------------------
;; 25.3 Classes and Instances
;; CLOS를 이해했으니, 이 단락부터는 CLOS 그 자체에서만 말하겠음.
;; 클래스 정의.
(defclass circle ()
  (radius center))

;; 인스턴스화.
(make-instance 'circle)

(defclass circle ()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))
(setf (circle-radius (make-instance 'circle)) 2)

(defclass circle ()
  ((radius :accessor circle-radius :initarg :radius)
   (center :accessor circle-center :initarg :center)))

(circle-radius
 (make-instance 'circle :radius 2 :center '(0 . 0)))

(defclass shape ()
  ((color   :accessor shape-color   :initarg :color)
   (visible :accessor shape-visible :initarg :visible :initform t)))
(shape-visible (make-instance 'shape))
(shape-visible (make-instance 'shape :visible nil))

(defclass screen-circle (circle shape)
  nil)

(shape-color (make-instance 'screen-circle :color 'red :radius 3))

(defclass screen-circle (circle shape)
  ((color :initform 'purple)))
(shape-color (make-instance 'screen-circle))

(defclass random-dot ()
  ((x :accessor dot-x :initform (random 100))
   (y :accessor dot-y :initform (random 100))))

(mapcar #'(lambda (name)
	    (let ((rd (make-instance 'random-dot)))
	      (list name (dot-x rd) (dot-y rd))))
	'(first second third))

;; accessor and reader
(defclass owl ()
  ((nocturnal :accessor owl-nocurnal
	      :initform t
	      :allocation :class)))
(owl-nocurnal (make-instance 'owl))
(setf (owl-nocurnal (make-instance 'owl)) 'maybe)
(owl-nocurnal (make-instance 'owl))

(defclass owl ()
  ((nocturnal :reader owl-nocurnal
	      :initform t
	      :allocation :class)))
;; (setf (owl-nocurnal (make-instance 'owl)) nil)
;; error
;; ----------------------------------------------------------------
;; 25.4 Methods
;; (defprop area)   - data field
;; (defprop area t) - method

(defmethod area ((c circle))
  (* pi (expt (circle-radius c) 2)))
(area (make-instance 'circle :radius 1))
;; 3.141592653589793d0

(defmethod move ((c circle) dx dy)
  (incf (first (circle-center c)) dx)
  (incf (rest  (circle-center c)) dy)
  (circle-center c))
(move (make-instance 'circle :center '(1 . 1)) 2 3)

(defclass <악당> () nil)
(defclass <애국자> () nil)
(defclass <애국적인-악당> (<악당> <애국자>) nil)

(defmethod 자신이냐-국가냐? ((악당 <악당>)) '자신)
(defmethod 자신이냐-국가냐? ((애국자 <애국자>)) '국가)
(자신이냐-국가냐? (make-instance '<애국적인-악당>))
;; 자신

;; CLOS에서는 하나 "이상"의 인자가 특수화(be specialized)될 수 있다.

(defclass stuff ()
  ((name :accessor name :initarg :name)))

(defclass ice-cream (stuff)
  nil)

(defclass topping (stuff)
  nil)

(defmethod combine ((ic ice-cream) (top topping) &optional (where :here))
  `(,(name ic) ice-cream with ,(name top) topping in a
     ,(case `,where
	    (:here 'glass)
	    (:to-go 'styrofoam))
     dish))

(combine (make-instance 'ice-cream :name 'fig)
	 (make-instance 'topping :name 'olive)
	 :here)
;; 디스패칭은 첫번째 인자의 타입에 기반한다.

(defmethod combine ((s1 string) (s2 string) &optional int?)
  (let ((str (concatenate 'string s1 s2)))
    (if int?
	(intern str)
	str)))

(combine "I am not a " "cook.")

;; 두번째 이상부터는 타입이랑 별 상관없어도된다.
;; 자기가 원하는 대로.
(defmethod combine ((s1 sequence) (x (eql 'palindrome))
		    &optional (length :odd))
  (concatenate (type-of s1)
	       s1
	       (subseq (reverse s1)
		       (case length (:odd 1) (:even 0)))))
(combine '(able was i ere) 'palindrome)

;; defgeneric으로 generic 함수를 명시적으로(explicitly) 정의할 수 있다.
(defgeneric combine (x y &optional z)
  (:method (x y &optional z)
    "I can't combine these arguments.")
  (:documentation "Combines thins."))
(combine #'expt "chocolate")

;; 인자 순서를 맞춰줘야함.
;; (x y &rest z) | (a b &rest c)
;; &rest는 &key로 대체할 수 도 있음.
;; (x y &rest z) | (a b &key c d)

(defmethod combine ((x string) (y string) &optional ignore)
  ;; overwrite할 수 있다.
  (concatenate 'string x " + " y))
(combine "asdf" "Asdf")
;; 재 정의말고 지우고자할때, CLOS에는 지우는게 없으니
;; 매크로를 이용하여 undefmethod를 정의.
(defmacro undefmethod (name &rest args)
  (if (consp (first args))
      (udm name nil (first args))
      (udm name (list (first args)) (second args))))

(defun udm (name qual specs)
  (let ((classes (mapcar #'(lambda (x) `(find-class ',x))
			 specs)))
    `(remove-method (symbol-function ',name)
		    (find-method (symbol-function ',name)
				 ',qual
				 (list ,@classes)))))

(undefmethod combine (string string))
(combine "asdf" "Asdf")

;; 전체다 날려버리려면 fmakunbound 함수를 써라.
(fmakunbound 'combine)
(combine "asdf" "Asdf")
;; ----------------------------------------------------------------
;; 25.5 Auxiliary Method and Combination
(defclass speaker ()
  nil)

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker)
       "life is not what it used to be")

(defclass intellectual (speaker)
  nil)

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after  ((i intellectual) string)
  (princ " in some sense"))

(speak (make-instance 'intellectual)
       "life is not what it used to be")

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(speak (make-instance 'intellectual)
       "life is not what it used to be")
;;
(defclass courtier (speaker)
  nil)

(defmethod speak :around ((c courtier) string)
  (format t "Does the King believe that ~A? " string)
  (if (eq (read) 'yes)
      (when (next-method-p) (call-next-method))
      (format t "Indeed, it is a preprosterous idea.~%"))
  'bow)

(speak (make-instance 'courtier) "kings will last")

(speak (make-instance 'courtier) "the world is round")

;; "operator method combination"에서,
;; around-method는 여전히 "call-next-method"를 통해 다음 method를 호출할 수 있다.;; 허나, primary methods는 더이상 "call-next-method"를 사용할 수 가 없다.
;; (앞서나온, "call-next"와는 다르다.)

(defgeneric price (x)
  (:method-combination +))

(defclass jacket   () nil)
(defclass trousers () nil)
(defclass suit     (jacket trousers) nil)

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)

(price (make-instance 'suit))
;; + and append list max min nconc or progn

;; ----------------------------------------------------------------
;; 25.6 CLOS and Lisp
;; - 1. 임베디드 언어는, 기존 환경과 잘 들어맞는다 (따라서, 기존 코드와 동일한 맥락을 유지하면서 생각할 수 있다).
;; - 2. 임베디드 언어는, 기존 코드의 노하우를 기반으로한 모든 것들을 가져다 쓸 수 있어, 강력하다.

;; CLOS는 embeded language의 좋은 예.
;; 비록 CLTL2에서 뚜렷히 언급하진 않았지만, CLOS method들은 lexical closure의 파워를 가지고 있다.
;; method아래에 closure가 감추어졌다

;; (defclass <계좌> ()
;;   ((잔액 :accessor 잔액 :initarg :잔액)))
;; (let ((거래횟수 0))
;;   (defun 거래횟수 () 거래횟수)
;;   (defmethod 출금 ((계좌 <계좌>) 거래액)
;;     (incf 거래횟수)
;;     (decf (잔액 계좌) 거래액))
;;   (defmethod 입금 ((계좌 <계좌>) 거래액)
;;     (incf 거래횟수)
;;     (incf (잔액 계좌) 거래액)))
;; (defparameter 내계좌 (make-instance '<계좌> :잔액 1000))
;; (출금 내계좌 100)
;; (입금 내계좌 100)
;; (거래횟수)

;; ----------------------------------------------------------------
;; 25.7 When to Object
;; 객체지향 스타일은 여러가지로 해택을 가져다줬다.
;; 근데, lisp은 표현력이 풍부한 언어라서 딱히 없어도된다.

;; CLOS로 Common Lisp는 가장 강력한 객체지향언어가 되었다.
;; 아이러니하게, 객체지향 프로그래밍이 거의 필요치 않는 언어이기도 하다.


;; 참조 site.
;; http://www.english4u.kr/lisp/clos.html
