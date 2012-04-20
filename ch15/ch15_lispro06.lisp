;15. Macros Returning Functions


;Chapter 5 showed how to write functions which return other
;functions. Macros make the task of combining operators much easier. This
;chapter will show how to use macros to build abstractions which are
;equivalent to those defined in Chapter 5, but cleaner and more efficient.



;5장에서는 다른 함수를 반환하는 함수를 작성하는 법을 다뤘다. 매크로는 오퍼레이터(연산자)들을 더 쉽게 조합하는 일을 한다. 이 장에서는 5장에서 정의되었지만, 더 명확하고 효율적으로 동등하게 추상화하는 매크로를 사용하는 법을 보여준다.




;15.1 Building Functions



;If f and g are functions, then |f #g(x) = f (g(x))|. Section 5.4 showed how
;to implement the |#| operator as a Lisp function called compose:



;f와 g가 함수일 때, |f #g(x) = f (g(x))| 다. 5.4에서는 compose라 불리는 Lisp 함수로의 |#| 연산자를 구현하는 법을 보여줬다.

;; 66p figure 5.3
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
    #'identity))


(funcall (compose #'list #'1+) 2)
;(3)



;In this section, we consider ways to define better function builders with
;macros.  Figure 15.1 contains a general function-builder called fn, which
;builds compound functions from their descriptions. Its argument should be
;an expression of the form (operator . arguments). The operator can be the
;name of a function or macro---or compose, which is treated specially. The
;arguments can be names of functions or macros of one argument, or
;expressions that could be arguments to fn. For example,



;이 섹션에서는, 매크로를 이용해 더 좋은 함수 빌더를 정의하는 방법을 생각한다. 15.1은 fn이라 불리는 일반적인 함수 빌더를 포함하는데, 서술에 복합적인 함수를 만든다. 그것의 인자는 (operator . arguments) 구조로 표현된다. 연산자는 함수나 매크로- 또는 compose-의 이름이 될 수 있고, 특별하게 취급된다. 인자들은 한 인자의 매크로나 함수의 이름이 되거나, fn으로의 인자들이 되는 표현식일 수 있다. 예를 들어,  

(fn (and integerp oddp))

(funcall (fn (and integerp oddp)) 3)
;T

;yields a function equivalent to

;다음과 같은 함수를 유도한다.


;figure 15.1
#'(lambda (x) (and (integerp x) (oddp x)))




(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
    (if (eq (car expr) 'compose)
        (build-compose (cdr expr))
      (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                           `(,(rbuild f) ,g))
                       fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                      (if fns
                          `(,(rbuild (car fns))
                            ,(rec (cdr fns)))
                        g)))
          (rec fns)))))


;If we use compose as the operator, we get a function representing the
;composition of the arguments, but without the explicit funcalls that were
;needed when compose was defined as a function. For example,


;만일 우리가 연산자로서 compose를 사용하면, 인자들의 조합을 표현하는 함수를 얻을 수 있지만, 함수로써 조합이 정의될 때, 필요한 명시적이지 않은 funcall들을 포함하지 않을 수 있다.



(fn (compose list 1+ truncate))

(funcall (fn (compose list 1+ round)) 4.6)
;(4)
;truncate
;ceilling
;round

;expands into:


;다음과 같이 확장된다.



#'
(lambda (#:g1) (list (1+ (truncate #:g1))))



;which enables inline compilation of simple functions like list and 1+. The
;fn macro takes names of operators in the general sense; lambda-expressions
;are allowed too, as in



;list와 1+로 간단한 함수들의 인라인 컴파일이 가능하다. fn 매크로는 일반적으로 연산자의 이름을 취한다; 람다 표현식도 마찬가지다.



(fn (compose (lambda (x) (+ x 3)) truncate))


;which expands into


;다음과 같이 확장된다.



#'
(lambda (#:g2) ((lambda (x) (+ x 3)) (truncate #:g2)))



;Here the function expressed as a lambda-expression will certainly be
;compiled inline, whereas a sharp-quoted lambda-expression given as an
;argument to the function compose would have to be funcalled.


;여기에 람다 표현식으로 표현된 함수는 인라인으로 명확히 컴파일되고, 샵-쿼터 람다 표현식은 funcall되어야 하는 compose함수로 인자를 전달한다.


;Section 5.4 showed how to define three more function builders: fif, fint,
;and fun. These are now subsumed in the general fn macro. Using and as the
;operator yields the intersection of the operators given as arguments:



;5.4는 3개 이상의 함수 빌더를 어떻게 정의하는지 보여준다:fif, fint, fun. 이것들은 일반 fn 매크로에 포함되어 있다. 연산자를 사용하고 이용하여 인자들로 제공될 때 연산자들의 교차점을 유도한다.



(mapcar (fn (and integerp oddp))
'(c 3 -4 6))
;(NIL T NIL NIL)


;while or yields the union:


;통합을 유도한다.


(mapcar (fn (or integerp symbolp))
'(c 3 p 0.2))
;(T T T NIL)


;and if yields a function whose body is a conditional:



;body의 함수 유도는 조건적이다.
;;;;;;;;;;;;;;;;;;;;
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
;;;;;;;;;;;;;;ch4 4.6 mapping functions.

(map1-n (fn (if oddp 1+ identity)) 6)
;(2 2 4 4 6 6)
;identity
;(identity 39)

;However, we can use other Lisp functions besides these three:


;그러나, 우리는 Lisp 함수들을 사용할 수 있다.



(mapcar (fn (list 1- identity 1+))
'(1 2 3))
;((0 1 2) (1 2 3) (2 3 4))


;and the arguments in the fn expression may themselves be expressions:


;그리고 fn 표현에서 인자들은 자체로 표현이 되기도 한다.


(remove-if (fn (or (and integerp oddp)
(and consp cdr)))
'(1 (1) c (d) 2 3.4 (e f g)))
;(C (D) 2 3.4)

;(cdr '(a b))



;Making fn treat compose as a special case does not make it any more
;powerful.  If you nest the arguments to fn, you get functional
;composition. For example,



;만들어진 fn은 더 이상 파워풀하지 않게 만들지 않는 특별한 경우로 compose를 취급한다.



(fn (list (1+ truncate)))


;expands into:

;다음으로 확장된다.


#'(lambda (#:g1)
    (list ((lambda (#:g2) (1+ (truncate #:g2))) #:g1)))


;which behaves like

;다음과 같이 된다.

(compose #'list #'1+ #'truncate)



;The fn macro treats compose as a special case only to make such calls
;easier to read.


;fn 매크로는 더 읽기 쉽게 호출하는 특별한 상황으로 compose로 다룬다.


;15.2 Recursion on Cdrs


;Sections 5.5 and 5.6 showed how to write functions that build recursive
;functions.  The following two sections show how anaphoric macros can
;provide a cleaner interface to the functions we defined there.



;5.5와 5.6은 재귀함수를 만드는 함수들을 어떻게 작성하는지 보여줬다. 다음 두 섹션은 전방조응 매크로들이 우리가 정의했던 함수들에 깔끔한 인터페이스를 제공할 수 있다.

;Section 5.5 showed how to define a flat list recurser builder called
;lrec. With lrec we can express a call to:



;5.5에서는 lrec 이라고 불리는 균일한 리스트 recurser 빌더를 정의하는 것을 보여줬다. lrec으로 우리는 호출을 표현할 수 있다.

(defun lrec (rec &optional base)
  (labels ((self (lst)
		 (if (null lst)
		     (if (functionp base)
			 (funcall base)
		       base)
		   (funcall rec (car lst)
			    #'(lambda ()
				(self (cdr lst)))))))
    #'self))

;Figure 5.5: Function to define flat list recursers.

(defun our-every (fn lst)
  (if (null lst)
      t
    (and (funcall fn (car lst))
         (our-every fn (cdr lst)))))


;for e.g. oddp as:


;예. oddp


(lrec #'(lambda (x f) (and (oddp x) (funcall f)))
      t)

(funcall (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)
      '(1 3))


;Here macros could make life easier. How much do we really have to say to
;express recursive functions? If we can refer anaphorically to the current
;car of the list (as it) and the recursive call (as rec), we should be able
;to make do with something like:



;여기 매크로들은 삶을 더 쉽게 한다. 우리가 정말 재귀 함수들을 표현해야 하는건 얼마나 할까? 만일 우리가 전방조응적인 리스트의 현재 car와 재귀 호출(rec 로)을 참고할 수 있다면, 우리는 다음과 같이 만들 수 있다.


(alrec (and (oddp it) rec) t)



;Figure 15.2 contains the definition of the macro which will allow us to say
;this.



;15.2는 이처럼 표현하도록 매크로의 정의를 포함한다.



(funcall (alrec (and (oddp it) rec) t)
'(1 3 5 6))
;T



(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro alrec (rec &optional base)
  "cltl1 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (labels ((rec () (funcall ,gfn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

;figure 15.2 Macros for list recursion.




;The new macro works by transforming the expression given as the second
;argument into a function to be passed to lrec. Since the second argument
;may refer anaphorically to it or rec, in the macro expansion the body of
;the function must appear within the scope of bindings established for these
;symbols.



;새로운 매크로는 lrec를 통과된 함수로 두번째 인자로서 주워진 표현을 변환하는 작업을 한다. 두번째 인자는 아마도 그것이나 rec으로 전방조응적 참조를 한다. 매크로 표현에서 함수의 내용은 반드시 이들 심볼들을 위해 정립된 바인딩의 범위 내에서 보여져야 한다.





;Figure 15.2 actually has two different versions of alrec. The version used
;; in the preceding examples requires symbol macros (Section 7.11). Only
;; recent versions of Common Lisp have symbol macros, so Figure 15.2 also
;; contains a slightly less convenient version of alrec in which rec is
;; defined as a local function. The price is that, as a function, rec would
;; have to be enclosed within parentheses:



;; 15.2는 alrec 의 두가지 다른 버전들이 있다. 버전은 심볼 매크로들(7.11 섹션)을 요구하는 예제들의 진행에서 사용된다. 커먼 리습의 현재 버전에만 심볼 매크로들이 있다. 그래서 15.2 또한 alrec의 약간 덜 편한 버전을 포함하고, rec은 지역 함수로서 정의된다. 값은 함수로써 rec은 괄호안에 감싸져야 할 것이다.


(alrec (and (oddp it) (rec)) t)



;The original version is preferable in Common Lisp implementations which
;provide symbol-macrolet.



;원래버전은 symbol-macrolet을 제공하는 커먼 리습 구현에서 바람직하다.




;Common Lisp, with its separate name-space for functions, makes it awkward
;to use these recursion builders to define named functions:

;함수들을 위한 그것의 분할 이름-공간이 있는 커먼 리습은 명명된 함수들을 정의하는 그것의 재귀 빌더를 사용하여 보기 않좋게 된다.





(setf (symbol-function 'our-length)
      (alrec (1+ rec) 0))




(defun our-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))

(our-copy-list '(a g 1 4 5))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))

(our-remove-duplicates '(1 1 1 3 4 5))

(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it rec) nil lst))

(our-find-if 'symbolp '(4 b e))
;조건에 맞는 인자가 나오면 리턴하며 종료함.

(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

(our-some 'evenp '(3 1 5))
;하나의 인자라도 조건에 맞으면, T를 리턴한다.

;figure 15.3 Common Lisp functions defined with on-cdrs.



;The final macro in Figure 15.2 is intended to make this more
;abstract. Using on-cdrs we could say instead:

;15.2의 마지막 매크로는 더 추상적이 되도록 의도된다. on-cdrs를 사용해 우리는 다르게 표현한다.



(defun our-length (lst)
  (on-cdrs (1+ rec) 0 lst))

(defun our-every (fn lst)
  (on-cdrs (and (funcall fn it) rec) t lst))

(our-every 'oddp '(1 3 5 6))
;모든 인자가 참일 경우 T 리턴
(some 'evenp '(2 4 3))
(every 'oddp '(1 3 5))

(our-length '(2 3 4))
(length '(1 v d 4))
;Figure 15.3 shows some existing Common Lisp functions defined with the new
;macro. Expressed with on-cdrs, these functions are reduced to their most
;basic form, and we notice similarities between them which might not
;otherwise have been apparent.



;15.3은 새로운 매크로내의 정의된 커먼 리습 함수들의 존재를 보여준다. on-cdrs로 표현된 이들 함수는 가장 단순한 형태로 줄여진다. 그리고 우리는 명백하지 않을 수 있는 함수들과의 유사한 것들을 공지한다. 




;; Figure 15.4 contains some new utilities which can easily be defined with
;; on-cdrs. The first three, unions, intersections, and differences implement
;; set union, intersection, and complement, respectively. Common Lisp has
;; built-in functions for these operations, but they can only take two lists
;; at a time.  Thus if we want to find the union of three lists we have to
;; say:



;15.4는 on-cdrs로 더 쉽게 정의될 수 있는 새로운 유틸리티들을 포함한다. 처음 세계, unions, intersections, differences는 각각 합집합, 교집합, 차집합을 구성한다. 커먼 리습은 이들 연산을 위해 내장된 함수가 있지만 한 번에 두 리스트만 취할 수 있다. 그러므로 우리가 세 리스트의 합집합을 찾기 원하면, 다음과 같이 해야 한다.



(union '(a b) (union '(b c) '(c d)))
;(A B C D)


;The new unions behaves like union, but takes an arbitrary number of
;arguments, so that we could say:


;새로운 합집합은 union과 같이 행동하지만, 인자들의 임의의 수를 취하므로, 다음과 같이 해야 한다.




(union '(a b) '(b c))
(unions '(a b) '(b c) '(c d))
;(D C A B)



(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec) (car sets) (cdr sets))))

(intersections '(1 3 5) '(3 5) '(1 2 3))
(intersection '(1 3 5) '(2 4 6 5) :key (lambda (a) (when (evenp a) (+ 1 a))))


(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))
;figure 15.4 New utilities defined with on-cdrs.
(maxmin '(1 3 5 7 2 4 6))


;Like union, unions does not preserve the order of the elements in the
;initial lists.


;union과 같이, unions는 초기화된 리스트들에서 엘레멘트의 순서를 보존하지 않는다.


;The same relation holds between the Common Lisp intersection and the more
;general intersections. In the definition of this function, the initial test
;for null arguments was added for efficiency; it short-circuits the
;computation if one of the sets is empty.



;같은 관계는 커먼 리습 intersection과 더 일반적인 intersections 사이에서 일어난다. 이 함수의 정의로, null 인자를 위한 초기화된 테스트는 효율성을 위해 추가된다; 집합의 하나가 비어있다면 그것은 계산을 단락한다.



;;Common Lisp also has a function called set-difference, which takes two
;;lists and returns the elements of the first which are not in the second:


;커먼 리습은 또한 set-difference라 불리는 함수가 있는데, 두 리스트들을 취해 두번째가 아닌 첫번째의 엘리먼트의 리턴한다.




(set-difference '(a b c d) '(a c))
;(D B)


;Our new version handles multiple arguments much as - does. For example,
;(differences x y z) is equivalent to (set-difference x (unions y z)),
;though without the consing that the latter would entail.

;새로운 버전은 다중 인자를 핸들링한다. 예를들어, (differences x y z)는 (set-difference x (unions y z))와 같은데 수반하는 후자의 cons가 없다.




(differences '(a b c d e) '(a f) '(d))
;(B C E)



;; These set operators are intended only as examples. There is no real need
;; for them, because they represent a degenerate case of list recursion
;; already handled by the built-in reduce. For example, instead of


;이 집합 연산자들은 예제로만 의도되었다. 그들을 위한 실제 필요는 없는데, 내장된 reduce에 의해 이미 핸들링 된 리스트 재귀의 타락한 경우를 표현하기 때문이다. 예를 들면 코드 대신에


;(unions ...)

;you might as well say just

;다음과 같이 표현할 수 있다.


;((lambda (&rest args) (reduce #'union args)) ...)


;In the general case, on-cdrs is more powerful than reduce, however.



;일반적인 경우, on-cdrs는 reduce보다 더 강력할 수 있다.

;; Because rec refers to a call instead of a value, we can use on-cdrs to
;; create functions which return multiple values. The final function in Figure
;; 15.4, maxmin, takes advantage of this possibility to find both the maximum
;; and minimum elements in a single traversal of a list:



;왜냐하면 rec은 값 대신 호출을 참조하므로, 우리는 on-cdrs를 다중 값을 리턴하는 함수들을 만드는데 사용할 수 있다. 15.4의 마지막 함수인 maxmin은 리스트의 단일 탐색에서 최대값과 최소값을 찾는게 가능성의 유리함이 있다.



(maxmin '(3 4 2 8 5 1 6 7))
;8, 1


;It would also have been possible to use on-cdrs in some of the code which
;appears in later chapters. For example, compile-cmds (page 310)


;다음 장에서 나타나는 코드의 몇몇은 on-cdrs를 사용하여 보여질 수 있다. 예를 들어 compile-cdms (310 쪽)


(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
    `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

;could have been defined as simply:


;는 간단히 정의된다.


(defun compile-cmds (cmds)
  (on-cdrs `(,@it ,rec) 'regs cmds))

;;15.3 Recursion on Subtrees

;; What macros did for recursion on lists, they can also do for recursion on
;; trees.  In this section, we use macros to define cleaner interfaces to the
;; tree recursers defined in Section 5.6.


;리스트들에서 재귀를 위해 매크로가 하는 것은, 그들이 트리들에서 재귀들을 위해 할 수도 있다. 이 장에서는, 우리가 5.6에 정의된 트리 recursers로 명확히 인터페이스하는 매크로를 사용한다.


;In Section 5.6 we defined two tree recursion builders, ttrav,which always
;; traverses the whole tree, and trec which is more complex, but allows you to
;; control when recursion stops. Using these functions we could express
;; our-copy-tree

;5.6에서 우리는 두개의 재귀 빌더를 정의했다. ttrav로 항상 모든 트리를 탐색하고 trec은 더 복잡하지만 재귀가 멈출 때 사용자에게 제어되도록 허용된다. 이들 함수를 사용하면 우리는 our-copy-tree를 표현할 수 있다.
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
		 (if (atom tree)
		     (if (functionp base)
			 (funcall base tree)
		       base)
		   (funcall rec (self (car tree))
			    (if (cdr tree)
				(self (cdr tree)))))))
    #'self))
;; p74 figure 5.8 function for recursion on trees
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		   base)
	       (funcall rec tree
			#'(lambda ()
			    (self (car tree)))
			#'(lambda ()
			    (if (cdr tree)
				(self (cdr tree))))))))
    #'self))
;; p75 figure 5.10 function for recursion on trees

(defun our-copy-tree (tree)
  (if (atom tree)
      tree
    (cons (our-copy-tree (car tree))
          (if (cdr tree) (our-copy-tree (cdr tree))))))

;as


;이로써


(ttrav #'cons)


;and a call to rfind-if


;rfind-if를 호출하고

(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
    (or (rfind-if fn (car tree))
        (and (cdr tree) (rfind-if fn (cdr tree))))))


;for e.g. oddp as:


;예를 들어 oddp는

(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)))

;; Anaphoric macros can make a better interface to trec, as they did for lrec
;; in the previous section. A macro sufficient for the general case will have
;; to be able to refer anaphorically to three things: the current tree, which
;; we'll call it, the recursion down the left subtree, which we'll call left,
;; and the recursion down the right subtree, which we'll call right. With
;; these conventions established, we should be able to express the preceding
;; functions in terms of a new macro thus:



;전방조응 매크로는 이 전 장에서 lrec을 위해 했듯이, trec으로 더 좋게 인터페이스 하도록 만들 수 있다. 일반적인 상황에서 더 풍부한 매크로는 세가지로 전방조응적인 참조를 할 수 있다: 호출할 현재 트리, left를 호출하는 왼쪽 하위 트리로 내려가는 재귀, right를 호출하는 오른쪽 트리로 내려가는 재귀. 만들어진 이 약속들로 우리는 새로운 매크로의 용어에서 함수를 진행시키는 표현이 가능하다.


(atrec (cons left right))

(atrec (or left right) (and (oddp it) it))


;Figure 15.5 contains the definition of this macro.


;15.5는 이 매크로의 정의를 포함한다.




;; In versions of Lisp which don't have symbol-macrolet,we can define atrec
;; using the second definition in Figure 15.5. This version defines left and
;; right as local functions, so our-copy-tree would have to be expressed as:


;symbol-macrolet이 없는 리습 버전에서 우리는 15.5의 두번째 정의를 이용해 atrec을 정의할 수 있다. 이 버전은 지역 함수들로서 left와 right를 정의하므로 our-copy-tree는 다음과 같이 표현한다.

(atrec (cons (left) (right)))


;For convenience, we also define a macro on-trees, which is analogous to
;on-cdrs from the previous section. Figure 15.6 shows the four functions
;from Section 5.6 defined with on-trees.

;편의상, 우리는 on-tree 매크로를 정의하는데, 이전 장의 on-cdrs와 유사하다. 15.6은 on-trees로 정의된 5.6으로 부터 4개의 함수들을 보여준다.


;; As noted in Chapter 5, functions built by the recurser generators defined
;; in that chapter will not be tail-recursive. Using on-cdrs or on-trees to
;; define a function will not necessarily yield the most efficient
;; implementation. Like the underlying trec and lrec, these macros are mainly
;; for use in prototypes and in parts of a program where efficiency is not
;; paramount. However, the underlying idea of this chapter and Chapter 5 is
;; that one can write function generators and put a clean macro interface on
;; them. This same technique could equally well be used to build function
;; generators which yielded particularly efficient code.


;5장에서 주목한, 그 장에서 정의된 recurser 생성자에 의해 만들어진 함수들은  꼬리 재귀를 하지 않을 것이다. 함수를 정의한 on-cdrs 나 on-trees는 가장 효율적인 구현을 필히 끌어내지는 않는다. trec 과 lrec 아래있는 것 처럼, 이들 매크로들은 대부분 프로토타입들과 효율성이 최고가 아닌 프로그램의 부분에서 사용된다. 그러나, 이 장과 5장의 깔려진 아이디어는 그 거이 함수 생성자들을 쓸 수 있고, 그들에게 깔끔한 매크로 인터페이스를 줄 수 있다. 이 유사한 기술은 특정 효율적인 코드를 산출한 함수 생성자들을 만든다.


(defmacro atrec (rec &optional (base 'it))
  "cltl2 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))

(defmacro atrec (rec &optional (base 'it))
  "cltl1 version"
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (labels ((left () (funcall ,lfn))
                        (right () (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

; figure 15.5 Macros for recursion on trees.


(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))

(count-leaves '((a b (c 0 ) ((((d )))))))

(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))

(defun flatten (tree)
  (on-trees (nconc left right) (list it) tree))

(flatten '(a g 3 (ddfd)))

(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))
;figure 15.6 Functions defined using on-trees.


(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
        (delay-forced x))
    x))

;figure 15.7 Implementation of force and delay.

;15.4 Lazy Evaluation


;; Lazy evaluation means only evaluating an expression when you need its
;; value.  One way to use lazy evaluation is to build an object known as a
;; delay. A delay is a placeholder for the value of some expression. It
;; represents a promise to deliver the value of the expression if it is needed
;; at some later time. Meanwhile, since the promise is a Lisp object, it can
;; serve many of the purposes of the value it represents. And when the value
;; of the expression is needed, the delay can return it.


;지연 평가는 사용자가 그것의 값을 원할 때 표현(수식)을 평가하는 것을 의미한다. 지연 평가의 한 방법은 지연으로 알려진 객체를 생성하는 것이다. 지연은 같은 수식의 값을 위한 자리표시자이다. 그것은 같은 지연된 시간에 필요로 할 때 수식의 값을 전달하는 약속을 나타낸다. 한편, 약속이 리습 객체일 때, 그것은 나타나는 값의 목적의 다수를 제공한다. 그리고 수식의 값이 필요로 해질 때, 지연은 그것을 반환한다.


;; Scheme has built-in support for delays. The Scheme operators force and
;; delay can be implemented in Common Lisp as in Figure 15.7. A delay is
;; represented as a two-part structure. The first field indicates whether the
;; delay has been evaluated yet, and if it has, contains the value. The second
;; field contains a closure which can be called to find the value that the
;; delay represents. The macro delay takes an expression, and returns a delay
;; representing its value:

;스킴은 지연을 위한 내장 지원이 있다. 스킴 연산자들 force 와 delay는 15.7에 커먼 리습에서 구현될 수 있다. delay는 두 부분의 구조로 표현되었다. 첫번째 필드는 아직 평가되지 않은 delay를 지칭한다. 값을 포함하고 있고 가지고 있는 delay이다. 두번째 필드는 delay가 나타내는 갓을 찾는 호출을 할 수 있는 클로저를 포함한다. 매크로 delay는 수힉을 취하고 그것의 값을 표현하는 delay를 반환한다.


(let ((x 2))
(setq d (delay (1+ x))))
;#S(DELAY ...)


;; To call the closure within a delay is to force the delay. The function
;; force takes any object: for ordinary objects it is the identity function,
;; but for delays it is a demand for the value that the delay represents.

;delay 내의 클로저를 호출하는 것은 delay를 강제하는 것이다. 함수 force는 어떠한 객체도 취할 수 있다: identity 함수인 보통의 객체를 위해 그러나 delay가 표시하는 값을 위해 요구하는 delays를 위해.


(force 'a)
;A
(force d)
;3

;We use force whenever we are dealing with objects that might be delays. For
;example, if we are sorting a list which might contain delays, we would say:


;우리는 지연될 수 있는 객체를 다루는 데서 force를 사용한다. 예를 들어, 우리가 delays를 포함하는 리스트를 분류할 때, 다음과 같이 쓴다:

(setf lst (list 1 2 3 50 23))

(sort lst #'(lambda (x y) (< (force x) (force y))))

;개방된 형태에서 delyas를 사용하는 것은 약간 불편하다. 실제 응용에서는 그들은 다른 추상의 층 아래에 가려진다.