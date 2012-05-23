;21. 멀티 프로세스
;이전 장에서 실행 프로그램 자체의 상태를 잡아 프로그램을 실행시키도록 허용하는 continuations를 어떻게 허용하는지 보여줬다. 그리고 나중에 다시 시작하기 위하여 그것을 멀리 저장할 수있는 방법을 보여주었다. 이 장에서는 단일 프로그램은 아니지만 독립된 프로세스의 집합인 계산 모델을 다룬다. 프로세스의 개념은 프로그램의 상태에 대한 우리의 개념과 긴밀하게 대응한다. 이전 장에서 그 위에 매크로의 추가 계층을 작성함으로써, 우리는 Common LISP 프로그램에서 멀티프로세싱을 내장할 수 있다.

;21.1 프로세스 추상화
;멀티 프로세스는 한 번에 여러 가지 일을해야 프로그램을 표현하는 편리한 방법이다. 기존의 프로세서는 한 번에 하나의 명령을 실행했다. 멀티 프로세스가 한 번에 하나 이상의 일을 할 것이라고 말하는 건 그들이 아무래도 하드웨어 한계를 극복한다는 말을하지 않는 것이다 : 의미하는 것은 그들이 우리가 추상화의 새로운 차원에서 생각하게 하며, 우리는 지정하지 않아도 컴퓨터가 주어진 시간에 정확하게 한다는 것이다. 가상 메모리는 컴퓨터가 한 번에 하나 이상의 프로그램을 실행할 수있는 것처럼 실제로, 프로세스의 개념은 한번에 한 프로그램보다 더 많이 실행하는 것처럼 행동하도록 허용하는 것이다.

;프로세스의 연구는 운영 체제의 분야에서 전통적으로 있어 왔다. 그러나 추상화와 같은 프로세스의 유용성은 운영 체제에 국한되지 않았다. 그들은 똑같이 다른 실시간 어플리케이션과 시물레이션에서 유용하다.

;멀티 프로세스에서 수행 작업의 대부분 문제의 특정 유형의 회피하는데 기여해 왔다. 교착 상태는 여러 프로세스가 하나의 고전적인 문제다:두 프로세스가 소로 무언가를 하기를 기다리는 상태로 다른 것 전의 문턱을 넘는 것이 각각 허용되지 않는 두 사람과 같다. 또 다른 문제는 일관성없는 상태에서 시스템을 통제하는 쿼리다 - 시스템이 한 계정에서 다른 자금을 전송하는 동안 잔액 조회 하는 것을 말한다. 여기에 제시된 코드는 교착 상태 또는 일관성없는 상태를 예방을 위한 알고리즘을 테스트하기 위해 사용될 수 있지만 그 자체가 이러한 문제에 대해 어떠한 보호 기능을 제공하지 않는다;이 장에서는 프로세스 추상화 자체에 대해서만 다룬다.

;이 장에서 구현은 이 책에있는 모든 프로그램의 암묵적 규칙을 따른다:가능한 한 작게 LISP을 방해한다. 정신력으로 프로그램이 분리된 응용프로그램에 쓰여진 것 보다 언어의 수정같이 가능한 많이 하게 한다. 프로그램을 만드는 것은 아주 잘 어울리는 부분의 기계들처럼, 그들을 더 강건하게 하여 LISP과 조화시킨다. 그것은 또한 노력을 절약시킨다; 때때로 LISP은 당신을 위해 당신의 놀라운 양을 할 수 있다.

;이 장의 목적은 여러 프로세스를 지원하는 언어를 만드는 것이다. 우리의 전략은 몇 가지 새로운 연산자를 추가하여 같은 언어로 LISP를 설정하는 것이다. 우리 언어의 기본 요소는 다음과 같다 : 함수는 이전 챕터에서 =defun 또는 =lambda 매크로로 정의되었다. 프로세스는 함수 호출로 인스턴스화 된다. 활성 프로세스의 수, 또는 어느 한 함수에서 프로세스 인스턴스의 수에는 제한이 없다. 각 프로세스는 우선 순위를 갖고, 처음 만들어졌을 때, 인자로 주어진다. Wait 표현은 함수 내에서 발생할 수 있다. wait 표현식은 변수, 테스트 표현 및 코드의 본문을 취한다. 프로세스가 wait을 만나면 테스트 표현식이 true를 반환 때까지 프로세스는 그 지점에서 일시 중지된다. 프로세스가 다시 시작되면 코드의 내용이 테스트 표현식의 값을 바인딩 변수와 함께 평가한다. 얼마나 자주, 언제 그들이 평가되는지에 대한 보장이 없기 때문에 테스트 표현은 일반적으로 부수 작용이 없다. 예약은 우선 순위에 의해 수행된다. 다시 시작할 수있는 모든 프로세스들에 대해, 시스템은 가장 높은 우선 순위로 하나를 실행한다. 가능한 프로세스가 없으면 기본 프로세스가 실행된다. 그것은 REPL 이다. 대부분의 객체의 생성 및 삭제는 즉시 가능하다. 실행중인 프로세스에서 그것은 새로운 기능을 정의할 수 있고, 프로세스를 초기화하거나 종료 시킨다.

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string
				"=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(setq *cont* #'identity)

;;Figure 20.4: Continuation-passing macros.


(defstruct proc pri state wait)

(proclaim '(special *procs* *proc*))

(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
			(format t "~%>> ")
			(princ (eval (read)))
			(pick-process))))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc
	    :state #'(lambda (,(gensym))
		       ,expr
		       (pick-process))
	    :pri ,pri)
	   *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
	   (setq *procs* nil)
	   ,@body
	   (catch *halt* (loop (pick-process)))))


;figure 21.1 : 프로세스 구조와 인스턴스.

;Continuations는 LISP 프로그램의 상태를 저장하는 것을 가능하게 한다. 한 번에 여러 상태를 저장할 수 있다는 것은 멀티 프로세스를 갖는 것으로부터 멀지 않다. 이전 장에서 정의된 매크를 시작으로, 우리는 멀티 프로세스를 구현하기 위해 코드의 60 라인도 필요하지 않다.

;21.2 구현

;figure 21.1과 21.2는 멀티 프로세스를 지원하기 위해 필요한 모든 코드를 포함한다. figure 21.1은 기본 데이터 구조, 기본 프로세스, 초기화, 프로세스 인스턴스에 대한 코드를 포함한다. 프로세스, 또는 procs는 다음과 같은 구조이다 :

;pri 는 양수인 프로세스의 우선 순위다.

;state 는 정지 프로세스의 상태를 나타내는 연속 표현이다. 프로세스는 그것의 state를 funcalling하는 것으로 다시 시작된다.

;wait은 보통 다시 시작되는 프로세스에 대해 순서대로 TRUE를 반환하지만, 처음에는 새로 만든 프로세스의 wait은 nil 이다. NULL wait이 있는 프로세스는 언제나 다시 시작할 수 있습니다.

;프로그램은 세 전역 변수를 사용한다 : *procs* 현재 일시중지된 프로세스 목록, *proc* 현재 실행중인 프로세스; *default-proc* 기본 프로세스

;기본 프로세스는 다른 프로세스를 실행 할 수 없는 경우에만 실행된다. 그것은 LISP의 toplevel을 시뮬레이션한다. 이 루프 내에서 사용자는 프로그램을 중지하거나 다시 시작될 중지된 프로세스를 가능하게 하는 표현을 입력할 수 있다. 기본 프로세스가 명시적으로 eval을 부르는 것을 확인할 수 있다. 이것은 그렇게 할 정당화되는 몇 가지 상황 중 하나이다. 일반적으로 그것은 두 가지 이유로, 런타임에 eval을 호출하기위한 좋은 생각이 아니다 :

;1. 그것은 비효율적이다. eval은 raw list를 넘기고, 어느 부분에서 그것을 컴파일하거나 인터프리터로 평가해야한다. 어찌 됐든 미리 코드를 컴파일하고, 단순 호출보다 느리다.

;2. 렉시컬 컨텍스트 없이 평가되기 때문에 그것은 파워풀하지 않다. 다른 것들 중에는, 이것이 평가될 때 밖으로 보이는 일반적인 변수를 참조할 수 없다는 것을 의미한다.

;보통 eval을 명시적으로 호출하는 것은 공항 선물 가게에서 뭔가를 구입하는 것과 같다. 마지막 순간까지 기다렸다가, 당신은 B급 상품의 제한된 선택을 위해 높은 가격을 지불해야 한다.

;앞의 두 인수 중 어느 것도 적용하면 이런 경우는 드문 경우이다. 우리는 어쩌면 미리 표현식을 컴파일할 수 없다. 우리는 단지 그들을 읽는다; 사전에 없다. 마찬가지로, toplevel이 null 렉시컬 환경에 있기 때문에, 둘러싼 렉시컬 변수를 참조 수 없다. 사실, 이 함수의 정의는 단순히 영어 설명을 반영한다 : 그것은 사용자 형식이 무엇인지 읽고 평가한다.

;매크로 fork는 함수 호출에서 프로세스를 인스턴화 한다. 함수는 =defun으로 평소처럼 정의한다.

(=defun foo (x)
	(format t "Foo was called with ~A.~%" x)
	(=values (1+ x)))

;이제 우리가 함수 호출과 우선순위 번호로 fork를 호출할 때 :

(fork (foo 2) 25)


;새로운 프로세스는 *procs*로 들어 간다. 새로운 프로세스가 25의 우선 순위를 가지고, nil의 proc-wait, 아직 시작되지 않았기 때문에, proc-state는 인자 2로 foo를 호출하는 구성이다.

;매크로 program은 우리가 프로세스 그룹을 만들고 그들을 함께 실행할 수 있다. 정의 :

(program two-foos (a b)
	 (fork (foo a) 99)
	 (fork (foo b) 99))


;두 fork 표현으로 macroexpands, 중지 프로세스을 면확히하는 코드 간에 끼워진, 실행될 프로세스를 반복적으로 선택하는 다른 코드. 이 루프를 벗어나면, 매크로 컨트롤 프로그램을 끝내도록 던져질 수 있는 컨트롤 태그를 설정한다. gensym 으로서,이 태그는 사용자의 코드에 의해 설립된 태그와 충돌하지 않는다. program으로 정의된 프로세스 그룹에는 특정 값을 반환하지 않으며, 오직 최상위에서 호출된 것만을 의미한다.
;프로세스가 인스턴스화 된 후에 프로세스 스케줄링 코드는 이상이 소요된다. 이 코드는 figure 21.2에 표시했다. 함수 pick-process가 다시 시작될 수가 가장 높은 우선 순위 프로세스를 선택하고 실행한다. 이 프로세스을 선택하는 것은 most-urgent-process의 작업이다. 중지된 프로세스는 wait 함수가 없다면 실행할 수 있는 자격이며, 또는 wait 함수는 true를 반환한다. 해당 프로세스 중에서 가장 높은 우선 순위와 하나가 선택된다. 선택된 프로세스 및 wait 함수 (하나가있는 경우)에 의해 반환된 값은 pick-process에 반환된다. 기본 프로세스가 항상 실행하고자 하기 때문에, 항상 선택되는 프로세스가 된다.

;figure 21.2의 코드의 나머지 부분은 프로세스간에 컨트롤을 전환하는데 사용되는 연산자를 정의한다. figure 21.3에 있는 pedestrian 함수에서 사용되는 표준 대기 표현은 wait이다. 이 예제에서 프로세스는 *open-doors* 목록에 무언가가 있을 때까지 기다리고 있다가 다음 메시지를 출력한다.


(ped)
(push 'door2 *open-doors*)
;Entering DOOR2
(halt)
;NIL


;wait은 =bind의 사상과 (페이지 267) 유사하고, 평가될 마지막 것이 되어야 하는 제한을 옯긴다. wait 후 실행되길 바라는건 뭐든지은 그 body에 넣어야 한다. 따라서, 우리는 여러 번 프로세스 대기가 있길 원한다면, 대기 표현은 중첩되어야 한다. 서로를 겨냥한 사실을 주장함으로써, 프로세스는 figure 21.4에서와 같이, 어떤 목표에 도달 협력할 수 있다.


(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
	  *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
	(if (> pri max)
	    (let ((val (or (not (proc-wait p))
			   (funcall (proc-wait p)))))
	      (when val
		(setq proc1 p
		      max pri
		      val1 val))))))
    (values proc1 val1)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
	(proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
	       #'(lambda (,parm) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
    (pick-process)))


;figure 21.2 : 프로세스 스케줄링.

(defvar *open-doors* nil)

(=defun pedestrian ()
	(wait d (car *open-doors*)
	      (format t "Entering ~A~%" d)))

(program ped ()
	 (fork (pedestrian) 1))

;figure 21.3 : 하나 대기가있는 하나의 프로세스.
;;;;;;;;;;

;프로세스는 같은 door가 주워지면 visitor 와 host 에 의해 인스턴스화 된다. 칠판에 메시지를 통해 제어권을 교환한다.


(ballet)
;Approach DOOR2. Open DOOR2. Enter DOOR2. Close DOOR2.
;Approach DOOR1. Open DOOR1. Enter DOOR1. Close DOOR1.


;또 다른게 있다, 대기 표현의 더 간단한 형태:yield 는 단지 실행 기회를 처리하는 더 높은 우선순위를 주기 위한 목적으로만 쓰인다. 프로세스가 setpri 표현을 실행 후 양보를 원하면, 현재 프로세스의 우선순위가 리셋된다. wait 으로, yield 후에 실행되는 어떠한 코드도 body에 넣어야 한다.

;figure 21.5에 있는 프로그램은 두 연산자가 함께 실행되는 방법을 보여준다. 처음에 바바리아인들은 두 가지 목표를 가진다 : 로마를 점령하여 약탈을 한다. 도시를 점령하는 것은 (약간) 더 높은 우선 순위를 가지고 있으며, 그래서 먼저 실행된다. 그러나, 도시도 감소하면, capture 프로세스의 우선 순위는 1로 줄어 듭니다. 그리고 나서 투표가 있고, 약탈이 가장 높은 우선 순위 프로세스가 되어 시작된다.


(barbarians)
;Liberating ROME.
;Nationalizing ROME.
;Refinancing ROME.
;Rebuilding ROME.


;바바리아인들은 로마의 궁궐을 약탈하고 부호를 ransomed 후에, capture 프로세스가 계속된다. 그리고 바바리아인들은 자신의 위치를 강화시킨다.


(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
	(format t "Approach ~A. " door)
	(claim 'knock door)
	(wait d (check 'open door)
	      (format t "Enter ~A. " door)
	      (unclaim 'knock door)
	      (claim 'inside door)))

(=defun host (door)
	(wait k (check 'knock door)
	      (format t "Open ~A. " door)
	      (claim 'open door)
	      (wait g (check 'inside door)
		    (format t "Close ~A.~%" door)
		    (unclaim 'open door))))

(program ballet ()
	 (fork (visitor 'door1) 1)
	 (fork (host 'door1) 1)
	 (fork (visitor 'door2) 1)
	 (fork (host 'door2) 1))

;figure 21.4 : 칠판에 의한 동기화.


;대기 표현 밑에는 보다 일반적인 abitrator 이다. 이 함수는 현재 프로세스를 저장하고 (아마 같은) 어떤 프로세스를 다시 실행하기 위해 pick-process를 호출한다.  그것은 두 개의 인수를 받는다 : 테스트 기능과 연속. 전자는 일시정지된 프로세스의 proc-wait에 저장될 것이고, 다시 시작될 수 있을 때 결정되도록 추후에 호출된다. 후자는 proc-state가 되고, 그것을 호출하는 것은 일시중지된 프로세스를 다시 시작하는 것을 호출한다.


(=defun capture (city)
	(take city)
	(setpri 1)
	(yield
	 (fortify city)))

(=defun plunder (city)
	(loot city)
	(ransom city))

(defun take (c) (format t "Liberating ~A.~%" c))

(defun fortify (c) (format t "Rebuilding ~A.~%" c))

(defun loot (c) (format t "Nationalizing ~A.~%" c))

(defun ransom (c) (format t "Refinancing ~A.~%" c))

(program barbarians ()
	 (fork (capture 'rome) 100)
	 (fork (plunder 'rome) 98))


;figure 21.5 : 변화하는 우선순위의 효과.
;이 연속 함수를 만든 매크로 wait과 yield는 람다-표현식에 그들의 body를 감싸는 것으로 단순화 한다. 예를 들어,

(wait d (car *bboard*) (=values d))

;로 확장

(arbitrator #'(lambda () (car *bboard*))
	    #'(lambda (d) (=values d)))



;코드가 figure 20.5에 나와있는 제한 사항을 따르는 경우에는 wait body의 closure를 만드는 것이 전체적으로 현재 컨티뉴에이션을 유지할 것이다. =values로 확장된 두 번째 인수는 다음과 같이 된다.

#'(labmda (d) (funcall *cont* d))

;closure가 *cont*를 참조하는 것을 포함할 때, 이 대기 함수를 사용하여 중지된 프로세스는 그것이 중지된 때에서 맞게될 곳의 핸들을 갖는다.

;선택적 인수를 취하는데, 프로그램의 값으로써 리턴되는 것이다. 기본 프로세스가 항상 실행하고자하기 때문에, 프로그램 종료의 유일한 방법은 명시적 halt이다. 그것은 평가되지 않기 원하면, halt가 있는 코드는 별 문제가 되지 않는다.

;개별 프로세스는 kill을 호출하는 것으로 종료될 수 있다. 인자가 없더라도, 이 연산은 현재 프로세스를 종료시킨다. 이 경우, kill은 현재 프로세스를 저장하는 것을 무시 대기 표현 같다. kill이 인자가 있을 때, 그들은 프로세스 목록에서 delete에 인자가 된다. 프로세스들은 참조할 때, 많은 속성들을 갖지 못하기 때문에, 현재 코드에서는 kill 표현에서 하나 이상을 기술할 수 없다. 그러나 보다 정교한 시스템이 프로세스를 통해 더 많은 정보를 연결할 수 있다 - 타임 스탬프, 소유자 등. 그것이 목록 *procs*에 보관되지 않기 때문에 기본 프로세스는 종료될 수 없다.

;21.3 이하보다 신속한 프로토 타입(less-than-rapid prototype)


;continuations로 시뮬레이션된 프로세스들은 실제 운영 체제 프로세스만큼 효율적이지 않다. 그렇다면 이 장에서에 이것과 같은 프로그램의 사용은 무엇인가?

;이러한 프로그램은 스케치하는 것과 같은 방법에서 유용하다. 습작 프로그래밍이나 빠른 프로토 타이핑에 프로그램이 그것의 아이디어를 표현하기 위한 수단으로서 자체가 끝이 아니다. 다른 많은 분야에서 이 목적을 제공하고 것을 스케치라고 한다. 근본적으로 아키텍트는 자신의 머리에서 전체 건물을 설계한다. 그러나 대부분의 건축가들은 손에 연필로 더 나은 생각을 할 수 있다: 건물의 설계는 일반적으로 예비 스케치들의 연속으로 표현된다.

;래피드 프로토 타이핑 소프트웨어를 스케치한다. 건축가 최초의 스케치와 마찬가지로 소프트웨어 프로토 타입은 쓸어내는 스트로크로 그려질 수 있다. 비용과 효율성의 고려 사항은 전체에 아이디어를 개발하기 위해 초기 투입에서 무시된다. 그 결과,이 단계에서, 만들 수 없는 건물이나 소프트웨어의 길을 비효율적인 부분이 될 가능성이 높다. 하지만, 스케치는 다음과 같은 이유로 가치가 있다.

;그들은 간단히 정보를 전달한다.
;그들은 실험 기회를 제공한다.

;이 장에서 설명하는 프로그램은 이어지는 챕터에서 이들과 같은 스케치이다. 그것은 몇 가지, 폭넓은 스트로크의 멀티 프로세싱의 윤곽을 제안한다. 그리고 생산 소프트웨어에 사용할 정도로 효율적 아니지만, 그것은 스케줄링 알고리즘과 같은 여러 프로세스의 다른 측면을 실험 하기엔 상당히 유용할 수 있다.

;챕터 22 - 24는 continuations의 다른 응용을 표현한다. 그것들 중 무엇도 생산 소프트웨어에서 사용하기에 충분히 효율적이지 않다. Lisp과 빠른 프로토타이핑은 함께 발전하므로, Lisp은 프로토타입들을 위해 의도된 특별한 많은 기능들을 포함한다 : 재산 목록, 키워드 매개 변수와 같은 비효율적이지만 편리 기능. Continuations 아마 이 범주에 속한다. 그들은 기능과 같은 프로그램 보다 많이 절차를 절약한다. 예를들어, Continuation 기반 구현 Prolog는 언어를 이해하는 좋은 방법이지만 그것을 구현하는 건 비효율적인 방법이다.

;이 책은 효율성 문제 보다 LISP를 구축할 수 추상적인 관념의 종류에 더 관심있다. 그것은 LISP는 프로토 타입을 쓰기 위한 생산 소프트웨어만큼 프로토타입을 작성하는 언어로 깨닫는 것을 중요시 한다. LISP는 속도 저하에 대한 평판을 가지고 있다면, 그것은 대체로 많은 프로그래머들은 프로토 타입과 함께 멈추기 때문이다. LISP으로 빠른 프로그램을 작성하기는 쉽다. 불행히도, 느린 프로그램을 작성하는 것은 매우 쉽다. 작고 명확하며 매우 비싼: LISP 프로그램의 초기 버전은 다이아몬드처럼 될 수 있다. 그렇한 방법으로 떠날 수 있는 큰 유혹이 있을 수 있다.

;다른 언어에서는 프로그램이 작동하기 힘든 작업에서 이미 수용적으로 효율적이도록 이어질 수 있다. 당신의 엄지 크기로 바닥을 타일로 채운다면 많은 낭비를 하지않는다. 이 원칙에 소프트웨어를 개발하면, 프로그램이 동작하고 종료될 때, 아이디어를 극복하는 게 어렵다는 것을 발견할 있다. "LISP에서는 시간이 전혀 없어도 프로그램을 작성할 수 있다"고 생각하면, "하지만 그들은 느리다."고 생각한다. 사실, 둘다 상황이 될 수 있다. 당신이 빠른 프로그램을 얻을 수 있지만, 그들을 위해 일을 해야한다. 이러한 측면에서 LISP를 사용하는 것은 가난한 나라에 사는 것 대신 부자 나라에 사는 것과 같다. it may seem unfortunate that one has to work to stay thin, but surely this is better than working to stay alive, and being thin as a matter of course.

;덜 추상적인 언어에서는 기능을 위해 동작한다. LISP에서는 속도를 위해 동작한다. 다행히 속도를 위해 동작하는 것이 쉽다 : 대부분의 프로그램은 속도 문제에서 몇몇의 심각한 부분이 있다.

;http://www.ibm.com/developerworks/kr/library/j-jtp11137.html
;자바 이론과 실습: 포크 찌르기, Part 1

;http://www.electrictoolbox.com/mysql-connection-php-fork/
;MySQL connections and PHP forked processes

;http://www.electrictoolbox.com/article/php/process-forking/
;Process Forking with PHP

;http://forum.falinux.com/zbxe/?document_srl=412814
;fork() 프로세스 생성