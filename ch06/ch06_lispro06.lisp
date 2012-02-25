;6. Functions as Representation
;a graph could represent a rail network
;In Lisp we can sometimes use closures as a representation

;6.1 Networks

;1 달러지폐 워싱턴
;2 달러 지폐 토마스 제퍼슨
;5 달러 지폐: 링컨
;10 달러 지폐:해밀톤
;20 달러 지폐: 잭슨
;50 달러 지폐: 그랜트
;100달러 지폐: 프랭클린


> (run-node 'people)
Is the person a man?
>> yes
Is he living?
>> no
Was he American?
>> yes
Is he on a coin?
>> yes
Is the coin a penny?
>> yes
LINCOLN

(defnode 'people "Is the person a man?"
  'male 'female)

 	
(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
	(make-node :contents conts
		   :yes yes
		   :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
	   (format t "~A~%>> " (node-contents n))
	   (case (read)
	     (yes (run-node (node-yes n)))
	     (t (run-node (node-no n)))))
	  (t (node-contents n)))))

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
	(if yes
	    #'(lambda ()
		(format t "~A~%>> " conts)
		(case (read)
		  (yes (funcall (gethash yes *nodes*)))
		  (t (funcall (gethash no *nodes*)))))
	  #'(lambda () conts))))

;6.2 Compiling Networks

(funcall (gethash 'people *nodes*))

 	
Is the person a man?
>>

 	
(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
	nil
      (let ((conts (second node))
	    (yes (third node))
	    (no (fourth node)))
	(if yes
	    (let ((yes-fn (compile-net yes))
		  (no-fn (compile-net no)))
	      #'(lambda ()
		  (format t "~A~%>> " conts)
		  (funcall (if (eq (read) 'yes)
			       yes-fn
			     no-fn))))
	  #'(lambda () conts))))))

> (setq n (compile-net 'people))
#<Compiled-Function BF3C06>
> (funcall n)
Is the person a man?
>>

;6.3 Looking Forward

