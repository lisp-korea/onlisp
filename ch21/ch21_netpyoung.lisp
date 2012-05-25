(load "../ch20/ch20_netpyoung.lisp")

;;

(defstruct proc
  pri
  state
  wait)

(proclaim '(special *procs* *proc*))
(defparameter *procs* nil)
(defparameter *proc*  nil)
(defparameter *halt* (gensym))

(defparameter *default-proc*
  (make-proc :state #'(lambda (x)
			(declare (ignorable x))
			(format t "~%>> ")
			(princ (eval (read)))
			(pick-process))))


(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
	(if (> pri max)
	    (let ((val (or (not (proc-wait p))
			   (funcall (proc-wait p)))))
	      (when val
		(setq proc1 p
		      max   pri
		      val1  val))))))
    (values proc1 val1)))

(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc*  p
	  *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
	(proc-wait  *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (params test &body body)
  `(arbitrator #'(lambda () ,test)
	       #'(lambda (,params) ,@body)))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n)
  (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))


;;
;; 
(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc :state #'(lambda (,(gensym))
				 ,expr
				 (pick-process))
		      :pri   ,pri)
	   *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
	   (setq *procs* nil)
	   ,@body
	   (catch *halt* (loop (pick-process)))))

;; 
(=defun foo (x)
  (format t "Foo was called with ~A. ~%" x)
  (=values (1+ x)))

(foo 123)
;; 
(defparameter *open-doors* nil)

(=defun pedestrian ()
  (wait d (car *open-doors*)
	(format t "Entering ~A~%" d)))

(program ped ()
	 (fork (pedestrian) 1))

(fork (foo 2) 25)
(program two-foos (a b)
	 (fork (foo a) 99)
	 (fork (foo b) 99))
;; (ped)
;; (push 'door2 *open-doors*)
;; (halt)
(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (GET-SETF-EXPANSION place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defparameter *bboard* nil)
(defun claim   (&rest f) (push f *bboard*))
(defun unclaim (&rest f) ;(pull f *bboard* :test #'equal))
(defun check   (&rest f) (find f *bboard* :test #'equal))

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
      (format t "Close ~A. ~%" door)
      (unclaim 'open door))))

(program ballet ()
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1))
  

;;

(defun take    (c) (format t "Liberating ~A. ~%" c))
(defun fortify (c) (format t "Rebuilding ~A. ~%" c))
(defun loot    (c) (format t "Nationalizing ~A. ~%" c))
(defun ransom  (c) (format t "Refinacing ~A. ~%" c))

(=defun capture (city)
  (take city)
  (setpri 1)
  (yield (fortify city)))

(=defun plunder (city)
  (loot city)
  (ransom city))

(program barbarians ()
  (fork (capture 'rome) 100)
  (fork (plunder 'rome) 98))
