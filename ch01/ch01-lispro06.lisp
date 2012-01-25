(mapcar fn
	(do* ((x 1 (1+ x))
	      (result (list x) (push x result)))
	    ((= x 10) (nreverse result))))

(map1-n fn 10)

(do ((x a (+ 1 x)))
    ((> x b))
  (print x))

(for (x a b)
     (print x))
