(load "../ch20/ch20_netpyoung.lisp")

(defparameter *cont* #'values)

(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))
