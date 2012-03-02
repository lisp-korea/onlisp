(package-name *package*)
;; "COMMON-LISP-USER"

(symbol-package 'foo)		      ; intern된 package를 찾는 함수.
;; #<PACKAGE "COMMON-LISP-USER">

;; (in-package 'mine :use 'COMMON-LISP)
;; exactly 1 expected, but 3 found
;; (in-package 'mine)
;; 'MINE cannot be coerced to a string.

(in-package :mine)
;; The name "MINE" does not designate any package.

(defpackage :mine (:use :cl))
(in-package :mine)

(in-package :common-lisp-user)
;; #<PACKAGE "COMMON-LISP-USER">
(in-package :cl)
;; #<PACKAGE "COMMON-LISP">
(in-package :cl-user)
;; #<PACKAGE "COMMON-LISP-USER">
(export 'bar)
(setq bar 5)

(in-package :mine)
(print cl-user::bar)
(import 'common-lisp-user:bar)
(print bar)


(defpackage :mine2 (:use :cl))
(in-package :mine2)
(defvar bar 100)
(import 'common-lisp-user::bar)
;; IMPORT COMMON-LISP-USER:BAR causes name-conflicts in
;; #<PACKAGE "MINE2"> between the following symbols:
;;   COMMON-LISP-USER:BAR, MINE2::BAR
(use-package :common-lisp-user)
;; IMPORT COMMON-LISP-USER:BAR causes name-conflicts in
;; #<PACKAGE "MINE2"> between the following symbols:
;;   COMMON-LISP-USER:BAR, MINE2::BAR

(unintern 'bar) ; (unintern :bar) 안됨.

(use-package :common-lisp-user)

;; =============
(defpackage :my-application
  (:use :cl)
  (:nicknames app)
  (:export :win
	   :lose
	   :draw))
(in-package :my-application)
(defun win () (print "win"))
(defun lose () (print "lose"))
(defun draw () (print "draw"))

(in-package :mine2)
(app:win)

(symbol-package 'app:win)

(in-package :cl-user)
(export 'bar)

(defpackage :mine3 (:use :cl))
(in-package :mine3)
(progn
  (in-package :cl-user)
  (export 'bar))


(defpackage :other
  (:use :cl)
  (:export :noise))
(in-package :other)

(defun noise (animal)
  (case animal
    (dog 'woof)
    (cat 'meow)
    (pig 'oink)))

(in-package :cl-user)
(other:noise 'pig)


(defpackage :other1
  (:use :cl)
  (:export :noise))
(in-package :other1)

(defun noise (animal)
  (case animal
    (:dog :woof)
    (:cat :meow)
    (:pig :oink)))

(in-package :cl-user)
(other1:noise :pig)
