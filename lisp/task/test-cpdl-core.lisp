(in-package :cl-user)

(require :fiveam)

(defpackage cpdl/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :fiveam))


(in-package :cpdl/test)

(def-suite all-tests
    :description "Path checking test suite")

(in-suite all-tests)

(defun test-cpdl ()
  (run! 'all-tests))

;;test conversion from s-exp to probability function
(test s-exp->prob
      (is (equal '(* (a) (b)) (tmsmt::s-exp->probability '(and (a) (b)))))
      (is (equal '(- 1 (a)) (tmsmt::s-exp->probability '(not (a)))))
      (is (equal '(- 1 (* (- 1 (a)) (- 1 (b)))) (tmsmt::s-exp->probability '(or (a) (b)))))
      (is (equal '(- 1 (* (a) (- 1 (* (a) (b))))) (tmsmt::s-exp->probability '(=> (a) (b)))))
      (is (equal '(* (- 1 (* (a) (- 1 (* (a) (b)))))
		   (- 1 (* (b) (- 1 (* (b) (a)))))) (tmsmt::s-exp->probability '(<=> (a) (b)))))
      (is (equal '(* (- 1 (* (A) (B))) (- 1 (* (- 1 (A)) (-1 (B)))))
		 (tmsmt::s-exp->probability '(xor (a) (b)))))
      (is (equal '(* (a)) (tmsmt::s-exp->probability '(and (a)))))
      (is (equal '(* (a) (b) (c)) (tmsmt::s-exp->probability '(and (a) (b) (c)))))
      (is (equal '(- 1 (* (- 1 (a)))) (tmsmt::s-exp->probability '(or (a)))))
      (is (equal '(- 1 (* (- 1 (a)) (- 1 (b)) (- 1 (c))))
		 (tmsmt::s-exp->probability '(or (a) (b) (c)))))
      (is (equal '(* (- 1 (a)) (b)) (tmsmt::s-exp->probability '(and (not (a)) (b))))))
