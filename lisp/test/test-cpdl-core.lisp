(in-package :cl-user)

(require :fiveam)

(defpackage cpdl/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :sycamore-util :z3 :fiveam))


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


;;test equality generation via in-start
(test in-start
  (is (equal t (test-in-start '((a . b) (b . c) (c . d) (d . 'true)) '((a . b) (a . c) (a . d)))))
  (is (equal t (test-in-start '((a . b) (b . c) (c . d) (d . 'true)) '((b . a) (b . c) (b . d)))))
  (is (equal t (test-in-start '((a . b) (b . c) (c . d) (d . 'true)) '((c . a) (c . b) (c . d)))))
  (is (equal t (test-in-start '((a . b) (b . c) (c . d) (d . 'true)) '((d . a) (d . b) (d . c)))))
  (is (equal nil (test-in-start '((a . b) (c . d)) '((a . d))))))

(defun test-in-start (alist check-list)
  (let ((test-hash (alist-hash-table alist :test 'equal)))
    (fold (lambda (val test)
	    (and val (tmsmt::in-start (car test) (cdr test) test-hash)))
	  t
	  check-list)))

(test smt-functions
      (is-true (test-smt-functions)))

(defun test-smt-functions ()
  "Tests that smt functions that rely on a solver or optimizer and a context don't return an error"
  (let* ((solver (z3::make-solver))
	 (solver-context (z3::z3-get-context solver))
	 (optimize (z3::make-optimizer))
	 (optimize-context (z3::z3-get-context optimize)))

    ;;declare constants
    (z3::smt-declare-const solver-context 's1 'int)
    (z3::smt-declare-const solver-context 's2 'int)
    (z3::smt-declare-const solver-context 's3 'int)

    (z3::smt-declare-const optimize-context 'o1 'int)
    (z3::smt-declare-const optimize-context 'o2 'int)
    (z3::smt-declare-const optimize-context 'o3 'int)

    ;;make assertions
    (z3::smt-assert solver '(= s1 1)) ;;no context given
    (z3::smt-assert solver '(= s2 s3) solver-context) ;;context given

    (z3::smt-assert optimize '(= o1 1))
    (z3::smt-assert optimize '(= o2 o3) optimize-context)

    ;;check sat
    (z3::smt-check solver)
    (z3::smt-check solver solver-context)

    (z3::smt-check optimize)
    (z3::smt-check optimize optimize-context)

    ;;get model variables
    (z3::smt-values solver '(s1))
    (z3::smt-values solver '(s1) solver-context)

    (z3::smt-values optimize '(o1))
    (z3::smt-values optimize '(o1) optimize-context)
    t))
  
    
    
