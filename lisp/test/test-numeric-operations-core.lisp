(in-package :cl-user)

(require :fiveam)

(defpackage numeric/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :sycamore-util :z3 :fiveam))

(in-package numeric/test)

(def-suite all-tests
    :description "Tests to confirm no issues with any mathematical operators used by z3")

(in-suite all-tests)

(defun test-numeric ()
  (run! 'all-tests))

(test addition
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-addition.pddl"
						 tmsmt::*tmsmt-root*))))

(test subtraction
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-subtraction.pddl"
						 tmsmt::*tmsmt-root*))))

(test multiplication
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-multiplication.pddl"
						 tmsmt::*tmsmt-root*))))

(test division
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-division.pddl"
						 tmsmt::*tmsmt-root*))))

(test square
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-square.pddl"
						 tmsmt::*tmsmt-root*))))

(test sqrt
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/numeric/numeric-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/numeric/check-sqrt.pddl"
						 tmsmt::*tmsmt-root*))))
