(in-package :cl-user)

(require :fiveam)

(defpackage sequential/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :sycamore-util :z3 :fiveam))

(in-package sequential/test)

(def-suite all-tests
    :description "tests to make sure solving for sequential goal states works in a simple case")

(in-suite all-tests)

(defun test-sequential ()
  (run! 'all-tests))

(test backtrack
      (is-true (tmsmt::pddl-sat (merge-pathnames #p"demo/domains/sequential/sequential-world.pddl"
						 tmsmt::*tmsmt-root*)
				(merge-pathnames #p"demo/domains/sequential/sequential-facts.pddl"
						 tmsmt::*tmsmt-root*)
				'((:max-steps . 3)(:trace . nil)))))
