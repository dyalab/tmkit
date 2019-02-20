(in-package :cl-user)

(require :fiveam)

(defpackage feedback/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :sycamore-util :z3 :fiveam))

(in-package feedback/test)

(def-suite all-tests
    :description "tests to make sure solving using probabilities works for simple cases")

(in-suite all-tests)

(defun test-feedback ()
  (run! 'all-tests))

(test backtrack
      (is-true (tmsmt::feedback-search (merge-pathnames #p"demo/domains/blocksworld/tm-blocks.pddl"
						 tmsmt::*tmsmt-root*)
				       (merge-pathnames
					#p"demo/domains/blocksworld/tm-sussman-probabilistic.pddl"
						 tmsmt::*tmsmt-root*))))
