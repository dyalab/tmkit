(cl:eval-when (:load-toplevel :execute)
    (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem tmsmt-test
  :description "Tests for TMKit"
  :depends-on ("tmsmt" "fiveam")
  :components (
               (:file "z3/test")
	       (:file "test/test-path-checker-core")
	       (:file "test/test-cpdl-core")
	       (:file "test/test-numeric-operations-core")
	       (:file "test/test-sequential-core")
	       (:file "test/test-feedback-core")))
