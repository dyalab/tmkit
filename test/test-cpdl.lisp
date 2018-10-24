(in-package :cl-user)

(require :asdf)
(require :tmsmt-test)

(in-package :cpdl/test)

(let((code (test-cpdl)))
  (if code
      (sb-ext:exit)
      (sb-ext:exit :code 1)))
