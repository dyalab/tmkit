(in-package :cl-user)

(require :asdf)
(require :tmsmt-test)

(in-package :sequential/test)

(let((code (test-sequential)))
  (if code
      (sb-ext:exit)
      (sb-ext:exit :code 1)))
