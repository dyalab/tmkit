(in-package :cl-user)

(require :asdf)
(require :tmsmt-test)

(in-package :feedback/test)

(let((code (test-feedback)))
  (if code
      (sb-ext:exit)
      (sb-ext:exit :code 1)))
