(in-package :cl-user)

(require :asdf)
(require :tmsmt-test)

(in-package :numeric/test)

(let((code (test-numeric)))
  (if code
      (sb-ext:exit)
      (sb-ext:exit :code 1)))
  
