(in-package :cl-user)

(require :asdf)
(require :tmsmt-test)

(in-package :path-checker/test)

(let((code (test-path-checker)))
  (if code
      (sb-ext:exit)
      (sb-ext:exit :code 1)))
  
