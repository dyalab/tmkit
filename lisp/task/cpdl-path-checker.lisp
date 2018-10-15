(in-package :tmsmt)

(defun path-checker (curState path function)
  (if (equal path nil)
      t
      (and (funcall function curState (car path)) (path-checker (car path) (cdr path) function))))

(defun compile-transition (exp)
  (eval `(lambda (now-state next-state)
	   (macrolet ((now (x) `(get-value (quote ,x) now-state))
		      (next (x)  `(get-value (quote ,x) next-state)))
	     ,(cons 'and (subst 'equal '= exp))))))

(defun get-value(x state)
  (etypecase state
    (hash-table (z3->lisp (gethash x state)))
    (tree-map (z3->lisp (tree-map-find state x)))))



    
	
    
	
