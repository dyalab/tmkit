(in-package :tmsmt)

(defun path-checker (domain path)
  (let* ((func (compile-transition (constrained-domain-transition-clauses domain)))
	 (end-state (path-checker-recursive (constrained-domain-start-map domain) path func)))
;;    end-state))
    (if end-state
	(eval-state (subst 'equal '= (constrained-domain-goal-clauses domain)) end-state)
	nil)))

(defun path-checker-recursive (curState path function)
  (if (equal path nil)
      curState
      (and (funcall function curState (car path))
	   (path-checker-recursive (car path) (cdr path) function))))

(defun compile-transition (exp)
  (eval `(lambda (now-state next-state)
	   (macrolet ((now (x) `(get-value (quote ,x) now-state))
		      (next (x)  `(get-value (quote ,x) next-state)))
	     ,(cons 'and (subst 'equal '= exp))))))

(defun get-value(x state)
  (etypecase state
    (hash-table (multiple-value-bind (ret found)
		    (gethash x state)
		  (if found
		      (values (z3->lisp ret) t)
		      (values nil nil))))
    (tree-map (multiple-value-bind (ret ign found)
		  (tree-map-find state x)
		  (declare (ignore ign))
		  (if found
		      (values (z3->lisp ret) t)
		      (values nil nil))))))

(defun eval-state (exp state)
  (cond
    ((not exp)
     t)
    (t
     (and (eval-exp (car exp) state)
	  (eval-state (cdr exp) state)))))

(defun eval-exp (exp state)
  (cond
    ((not exp)
     nil)
    ((not (consp exp))
     (get-value exp state))
    ((get-value exp state)
     (get-value exp state))
    ((fboundp (car exp))
     (destructuring-case exp
       ((and &rest args)
	(eval (cons 'and (loop for x in args
			    collect (eval-exp x state)))))
       ((or &rest args)
	(eval (cons 'or (loop for x in args
			   collect (eval-exp x state)))))
       ((not arg)
	(not (get-value arg state)))
       ((t &rest args)
	(apply (car exp) (loop for x in args
			    collect (eval-exp x state))))))
    (t
     (get-value exp state))))
