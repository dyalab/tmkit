(in-package :tmsmt)

;;returns a list containing just the weakest step.
(defun make-assert-not-weakest-step (plan limiting-step domain)
  (loop for (action . performed) in plan
     if (and (eq performed :true)
	     (= limiting-step (car action)))
     collect (cpd-mangle-fluent domain (cdr action)
				limiting-step)))

;;returns a list containing the whole plan and the final goal state.
(defun make-assert-not-whole-plan (plan limiting-step domain)
  (declare (ignore limiting-step))
  (let* ((i 0)
	(final-goal (car (last (constrained-domain-goal-clauses domain))))
	(plan (loop for (action . performed) in plan
		 if (eq performed :true)
		 collect (progn
			   (if ( > (car action) i)
			       (setf i (car action)))
			   (cpd-mangle-fluent domain
					      (cdr action) (car action))))))

    (cons (cpd-mangle-exp domain final-goal i)
	  plan)))

;;returns a list containing all the actions up to the weakest step
(defun make-assert-not-up-to-weakest-step (plan limiting-step domain)
  (loop for (action . performed) in plan
     if (and (eq performed :true)
	     (>= limiting-step (car action)))
     collect (cpd-mangle-fluent domain (cdr action)
				(car action))))
