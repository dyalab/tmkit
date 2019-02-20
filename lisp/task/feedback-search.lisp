(in-package :tmsmt)

(defstruct feedback-planner
  planner
  probability-transition-function
  goal-functions
  value-hash
  action-fluent-list
  fluent-propagation-list
  plan
  (success-probability 0)
  )

(defun feedback-plan-options (&key (max-steps 10)
				(trace nil)
				(threshold .5))
  `((:max-steps . ,max-steps)
    (:trace . ,trace)
    (:threshold . ,threshold)))

(defun feedback-plan-option (options thing)
  (cdr (assoc thing options)))

(defun collect-bool-function-names (op)
  (loop for x in (append (pddl-operators-functions op)
			 (pddl-operators-derived op))
     if (eq (pddl-function-type x) 'bool)
     collect (pddl-function-name x)))

(defun find-and-replace-probabilities (operators facts thresh)
  (let* ((func-names (collect-bool-function-names operators))
	 (init (pddl-facts-init facts))
	 (start-hash (make-hash-table :test 'equal))
	 (bool-init (reduce (lambda (ret var)
			      (destructuring-case var
				((= &rest args)				   
				 (assert (and (cdr args) (null (cddr args))))
				 (setf (gethash (cons 0 (car args)) start-hash) (cadr args))
				 (if (and (consp (car args))
					  (member  (caar args) func-names))
				     (progn
				       (if (> (cadr args) thresh)
					   (cons (car args) ret)
					   ret))
				     (cons var ret)))
				((t &rest args)
				 (declare (ignore args))
				 (setf (gethash (cons 0 var) start-hash) 1)
				 (cons var ret))))
			    init :initial-value nil)))
    (setf (pddl-facts-init facts) bool-init)
    start-hash))

(defun compile-propagation-function (exp)
  (eval `(lambda (var-hash time-step)
	   (macrolet ((now (x) `(get-value-at-timestep (quote ,x) var-hash time-step))
		      (next (x) `(get-value-at-timestep (quote ,x) var-hash (+ 1 time-step))))
	     ,exp))))

(defun get-value-at-timestep (var var-hash k)
  (gethash (cons k var) var-hash 0))

(defun is-present (clauses var-hash time-step)
  (cond
    ((null clauses)
     nil)
    ((not (= 0 (get-value-at-timestep (car clauses) var-hash time-step)))
     t)
    (t
     (is-present (cdr clauses) var-hash time-step))))

(defun create-variable-propagation-list (ground)
  (let* ((action-list (ground-domain-operators ground))
	 (variables (ground-domain-variables ground))
	 (action-fluents (map 'list #'pddl-sat-op action-list))
	 (true-hash (make-hash-table :test #'equal))
	 (false-hash (make-hash-table :test #'equal))
	 (action-hash (make-hash-table :test #'equal)))
      
      (loop for a in action-list
	 for op-exp in action-fluents
	 do (progn	      
	      (dolist (v (ground-action-modified-true a))
		(setf (gethash v true-hash) (cons op-exp (gethash v true-hash))))
	      (dolist (v (ground-action-modified-false a))
		(setf (gethash v false-hash) (cons op-exp (gethash v false-hash))))))
      
      (loop for v in variables
	 collect (let* ((true-clause (gethash v true-hash))
			(false-clause (gethash v false-hash))
			(v v))
		   (cons v (lambda (var-hash time-step)
				   (cond
				     ((is-present true-clause var-hash time-step)
				      1)
				     ((is-present false-clause var-hash time-step)
				      0)
				     (t
				      (get-value-at-timestep v var-hash time-step)))))))))
				      

(defun feedback-planner-init (operator facts options)
  (let* ((operator (load-operators operator))
	 (facts (load-facts facts))
	 (threshold (feedback-plan-option options :threshold))
	 (value-hash (find-and-replace-probabilities operator facts threshold))
	 (ground (ground-domain operator facts))
	 (cpd (ground->cpdl ground)))

    (loop for key being the hash-key of (constrained-domain-start-map cpd)
       using (hash-value val)
       do (if (eq val 'false)
	      (setf (gethash (cons 0 key) value-hash) 0)))
    
    (multiple-value-bind (plan sat planner)
	(cpd-plan cpd options)
      (if sat
	  (let ((variable-prop (create-variable-propagation-list ground))
		(trans-func (s-exp->probability (cons 'and
						      (constrained-domain-transition-clauses cpd))
						:if-symb 'if))
		(goal-funcs (loop for goal in (constrained-domain-goal-clauses cpd)
			       collect (compile-propagation-function
					(s-exp->probability
					 (exp-now goal))))))
	    (make-feedback-planner :planner planner
				   :probability-transition-function (compile-propagation-function
								     trans-func)
				   :goal-functions goal-funcs
				   :value-hash value-hash
				   :action-fluent-list (map 'list #'pddl-sat-op
							    (ground-domain-operators ground))
				   :fluent-propagation-list variable-prop
				   :plan plan))))))


(defun calculate-plan-probability (feedback-planner plan)
  (let ((values (feedback-planner-value-hash feedback-planner))
	(fluent-propagation (feedback-planner-fluent-propagation-list feedback-planner))
	(k (cpd-planner-k (feedback-planner-planner feedback-planner)))
	(trans-func (feedback-planner-probability-transition-function feedback-planner))
	(goal-funcs (feedback-planner-goal-functions feedback-planner))
	(goals (cpd-planner-added-goals
		(feedback-planner-planner feedback-planner))))

    (loop for (action . performed) in plan
       do (if (eq performed :true)
	      (setf (gethash action values) 1)
	      (setf (gethash action values) 0)))

    (multiple-value-bind (transition-probability limiting-step)
	(propagate-transition-probabilities values trans-func fluent-propagation k 0 1 1 0)
      (let ((goal-probability (loop for (goal . timestep) in goals
				 for goal-func in goal-funcs
				 collect (funcall goal-func values timestep))))
	(format t "~%goal: ~D transition: ~D ~%" goal-probability transition-probability)
	(values (eval (cons '* (cons transition-probability goal-probability)))
		(cons (car (loop for (action . performed) in plan
			      if (and (eq performed :true)
				      (= limiting-step (car action)))
			      collect (cdr action)))
		      limiting-step))))))

(defun propagate-transition-probabilities (values trans-func fluent-propagation max-steps
					   cur-step trans-prob limiting-prob limiting-step)
  (if (= cur-step max-steps)
      (values trans-prob limiting-step)
      (progn
	(loop for (var . func) in fluent-propagation
	   do (let ((new-var (funcall func values cur-step)))
		(setf (gethash (cons (+ 1 cur-step) var) values) new-var)))
	(let ((step-trans (funcall trans-func values cur-step)))
	  (if (> limiting-prob step-trans)
	      (propagate-transition-probabilities values trans-func fluent-propagation max-steps
						  (+ cur-step 1) (* trans-prob step-trans)
						  step-trans cur-step)
	      (propagate-transition-probabilities values trans-func fluent-propagation max-steps
						  (+ cur-step 1) (* trans-prob step-trans)
						  limiting-prob limiting-step))))))

    
    

(defun feedback-planner-next (feedback-planner new-plan)
  (let ((current-prob (feedback-planner-success-probability feedback-planner)))
    (multiple-value-bind (new-prob limiting-action)
	(calculate-plan-probability feedback-planner new-plan)
      (format t "~D ~%" new-prob)
      (cond
	((<= new-prob current-prob) ;;at a local max, return current plan
	 (feedback-planner-plan feedback-planner))
	((= new-prob 1) ;;return current plan if it is certain
	 (feedback-planner-plan feedback-planner))
	(t ;;iterate
	  (let* ((planner (feedback-planner-planner feedback-planner))
		 (add (cpd-planner-eval-function planner))
		 (domain (cpd-planner-domain planner))
		 (goal (caar (cpd-planner-added-goals planner)))
		 (k (cpd-planner-k planner)))
	    
	    (setf (feedback-planner-success-probability feedback-planner) new-prob)
	    (setf (feedback-planner-plan feedback-planner) new-plan)
	    
	    (funcall add '(pop 1))
	    (funcall add '(pop 1))
	    (funcall add `(assert (not ,(cpd-mangle-fluent domain (car limiting-action)
							(cdr limiting-action)))))
	    (funcall add '(push 1))
	    (cpd-smt-encode-goal add domain goal k)
	    (setf (cpd-planner-added-goals planner) (cdr (cpd-planner-added-goals planner)))
	    (setf (cpd-planner-remaining-goals planner)
		  (cons goal (cpd-planner-remaining-goals planner)))
	    (multiple-value-bind (plan sat planner)
		(cpd-plan-next planner)
	      (declare (ignore planner))
	      (if sat
		  (feedback-planner-next feedback-planner plan)
		  (feedback-planner-plan feedback-planner)))))))))

(defun feedback-search (operators facts &optional options)
  (let ((feedback-planner (feedback-planner-init operators facts
						 (or options (feedback-plan-options)))))
    (if feedback-planner
	(cpd-actions (feedback-planner-next feedback-planner
					    (feedback-planner-plan feedback-planner))))))
