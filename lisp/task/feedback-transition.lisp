(in-package :tmsmt)

(defstruct probability-calculator
  ;; Probability transition function to calculate the chance of
  ;; success
  probability-transition-function

  ;; Goal functions to calculate the probability we are in a goal
  ;; state
  goal-functions

  ;; A hash-table containing the fluents at each timestep and their
  ;; probabilities
  value-hash

   ;; List of the fluents considered actions
  action-fluent-list

  ;; Functions determining the value of a fluent at the next timestep
  ;; given the current timestep
  fluent-propagation-list)

;; Function compilation functions
(defun compile-propagation-function (exp)
  (eval `(lambda (var-hash time-step)
	   (macrolet ((now (x) `(get-value-at-timestep (quote ,x) var-hash time-step))
		      (next (x) `(get-value-at-timestep (quote ,x) var-hash (+ 1 time-step))))
	     ,exp))))

(defun create-variable-propagation-list (ground)
  (let* ((action-list (ground-domain-operators ground))
	 (variables (ground-domain-variables ground))
	 (action-fluents (map 'list #'pddl-sat-op action-list))
	 (true-hash (make-hash-table :test #'equal))
	 (false-hash (make-hash-table :test #'equal)))

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

(defun reset-actions (actions k value-hash)
  (loop for i from 0 to k
     do (loop for action in actions
	   do (setf (gethash (cons i action) value-hash) 0))))



(defun find-and-replace-probabilities (facts &key (start-hash (make-hash-table :test 'equal)))
  (let* ((init (pddl-facts-init facts))
	 (thresh 0.5)
	 (bool-init (reduce (lambda (ret var)
			      (destructuring-case var
				(('tmsmt/pddl::probabilistic val &rest args)
				 (assert (numberp val))
				 (setf (gethash (cons 0 (car args)) start-hash) val)
				 (if (> val thresh)
				     (cons (car args) ret)
				     ret))
				((t &rest args)
				 (declare (ignore args))
				 (setf (gethash (cons 0 var) start-hash) 1)
				 (cons var ret))))
			    init :initial-value nil)))
    (values start-hash
	    (make-pddl-facts
	     :name (pddl-facts-name facts)
	     :domain (pddl-facts-domain facts)
	     :objects (pddl-facts-objects facts)
	     :type-map (pddl-facts-type-map facts)
	     :init bool-init
	     :goal (pddl-facts-goal facts)
	     :metric (pddl-facts-metric facts)
	     :probability-threshold (pddl-facts-probability-threshold facts)))))

;; Helper function for success chance
(defun calculate-plan-probability (trans-calc plan k succ-prob)
  (let* ((values             (probability-calculator-value-hash trans-calc))
	 (actions            (probability-calculator-action-fluent-list trans-calc))
	 (fluent-propagation (probability-calculator-fluent-propagation-list trans-calc))
	 (trans-func         (probability-calculator-probability-transition-function trans-calc))
	 (goal-funcs         (probability-calculator-goal-functions trans-calc)))

    ;; Set the probability of performing actions to true if performed
    (reset-actions actions k values)
    (loop for (action . performed) in plan
       do (if (eq performed :true)
	      (setf (gethash action values) 1)
	      (setf (gethash action values) 0)))

    (multiple-value-bind (transition-probability limiting-step)
	(propagate-transition-probabilities values trans-func fluent-propagation k succ-prob)
      (let ((goal-probability (loop for goal-func in goal-funcs
				     collect (funcall goal-func values k))))
	(values (eval (cons '* (cons transition-probability goal-probability)))
		transition-probability
		limiting-step)))))

(defun propagate-transition-probabilities (values trans-func
					   fluent-propagation max-steps succ-prob)
  (labels ((propagate-recursive (cur-step trans-prob limiting-prob limiting-step)
	     (if (= cur-step max-steps)
		 (values trans-prob limiting-step)
		 (progn
		   ;; Propagate probabilities
		   (loop for (var . func) in fluent-propagation
		      do (let ((new-var (funcall func values cur-step)))
			   (setf (gethash (cons (+ 1 cur-step) var) values) new-var)))

		   ;; Recurse
		   (let ((step-trans (funcall trans-func values cur-step)))
		     (if (and (> succ-prob (* trans-prob step-trans))
			      (> limiting-step cur-step))
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      step-trans cur-step)
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      limiting-prob limiting-step)))))))
    (propagate-recursive 0 1 1 (+ max-steps 1))))


(defun probability-calculator-init (operator facts)
  (let* ((operator (load-operators operator))
	 (facts    (load-facts facts)))
    (multiple-value-bind (value-hash facts)
	(find-and-replace-probabilities facts)
	 (let* ((ground        (ground-domain operator facts))
		(variable-prop (create-variable-propagation-list ground))
		(cpd           (ground->cpdl ground))
		(trans-func    (s-exp->probability (cons 'and
						  (constrained-domain-transition-clauses cpd))
					    :if-symb 'if))
		(goal-funcs    (loop for goal in (constrained-domain-goal-clauses cpd)
			   collect (compile-propagation-function
				    (s-exp->probability
				     (exp-now goal))))))

    ;; Set things that don't appear in the start state to a probability of 0
    (loop for key being the hash-key of (constrained-domain-start-map cpd)
       using (hash-value val)
       do (if (and (eq val 'false) (null (gethash (cons 0 key) value-hash)))
	      (setf (gethash (cons 0 key) value-hash) 0)))

    (values (make-probability-calculator
	     :probability-transition-function (compile-propagation-function
					       trans-func)
	     :goal-functions goal-funcs
	     :value-hash value-hash
	     :action-fluent-list (map 'list #'pddl-sat-op
							(ground-domain-operators ground))
	     :fluent-propagation-list variable-prop)
	    cpd)))))
