(in-package :tmsmt)

(defstruct feedback-planner
  ;;a CPD planner to do the boolean planning
  planner

  ;;probability transition function to calculate the chance of success
  probability-transition-function

  ;;goal functions to calculate the probability we are in a goal state
  goal-functions

  ;;a hash-table containing the fluents at each timestep and their probabilities
  value-hash

  ;;a list of the fluents considered actions
  action-fluent-list

  ;;functions determining the value of a fluent at the next timestep given the current timestep
  fluent-propagation-list

  ;;a list of variable assignments to move the goal state
  plan

  ;;chance of plan succeeding
  (success-probability 0)

  ;;function to determine what set of actions to assert as not true in order to generate a new plan
  feedback-function

  ;;anything above true-thresh is considered deterministicly true
  true-thresh

  ;;anything below false-thresh is considered deterministicly false
  false-thresh

  ;;previous plans so searching isn't repeated
  (previous-plans nil))

(defun feedback-plan-options (&key (max-steps 10)
				(trace nil)
				(threshold .5)
				(feedback-func 'make-assert-not-weakest-step))
  `((:max-steps . ,max-steps)
    (:trace . ,trace)
    (:threshold . ,threshold)
    (:feedback-func . ,feedback-func)))

(defun feedback-plan-option (options thing)
  (cdr (assoc thing options)))

(defun collect-bool-function-names (op)
  (loop for x in (append (pddl-operators-functions op)
			 (pddl-operators-derived op))
     if (eq (pddl-function-type x) '|Bool|)
     collect (pddl-function-name x)))
  

(defun find-and-replace-probabilities (operators facts thresh)
  (let* ((func-names (collect-bool-function-names operators))
	 (init (pddl-facts-init facts))
	 (start-hash (make-hash-table :test 'equal))
	 (bool-init (reduce (lambda (ret var)
			      (destructuring-case var
				((= &rest args)				   
				 (assert (and (cdr args) (null (cddr args))))
				 (let ((f (car args))
				       (val (cadr args)))
				   (setf (gethash (cons 0 f) start-hash) val)
				   (if (and (consp f)
					    (member (caar args) func-names))
				       (if (> (cadr args) thresh)
					   (cons f ret)
					   ret)
				       (cons var ret))))
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


(defun make-assert-not-weakest-step (plan limiting-step domain)
  (loop for (action . performed) in plan
     if (and (eq performed :true)
	     (= limiting-step (car action)))
     collect (cpd-mangle-fluent domain (cdr action)
				limiting-step)))

(defun make-assert-not-whole-plan (plan limiting-step domain)
  (declare (ignore limiting-step))
  (loop for (action . performed) in plan
     if (eq performed :true)
     collect (cpd-mangle-fluent domain
				(cdr action) (car action))))
    
				      

(defun feedback-planner-init (operator facts options)
  (let* ((operator (load-operators operator))
	 (facts (load-facts facts))
	 (threshold (feedback-plan-option options :threshold))
	 (trace (feedback-plan-option options :trace))
	 (value-hash (find-and-replace-probabilities operator facts threshold))
	 (ground (ground-domain operator facts))
	 (cpd (ground->cpdl ground))
	 (variable-prop (create-variable-propagation-list ground))
	 (trans-func (s-exp->probability (cons 'and
					       (constrained-domain-transition-clauses cpd))
					 :if-symb 'if))
	 (goal-funcs (loop for goal in (constrained-domain-goal-clauses cpd)
			collect (compile-propagation-function
				 (s-exp->probability
				  (exp-now goal))))))

    (loop for key being the hash-key of (constrained-domain-start-map cpd)
       using (hash-value val)
       do (if (and (eq val 'false) (null (gethash (cons 0 key) value-hash)))
	      (setf (gethash (cons 0 key) value-hash) 0)))
    
    (z3::choose-solver (solver t :trace trace)
      (let ((planner (cpd-plan-init cpd solver options)))
	(make-feedback-planner :planner planner
			       :probability-transition-function (compile-propagation-function
								 trans-func)
			       :goal-functions goal-funcs
			       :value-hash value-hash
			       :action-fluent-list (map 'list #'pddl-sat-op
							(ground-domain-operators ground))
			       :fluent-propagation-list variable-prop
			       :plan nil
			       :feedback-function (feedback-plan-option options
									:feedback-func)
			       :true-thresh threshold
			       :false-thresh threshold)))))
      


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
	(propagate-transition-probabilities values trans-func fluent-propagation k)
      (let ((goal-probability (loop for (goal . timestep) in goals
				 for goal-func in goal-funcs
				 collect (funcall goal-func values timestep))))
	(values (eval (cons '* (cons transition-probability goal-probability)))
		limiting-step)))))

(defun propagate-transition-probabilities (values trans-func fluent-propagation max-steps)
  (labels ((propagate-recursive (cur-step trans-prob limiting-prob limiting-step)
	     (if (= cur-step max-steps)
		 (values trans-prob limiting-step)
		 (progn
		   (loop for (var . func) in fluent-propagation
		      do (let ((new-var (funcall func values cur-step)))
			   (setf (gethash (cons (+ 1 cur-step) var) values) new-var)))
		   (let ((step-trans (funcall trans-func values cur-step)))
		     (if (> limiting-prob step-trans)
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      step-trans cur-step)
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      limiting-prob limiting-step)))))))
    (propagate-recursive 0 1 1 0)))

(defun loosen-start-state (start-map prob-map true-thresh false-thresh)
  (loop for start-key being the hash-key of start-map
     using (hash-value start-val)
       if (or (eq start-val 'true) (eq start-val 'false))
     do (let ((prob-val (gethash (cons 0 start-key) prob-map)))
	  (cond
	    ((and (eq start-val 'true) (>= true-thresh prob-val))
	     (remhash start-key start-map))
	    ((and (eq start-val 'false) (<= false-thresh prob-val))
	     (remhash start-key start-map))))))
	  

(defun feedback-planner-loosen-threshold (feedback-planner)
  (let ((new-false (- (feedback-planner-false-thresh feedback-planner) .1))
	(new-true (+ (feedback-planner-true-thresh feedback-planner) .1)))
    (if (or (<= new-false 0) (>= new-true 1))
	(feedback-planner-plan feedback-planner)
	(let* ((planner (feedback-planner-planner feedback-planner))
	       (domain (cpd-planner-domain planner))
	       (start-map (constrained-domain-start-map domain))
	       (add (cpd-planner-eval-function planner))
	       (value-hash (feedback-planner-value-hash feedback-planner))
	       (checked-plans (feedback-planner-previous-plans feedback-planner)))

	  (format t "~%loosening threshold ~%")
	  (loosen-start-state start-map value-hash new-false new-true)

	  (setf (feedback-planner-true-thresh feedback-planner) new-true)
	  (setf (feedback-planner-false-thresh feedback-planner) new-false)
	  
	  (setf (cpd-planner-k planner) 0) ;;reset k to 0
	  
	  (funcall add '(pop 1))
	  (funcall add '(pop 1))
	  (cpd-smt-encode-start add domain)

	  (if checked-plans
	      (funcall add `(assert (not ,(cons 'or checked-plans)))))

	  (funcall add '(push 1))
	  (cpd-smt-encode-goal add domain (car (constrained-domain-goal-clauses domain)) 0)

	  (setf (cpd-planner-backtracking planner) t)

	  (feedback-planner-step feedback-planner)))))
    

(defun feedback-planner-next (feedback-planner new-plan)
  (let ((current-prob (feedback-planner-success-probability feedback-planner)))
    (multiple-value-bind (new-prob limiting-step)
	(calculate-plan-probability feedback-planner new-plan)
      (cond
	((= new-prob 1) ;;return current plan if it is certain
	 (feedback-planner-plan feedback-planner))
	
	(t ;;iterate
	 (let* ((planner (feedback-planner-planner feedback-planner))
		(add (cpd-planner-eval-function planner))
		(domain (cpd-planner-domain planner))
		(goal (caar (cpd-planner-added-goals planner)))
		(k (cpd-planner-k planner))
		(feedback-func (feedback-planner-feedback-function feedback-planner))
		(limiting-actions (funcall feedback-func new-plan limiting-step domain))
		(mangle-plan (cons 'and (cons (cpd-mangle-exp domain goal k)
					      limiting-actions))))

	   
	   ;;prefer plans that are less steps
	   (if (> new-prob current-prob)
	       (progn
		 (format t "~%Success probability: ~D ~%" new-prob)
		 (setf (feedback-planner-success-probability feedback-planner) new-prob)
		 (setf (feedback-planner-plan feedback-planner) new-plan)))
	   
	   (funcall add '(pop 1))
	   (funcall add '(pop 1))

	   (funcall add `(assert (not ,mangle-plan)))
	   
	   (funcall add '(push 1))
	      
	   (cpd-smt-encode-goal add domain goal k)
	   (setf (cpd-planner-added-goals planner) (cdr (cpd-planner-added-goals planner)))
	   (setf (cpd-planner-remaining-goals planner)
		 (cons goal (cpd-planner-remaining-goals planner)))

	   (setf (feedback-planner-previous-plans feedback-planner)
		 (cons mangle-plan
		       (feedback-planner-previous-plans feedback-planner)))
	   
	   (feedback-planner-step feedback-planner)))))))

(defun feedback-planner-step (feedback-planner)
  (let ((planner (feedback-planner-planner feedback-planner)))
    (multiple-value-bind (plan sat planner)
	(cpd-plan-next planner)
      (declare (ignore planner))
      (if sat
	  (feedback-planner-next feedback-planner plan)
	  (feedback-planner-loosen-threshold feedback-planner)))))

(defun feedback-search (operators facts &optional options)
  (let ((feedback-planner (feedback-planner-init operators facts
						 (or options (feedback-plan-options)))))
    (values feedback-planner (cpd-actions (feedback-planner-step feedback-planner)))))
