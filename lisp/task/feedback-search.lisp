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

  ;;the intial cpd hash, used for re-tightening bounds
  intial-start

  ;;a list of the fluents considered actions
  action-fluent-list

  ;;functions determining the value of a fluent at the next timestep given the current timestep
  fluent-propagation-list

  ;;new bounds that limit the search space perminately
  (new-bounds nil)

  ;;previous plans that have a low probability of success. Forgotten when we increase the step
  (low-plans nil)

  ;; A tree set containing plans sorted by the probability that the
  ;; transitions hold for each step. Elements are stored in the form (probability . plan)
  bound-set

  ;; A tree set containing plans sorted by the probability that the
  ;; plan results in the state being the goal state. Elements are
  ;; stored in the form (probability . plan)
  ;;plan-set

  ;;function to determine what set of actions to assert as not true in order to generate a new plan
  feedback-function

  ;;anything above true-thresh is considered deterministicly true
  true-thresh

  ;;anything below false-thresh is considered deterministicly false
  false-thresh

  ;;max-steps to go to with cpd-solving
  max-steps

  ;;a list of variable assignments to move the goal state
  plan

  ;;chance of plan succeeding
  (success-probability 0))

(defun feedback-plan-options (&key (max-steps 10)
				(trace nil)
				(feedback-func 'make-assert-not-up-to-weakest-step))
  `((:max-steps . ,max-steps)
    (:trace . ,trace)
    (:feedback-func . ,feedback-func)))

(defun feedback-plan-option (options thing)
  (cdr (assoc thing options)))

(defun compare-first-element (l1 l2)
  (cond
    ((= (car l1) (car l2))
     (if (equal (cdr l1) (cdr l2))
	 0
	 1))
    ((> (car l1) (car l2))
     1)
    (t
     -1)))

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


(defun feedback-planner-init (operator facts options)
  (let* ((operator (load-operators operator))
	 (facts (load-facts facts))
	 (threshold 0.5)
	 (trace (feedback-plan-option options :trace))
	 (cpd-opts `((:max-steps . 1)(:trace . ,trace)))
	 (value-hash (find-and-replace-probabilities operator facts threshold))
	 (ground (ground-domain operator facts))
	 (cpd (ground->cpdl ground))
	 (intial-start (copy-hash-table (constrained-domain-start-map cpd)))
	 (variable-prop (create-variable-propagation-list ground))
	 (trans-func (s-exp->probability (cons 'and
					       (constrained-domain-transition-clauses cpd))
					 :if-symb 'if))
	 (goal-funcs (loop for goal in (constrained-domain-goal-clauses cpd)
			collect (compile-propagation-function
				 (s-exp->probability
				  (exp-now goal))))))

    ;;set things that don't appear in the start state to a probability of 0
    (loop for key being the hash-key of (constrained-domain-start-map cpd)
       using (hash-value val)
       do (if (and (eq val 'false) (null (gethash (cons 0 key) value-hash)))
	      (setf (gethash (cons 0 key) value-hash) 0)))

    ;;intialize cpd and feedback planners
    (z3::choose-solver (solver t :trace trace)
      (let ((planner (cpd-plan-init cpd solver cpd-opts))
	    (succ-prob (eval (cons '*
				   (loop for goal-func in goal-funcs
				      collect (funcall
					       goal-func value-hash 0))))))
	(incf (cpd-planner-k planner))
	(make-feedback-planner :planner planner
			       :probability-transition-function (compile-propagation-function
								 trans-func)
			       :goal-functions goal-funcs
			       :value-hash value-hash
			       :intial-start intial-start
			       :action-fluent-list (map 'list #'pddl-sat-op
							(ground-domain-operators ground))
			       :fluent-propagation-list variable-prop
			       :plan nil
			       :success-probability succ-prob
			       :bound-set (make-tree-set #'compare-first-element)
			       ;;:plan-set (make-tree-set #'compare-first-element)
			       :feedback-function (feedback-plan-option options
									:feedback-func)
			       :true-thresh threshold
			       :false-thresh threshold
			       :max-steps (feedback-plan-option options :max-steps))))))



(defun calculate-plan-probability (feedback-planner plan)
  (let ((values (feedback-planner-value-hash feedback-planner))
	(fluent-propagation (feedback-planner-fluent-propagation-list feedback-planner))
	(k (cpd-planner-k (feedback-planner-planner feedback-planner)))
	(trans-func (feedback-planner-probability-transition-function feedback-planner))
	(goal-funcs (feedback-planner-goal-functions feedback-planner))
	(goals (cpd-planner-added-goals
		(feedback-planner-planner feedback-planner)))
	(succ-prob (feedback-planner-success-probability feedback-planner)))

    ;;set the probability of performing actions to true if performed
    (loop for (action . performed) in plan
       do (if (eq performed :true)
	      (setf (gethash action values) 1)
	      (setf (gethash action values) 0)))

    (multiple-value-bind (transition-probability limiting-step)
	(propagate-transition-probabilities values trans-func fluent-propagation k succ-prob)
      (let ((goal-probability (loop for (goal . timestep) in goals
				 for goal-func in goal-funcs
				 collect (funcall goal-func values timestep))))
	(values (eval (cons '* (cons transition-probability goal-probability)))
		transition-probability
		limiting-step)))))

(defun propagate-transition-probabilities (values trans-func fluent-propagation max-steps succ-prob)
  (labels ((propagate-recursive (cur-step trans-prob limiting-prob limiting-step)
	     (if (= cur-step max-steps)
		 (values trans-prob limiting-step)
		 (progn
		   ;;propagate probabilities
		   (loop for (var . func) in fluent-propagation
		      do (let ((new-var (funcall func values cur-step)))
			   (setf (gethash (cons (+ 1 cur-step) var) values) new-var)))

		   ;;recurse
		   (let ((step-trans (funcall trans-func values cur-step)))
		     (if (and (> succ-prob (* trans-prob step-trans))
			      (> limiting-step cur-step))
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      step-trans cur-step)
			 (propagate-recursive (+ cur-step 1) (* trans-prob step-trans)
					      limiting-prob limiting-step)))))))
    (propagate-recursive 0 1 1 (+ max-steps 1))))

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


(defun feedback-planner-modify-threshold (feedback-planner)
  (let* ((new-false  (- (feedback-planner-false-thresh feedback-planner) .1))
	 (new-true   (+ (feedback-planner-true-thresh feedback-planner) .1))
	 (planner    (feedback-planner-planner feedback-planner))
	 (domain     (cpd-planner-domain planner))
	 (max-steps  (feedback-planner-max-steps feedback-planner))
	 (add        (cpd-planner-eval-function planner))
	 (k          (cpd-planner-k planner))
	 (new-bounds (feedback-planner-new-bounds feedback-planner))
	 (low-plans  (feedback-planner-low-plans feedback-planner)))

    ;; Add in new bounds if they exist
    (funcall add '(pop 1))
    (funcall add '(pop 1))

    (if new-bounds
	(funcall add `(assert ,(cons 'and new-bounds))))
    (setf (feedback-planner-new-bounds feedback-planner) nil)

    ;; Figure out what to do next
    (cond
      ((and (or (<= new-false 0) (>= new-true 1))
	    (= (cpd-planner-k planner) max-steps))
       ;;quit if we have reached a bound and we have gone the max number of steps
       (feedback-planner-plan feedback-planner))

      ((or (<= new-false 0) (>= new-true 1))
       ;;increase time-horizon and start re-tighten thresholds
       (format t "~%re-tightening threshold~%")
       (setf (feedback-planner-true-thresh feedback-planner) 0.5)
       (setf (feedback-planner-false-thresh feedback-planner) 0.5)
       (setf (feedback-planner-low-plans feedback-planner)
	     (loop for i from 0 to k
		collect `(not ,(cpd-mangle-exp domain
					       (car (last (constrained-domain-goal-clauses domain)))
					       i))))



       (setf (cpd-planner-options planner)
	     `((:max-steps . ,(+ 1 k))
	       (:trace . (cpd-plan-option (cpd-planner options planner) :trace))))


       ;;reset hash to be what it was in the beginning
       (setf (constrained-domain-start-map domain)
	     (feedback-planner-intial-start feedback-planner))

       (cpd-smt-encode-transition add domain (cpd-planner-transition-args planner) (1- k))

       (cpd-smt-encode-start add domain)
       (funcall add `(assert ,(cons 'and low-plans)))
       (funcall add '(push 1))

       (incf (cpd-planner-k planner))
       (setf (cpd-planner-backtracking planner) nil)

       (feedback-planner-step feedback-planner))

      (t
       ;;otherwise loosen the bounds
       (let* ((start-map (constrained-domain-start-map domain))
	      (value-hash (feedback-planner-value-hash feedback-planner)))

	 (format t "~%loosening threshold ~%")
	 (loosen-start-state start-map value-hash new-true new-false)
	 (setf (constrained-domain-start-map domain) start-map)

	 (setf (feedback-planner-true-thresh feedback-planner) new-true)
	 (setf (feedback-planner-false-thresh feedback-planner) new-false)


	 (cpd-smt-encode-start add domain)
	 ;; Don't recheck things that we've already tried
	 (if low-plans
	     (funcall add `(assert ,(cons 'and low-plans))))
	 (funcall add '(push 1))

	 (setf (cpd-planner-backtracking planner) t)

	 (feedback-planner-step feedback-planner))))))

(defun check-and-add-bounds (feedback-planner new-prob)
  (let* ((add (cpd-planner-eval-function (feedback-planner-planner feedback-planner)))
	 (bound-tree (feedback-planner-bound-set feedback-planner))
	 (new-bounds ()))
    (labels ((rec (tree)
	       (if (not (= (tree-set-count tree) 0))
		   (progn
		     (let* ((min-el (tree-set-min tree))
			    (min    (car min-el))
			    (bound  (cdr min-el)))
		       (if (< min new-prob)
			   (progn
			     (push bound new-bounds)
			     (rec (tree-set-remove-min tree)))
			   tree)))
		   tree)))
      (setf (feedback-planner-bound-set feedback-planner) (rec bound-tree))
      (if new-bounds
	  (progn
	    (setf (feedback-planner-new-bounds feedback-planner)
		  (append new-bounds (feedback-planner-new-bounds feedback-planner)))
	    (funcall add `(assert ,(cons 'and new-bounds))))))))


(defun feedback-planner-next (feedback-planner new-plan)
  (let ((current-prob (feedback-planner-success-probability feedback-planner)))
    (multiple-value-bind (new-prob trans-prob limiting-step)
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
		(bound-assert `(not ,(cons 'and limiting-actions))))

	   ;; Add the new plan to the tree set
	   ;; (setf (feedback-planner-plan-set feedback-planner)
	   ;; 	 (tree-set-insert (feedback-planner-plan-set feedback-planner)
	   ;; 			  (cons new-prob new-plan)))

	   (funcall add '(pop 1))
	   (funcall add '(pop 1))

	   ;;prefer plans that are less steps
	   (if (> new-prob current-prob)
	       (progn
		 (format t "~%Success probability: ~D ~%" new-prob)
		 (setf (feedback-planner-success-probability feedback-planner) new-prob)
		 (setf (feedback-planner-plan feedback-planner) new-plan)
		 (check-and-add-bounds feedback-planner new-prob)))

	   ;; Bound the search tree
	   (funcall add `(assert ,bound-assert))

	   (funcall add '(push 1))

	   ;; Re-add last goal
	   (cpd-smt-encode-goal add domain goal k)
	   (setf (cpd-planner-added-goals planner) (cdr (cpd-planner-added-goals planner)))
	   (setf (cpd-planner-remaining-goals planner)
		 (cons goal (cpd-planner-remaining-goals planner)))

	   ;; Add the new info to new bounds to eventually be
	   ;; asserted as not true before the start only if we got to a
	   ;; lower probability before the final step
	   (if ( < trans-prob current-prob)
		 (setf (feedback-planner-new-bounds feedback-planner)
		       (cons bound-assert
			     (feedback-planner-new-bounds feedback-planner)))
		 (progn
		   (setf (feedback-planner-low-plans feedback-planner)
			 (cons bound-assert
			       (feedback-planner-low-plans feedback-planner)))
		   (setf (feedback-planner-bound-set feedback-planner)
		       (tree-set-insert (feedback-planner-bound-set feedback-planner)
					(cons trans-prob bound-assert)))))

	   (multiple-value-bind (plan sat planner)
	       (cpd-plan-next planner)
	     (declare (ignore planner))
	     (if sat
		 (feedback-planner-next feedback-planner plan)
		 (feedback-planner-modify-threshold feedback-planner)))))))))

(defun feedback-planner-step (feedback-planner)
  (let ((planner (feedback-planner-planner feedback-planner)))
    (multiple-value-bind (plan sat planner)
	(cpd-plan-step planner)
      (declare (ignore planner))
      (if sat
	  (feedback-planner-next feedback-planner plan)
	  (feedback-planner-modify-threshold feedback-planner)))))

(defun feedback-search (operators facts &optional options)
  (let ((feedback-planner (feedback-planner-init operators facts
						 (or options (feedback-plan-options)))))
    (format t "~%Initial success chance ~D~%" (feedback-planner-success-probability feedback-planner))
    (values feedback-planner (cpd-actions (feedback-planner-step feedback-planner)))))
