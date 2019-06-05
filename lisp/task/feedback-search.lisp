(in-package :tmsmt)

(defstruct feedback-planner
  ;; CPD planner to do the boolean planning
  planner

  facts

  operator

  probability-calculator

  ;; Initial cpd hash, used for re-tightening bounds
  initial-start

  ;; New bounds that limit the search space perminately
  (new-bounds nil)

  ;; Previous plans that have a low probability of success. Forgotten
  ;; when we increase the time step
  (low-plans nil)

  ;; A tree set containing plans sorted by the probability that the
  ;; transitions hold for each step. Elements are stored in the form (probability . plan)
  bound-set

  ;; A list of plans that we have already checked. Used when we have
  ;; to restart planning.
  (previous-plans nil)

  ;; Function to determine what set of actions to assert as not true
  ;; in order to generate a new plan
  feedback-function

  ;; Function for testing/modifying the start state
  state-change-function

  ;; Function to call if a new plan is found. Takes in the new plan and the success probability
  new-plan-function

  ;; Anything above true-thresh is considered deterministicly true
  true-thresh

  ;; Anything below false-thresh is considered deterministicly false
  false-thresh

  ;; Max-steps to go to with cpd-solving
  max-steps

  ;; what timestep we got to before we restarted
  (backtrack-k 1)

  ;; Best plan
  plan

  ;; length of the best plan
  (best-k 0)

  ;; Chance of plan succeeding
  (success-probability 0))

;; General utility functions

(defun feedback-plan-options (options)
  `((:max-steps .         ,(or (feedback-plan-option options :max-steps)
			       10))
    (:trace .             ,(feedback-plan-option options :trace))
    (:feedback-func .     ,(or (feedback-plan-option options :feedback-func)
			       'make-assert-not-up-to-weakest-step))
    (:state-change-func . ,(or (feedback-plan-option options :state-change-func)
			       (lambda (x) (declare (ignore x)))))
    (:new-plan-func . ,(or (feedback-plan-option options :new-plan-func)
			   (lambda (x y) (declare (ignore x y)))))))

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

(defun only-true (plan)
  (loop for (action . performed) in plan
     if (eq performed :true)
     collect (cons action  performed)))



;; ;; Helper functions for converting between probabilities and Booleans in the start state

;; (defun collect-bool-function-names (op)
;;   (loop for x in (append (pddl-operators-functions op)
;; 			 (pddl-operators-derived op))
;;      if (eq (pddl-function-type x) '|Bool|)
;;      collect (pddl-function-name x)))

;; Helper function for modifiying the start state sent to CPD planner

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
       ;; Quit if we have reached a bound and we have gone the max number of steps
       (feedback-planner-plan feedback-planner))

      ((or (<= new-false 0) (>= new-true 1))
       ;; Increase time-horizon and start re-tighten thresholds
       (format t "~%re-tightening threshold~%")
       (setf (feedback-planner-true-thresh feedback-planner) 0.5)
       (setf (feedback-planner-false-thresh feedback-planner) 0.5)

       ;; Don't check plans where we reach the goal state in a previous timestep
       (setf (feedback-planner-low-plans feedback-planner)
	     (loop for i from 0 to k
		collect `(not ,(cpd-mangle-exp domain
					       (car (last (constrained-domain-goal-clauses domain)))
					       i))))
       (setf low-plans (feedback-planner-low-plans feedback-planner))



       (setf (cpd-planner-options planner)
	     `((:max-steps . ,(+ 1 k))
	       (:trace . ,(cpd-plan-option (cpd-planner-options planner) :trace))))


       ;; Reset hash to be what it was in the beginning
       (setf (constrained-domain-start-map domain)
	     (copy-hash-table (feedback-planner-initial-start feedback-planner)))

       (cpd-smt-encode-transition add domain (cpd-planner-transition-args planner) (1- k))

       (cpd-smt-encode-start add domain)
       (funcall add `(assert ,(cons 'and low-plans)))
       (funcall add '(push 1))

       (incf (cpd-planner-k planner))
       (when (> (cpd-planner-k planner)
	      (feedback-planner-backtrack-k feedback-planner))
	   (setf (feedback-planner-backtrack-k feedback-planner) (cpd-planner-k planner))
	   (setf (cpd-planner-backtracking planner) nil))

       (feedback-planner-step feedback-planner))

      (t
       ;; Otherwise loosen the bounds
       (let* ((start-map (constrained-domain-start-map domain))
	      (value-hash (probability-calculator-value-hash
			   (feedback-planner-probability-calculator feedback-planner))))

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



;; State change code

(defun check-state (feedback-planner)
  (if (funcall (feedback-planner-state-change-function feedback-planner) feedback-planner)
      (recalculate-probabilities feedback-planner)))

(defun recalculate-probabilities (feedback-planner)
  (format t "recalculating state~%")
  (let* ((prev-plans    (feedback-planner-previous-plans feedback-planner))
	 (best-plan     (feedback-planner-plan feedback-planner))
	 (best-k        (feedback-planner-best-k feedback-planner))
	 (feedback-func (feedback-planner-feedback-function feedback-planner))
	 (prob-calc     (feedback-planner-probability-calculator feedback-planner))
	 (planner       (feedback-planner-planner feedback-planner))
	 (domain        (cpd-planner-domain planner))
	 (bounds        (make-tree-set #'compare-first-element)))
    (multiple-value-bind (best-prob ign1 ign2)
	(calculate-plan-probability prob-calc best-plan best-k 1)
      (declare (ignore ign1 ign2))
      (setf (feedback-planner-success-probability feedback-planner) best-prob)

      ;; It is possible (likely) that we just performed an action in
      ;; the best plan, resulting in the persieved state changing. So
      ;; lets add one action iteration to prev-plans to be considered.

      (if (> best-k 0)
	  (push (cons (- best-k 1) (loop for (action . performed) in best-plan
				      if (and (not (= (car action) 0))
					      (eq performed :true))
				      collect (cons (cons (- (car action) 1)
							  (cdr action))
						    :true)))
		prev-plans))

      (setf (feedback-planner-new-bounds feedback-planner)
	    (remove nil (loop for (k . plan) in prev-plans
			   collect (multiple-value-bind (prob trans-prob limiting-step)
				       (calculate-plan-probability prob-calc plan k best-prob)
				     (let* ((limiting-actions
					     (funcall feedback-func plan limiting-step domain))
					    (bound-assert `(not ,(cons 'and limiting-actions))))
				       (if (> prob best-prob)
					   (progn
					     (setf (feedback-planner-plan feedback-planner) plan)
					     (setf (feedback-planner-best-k feedback-planner) k)
					     (setf best-prob prob)
					     (setf (feedback-planner-success-probability
						    feedback-planner) prob)))
				       (if (< trans-prob best-prob)
					   bound-assert
					   (progn
					     (setf bounds (tree-set-insert bounds
									   (cons trans-prob
										 bound-assert)))
					     nil)))))))
      (setf (feedback-planner-bound-set feedback-planner) bounds)
      (check-bounds feedback-planner best-prob)
      t)))

(defun restart-planning (feedback-planner)
  (let* ((planner (feedback-planner-planner feedback-planner))
	 (domain  (cpd-planner-domain planner))
	 (add     (cpd-planner-eval-function planner)))

    ;; Reset the timestep to 1
    (setf (cpd-planner-k planner) 1)
    (setf (cpd-planner-options planner)
	  `((:max-steps . 1)
	    (:trace . ,(cpd-plan-option (cpd-planner-options planner) :trace))))

    (setf (feedback-planner-low-plans feedback-planner)
	  (list `(not ,(cpd-mangle-exp domain
				       (car (last (constrained-domain-goal-clauses domain)))
				       0))))

    (funcall add '(pop 1))
    (funcall add '(push 1))

    (setf (feedback-planner-true-thresh feedback-planner)  0.4)
    (setf (feedback-planner-false-thresh feedback-planner) 0.6)
    (setf (cpd-planner-backtracking planner) t)
    (feedback-planner-modify-threshold feedback-planner)))

;; Bounding code

(defun check-bounds (feedback-planner new-prob)
  (let* ((bound-tree (feedback-planner-bound-set feedback-planner))
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
		  (append new-bounds (feedback-planner-new-bounds feedback-planner)))))
      new-bounds)))

;; Feedback planner main code

(defun feedback-planner-init (operator facts options)
  (let* ((trace         (feedback-plan-option options :trace))
	 (cpd-opts     `((:max-steps . 1)(:trace . ,trace))))

    (multiple-value-bind (prob-calc cpd)
	(probability-calculator-init operator facts)
      (let* ((initial-start (copy-hash-table (constrained-domain-start-map cpd)))
	     (goal-funcs    (probability-calculator-goal-functions prob-calc))
	     (value-hash    (probability-calculator-value-hash prob-calc))
	     (succ-prob     (eval (cons '*
					(loop for goal-func in goal-funcs
					   collect (funcall
						    goal-func value-hash 0))))))

	;; Initialize constrained domain and feedback planners
    (z3::choose-solver (solver t :trace trace)
      (let ((planner (cpd-plan-init cpd solver cpd-opts 2)))
	(incf (cpd-planner-k planner))
	(make-feedback-planner :planner planner
			       :probability-calculator prob-calc
			       :initial-start initial-start
			       :plan nil
			       :success-probability succ-prob
			       :bound-set (make-tree-set #'compare-first-element)
			       :feedback-function (feedback-plan-option options
									:feedback-func)
			       :state-change-function (feedback-plan-option options
									    :state-change-func)
			       :new-plan-function (feedback-plan-option options
									:new-plan-func)
			       :true-thresh 0.5
			       :false-thresh 0.5
			       :max-steps (feedback-plan-option options :max-steps)
			       :facts (load-facts facts)
			       :operator (load-operators operator))))))))


(defun feedback-planner-next (feedback-planner new-plan)
  (let* ((planner      (feedback-planner-planner feedback-planner))
	 (k            (cpd-planner-k planner))
	 (current-prob (feedback-planner-success-probability feedback-planner)))
    (multiple-value-bind (new-prob trans-prob limiting-step)
	(calculate-plan-probability (feedback-planner-probability-calculator feedback-planner)
				    new-plan k current-prob)
      (cond
	((= new-prob 1) ;; Return current plan if it is certain
	 (progn
	   (setf (feedback-planner-success-probability feedback-planner) new-prob)
	   (setf (feedback-planner-plan feedback-planner) new-plan)
	   (setf (feedback-planner-best-k feedback-planner) k)
	   new-plan))
	(t ;; Iterate
	 (let* ((domain           (cpd-planner-domain planner))
		(add              (cpd-planner-eval-function planner))
		(goal             (caar (cpd-planner-added-goals planner)))
		(feedback-func    (feedback-planner-feedback-function feedback-planner))
		(limiting-actions (funcall feedback-func new-plan limiting-step domain))
		(bound-assert    `(not ,(cons 'and limiting-actions)))
		(new-plan         (only-true new-plan)))

	   (push (cons k new-plan)
		 (feedback-planner-previous-plans feedback-planner))

	   (funcall add '(pop 1))
	   (funcall add '(pop 1))

	   ;; Prefer plans that are less steps
	   (if (> new-prob current-prob)
	       (progn
		 (format t "~%Success probability: ~D ~%" new-prob)
		 (funcall (feedback-planner-new-plan-function feedback-planner) new-plan new-prob)
		 (setf (feedback-planner-success-probability feedback-planner) new-prob)
		 (setf (feedback-planner-plan feedback-planner) new-plan)
		 (setf (feedback-planner-best-k feedback-planner) k)
		 (let ((new-bounds (check-bounds feedback-planner new-prob)))
		   (if new-bounds
		       (funcall add `(assert ,(cons 'and new-bounds)))))))

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
	   ;; lower probability in the transition portion
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

	   (feedback-planner-step feedback-planner 'cpd-plan-next)))))))

(defun feedback-planner-step (feedback-planner &optional (step-func 'cpd-plan-step))
  (if (check-state feedback-planner)
      (restart-planning feedback-planner)
      (let ((planner (feedback-planner-planner feedback-planner)))
	(multiple-value-bind (plan sat planner)
	    (funcall step-func planner)
	  (declare (ignore planner))
	  (if sat
	      (feedback-planner-next feedback-planner plan)
	      (feedback-planner-modify-threshold feedback-planner))))))


(defun feedback-search (operators facts &optional options)
  (let ((feedback-planner (feedback-planner-init operators facts
						 (feedback-plan-options options))))
    (format t "~%Initial success chance ~D~%" (feedback-planner-success-probability feedback-planner))
    (values feedback-planner (cpd-actions (feedback-planner-step feedback-planner)))))
