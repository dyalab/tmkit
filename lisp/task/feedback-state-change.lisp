(in-package :tmsmt)


(defun add-object (object-type new-fluents facts)
  (let* ((objects (pddl-facts-objects facts))
	 (num (+ 1 (count object-type objects :test (lambda (type obj)
						      (eq (pddl-typed-type obj)
							  type)))))
	 (new-obj (make-pddl-typed
		   :name (intern (concatenate
				  'string
				  (format nil "~s" object-type)
				  (format nil "~D" num))
				 :tmsmt/pddl)
		   :type object-type))
	 (new-fluents (loop for fluent in new-fluents
			   collect (substitute (pddl-typed-name new-obj) '? fluent))))
    (push new-obj
	  (pddl-facts-objects facts))
    (setf (pddl-facts-init facts)
	  (append new-fluents
		  (pddl-facts-init facts)))))

(defun remove-object (object facts)
  (setf (pddl-facts-objects facts)
	(remove object (pddl-facts-objects facts) :test #'equalp))
  (let* ((name (pddl-typed-name object))
	 (init (pddl-facts-init facts))
	 (new-init (reduce (lambda (ret var)
			     (if (find name var :test #'equal)
				 ret
				 (cons var ret)))
			   init :initial-value nil)))
    (setf (pddl-facts-init facts) new-init)))


;; This is pretty inefficient TODO: make this better
(defun make-state-changes (feedback-planner add-objects remove-objects)
  (let ((facts (feedback-planner-facts feedback-planner))
	(domain (feedback-planner-operator feedback-planner)))

    (loop for (object new-fluents) in add-objects
       do (add-object object new-fluents facts))

    (loop for object in remove-objects
       do (remove-object object facts))

    (multiple-value-bind (prob-calc cpd)
	(probability-calculator-init domain facts)

      (let* ((old-planner (feedback-planner-planner feedback-planner))
	     (trace (cpd-plan-option (cpd-planner-options old-planner) :trace))
	     (initial-start  (copy-hash-table (constrained-domain-start-map cpd)))
	     (k       (feedback-planner-backtrack-k feedback-planner))
	     (cpd-opts     `((:max-steps . 1)(:trace . ,trace))))

	;; Intialize constrained domain and feedback planners
	(format t "~%making new cpd solver~%")
	(z3::choose-solver (solver t :trace trace)
	  (let* ((planner (cpd-plan-init cpd solver cpd-opts 2))
		 (add     (cpd-planner-eval-function planner))
		 (args    (cpd-planner-transition-args planner)))

	    (loop for i from 1 to k
	       do (cpd-smt-encode-fluents add cpd i)
	       do (cpd-smt-encode-transition add cpd args (1- i)))

	    (setf (feedback-planner-planner feedback-planner) planner)
	    (setf (feedback-planner-probability-calculator feedback-planner) prob-calc)
	    (setf (feedback-planner-initial-start feedback-planner) initial-start)))))))
