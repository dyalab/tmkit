(in-package :tmsmt)

(defun test-state-change (feedback-planner)
  (let ((val-hash (probability-calculator-value-hash
		   (feedback-planner-probability-calculator feedback-planner))))
    (if (and (= (cpd-planner-k (feedback-planner-planner feedback-planner)) 4)
	     (= (gethash '(0 tmsmt/pddl::on tmsmt/pddl::c tmsmt/pddl::a)
			 val-hash) 0.7))
      (progn
	(format t "changing intial state~%")
	(setf (gethash '(0 tmsmt/pddl::holding tmsmt/pddl::c)
		       val-hash) 0)
	(setf (gethash '(0 tmsmt/pddl::clear tmsmt/pddl::c)
		       val-hash) 1)
	(setf (gethash '(0 tmsmt/pddl::handempty)
		       val-hash) 1)
	(setf (gethash '(0 tmsmt/pddl::ontable tmsmt/pddl::c tmsmt/pddl::z)
		       val-hash) 1)

	(setf (gethash '(0 tmsmt/pddl::clear tmsmt/pddl::a)
		       val-hash) 1)
	(setf (gethash '(0 tmsmt/pddl::on tmsmt/pddl::c tmsmt/pddl::a)
		       val-hash) 0)))))

(defun test-add-object (feedback-planner)
  (if (and (= (cpd-planner-k (feedback-planner-planner feedback-planner)) 3)
	   (not ( gethash '(0 tmsmt/pddl::have-mover tmsmt/pddl::mover1)
			  (probability-calculator-value-hash
			   (feedback-planner-probability-calculator feedback-planner)))))
      (make-state-changes feedback-planner `((mover  ((tmsmt/pddl::have-mover ?)))) nil)))

(defun test-remove-object (feedback-planner)
  (if (and (= (cpd-planner-k (feedback-planner-planner feedback-planner)) 3)
	   (gethash '(0 tmsmt/pddl::have-mover tmsmt/pddl::mover1)
		    (probability-calculator-value-hash
		     (feedback-planner-probability-calculator feedback-planner))))
      (make-state-changes feedback-planner nil `(,(make-pddl-typed
						:name 'tmsmt/pddl::mover1
						:type 'tmsmt/pddl::mover)))))
