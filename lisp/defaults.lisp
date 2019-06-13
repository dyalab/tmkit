(in-package :tmsmt)


(defun default-cpdl-plan (planner domain facts options)
  (declare (ignore planner))
  (pddl-sat domain facts options))

(defun default-communication (plan)
  (format t "~S~%" (rope plan)))

(defun default-motion-plan (plan start-graph start-q)
  (tmp-refine plan start-graph start-q
	      :prefix-cache t))

(defun default-scene-update (sg q)
  (values sg q))

(defun default-probabilistic-cpdl-plan (planner domain facts options)
  (multiple-value-bind (plan sat planner prob)
      (if planner
	  (feedback-planner-step planner 'cpd-plan-next)
	  (let ((planner (feedback-planner-init domain facts (feedback-plan-options options))))
	    (values (feedback-planner-plan planner)
		    (if (feedback-planner-plan planner) t nil)
		    planner
		    (feedback-planner-success-probability planner))))
    (values (cpd-actions plan) sat planner prob)))
