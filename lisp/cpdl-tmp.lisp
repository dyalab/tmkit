(in-package :tmsmt)


(defun cpdl-tmp (&key
		   start-graph
		   start-q
		   domain
		   facts
		   options
		   planner-func
		   communication-func
		   motion-plan-func
		   scene-update-func)
  (format t "CPDL-TMP~%")
  (format t "Planning function: ~S~%" planner-func)
  (format t "Motion planning function: ~S~%" motion-plan-func)
  (format t "Communication function: ~S~%" communication-func)
  (format t "Scene update function: ~S~%" scene-update-func)
  (let (m-plan
	(start-graph start-graph)
	(start-q start-q))
    (labels ((new-plan (planner)
	       (multiple-value-bind (plan sat planner prob)
		   (funcall planner-func planner domain facts options)
		 (if sat
		     (refine plan planner prob)
		     m-plan)))
	     (refine (plan planner prob)
	       (format t "~a~%" plan)
	       (multiple-value-bind (new-start new-q)
		   (funcall scene-update-func start-graph start-q)
		 (setf start-graph new-start)
		 (setf start-q new-q)
		 (handler-case (setf m-plan
				     (funcall motion-plan-func plan start-graph start-q))

		   ;; No motion plan found
		   (planning-failure (e)
		     ;; TODO: add some other constraint
		     (declare (ignore e))
		     (new-plan planner))
		   (:no-error (e)
		     (declare (ignore e))
		     (funcall communication-func m-plan)
		     (if (numberp prob)
			 (if (> 1 prob)
			     (new-plan planner)
			     m-plan)
			 m-plan))))))
      (new-plan nil))))
