(in-package :tmsmt)

(defun pddl-path-check (operator facts path-actions)
  (let* ((cpdl (pddl-sat-domain operator facts))
	 (func (compile-transition (constrained-domain-transition-clauses cpdl)))
	 (ground (ground-domain operator facts))
	 (start (recurse-effects (ground-domain-start ground)
				 (make-tree-map #'gsymbol-compare) t)))
    (action-state-checker start func path-actions (create-ground-action-hash ground))))

(defun action-state-checker (now-state func path-actions ground-actions)
  (if (equal nil path-actions)
      t
      (let ((next-state (make-next-state now-state (car path-actions) ground-actions)))
	(setf now-state (tree-map-insert now-state (car path-actions) t))
	(and (funcall func now-state next-state)
	     (action-state-checker next-state func (cdr path-actions) ground-actions)))))

(defun make-next-state (now-state path-action ground-actions)
  (let ((next-state now-state)
	(ground-action (gethash path-action ground-actions)))
    (assert (not (equal ground-action nil)) (ground-action) "No action found")
    (recurse-effects (ground-action-effect ground-action) next-state t)))

(defun recurse-effects (effects next-map truth)
  (cond
    ((equal 'not (car effects))
     (recurse-effects (car (cdr effects)) next-map (not truth)))
    ((equal 'and (car effects))
     (dolist (effect (cdr effects))
       (setf next-map (recurse-effects effect next-map truth)))
     next-map)
    ((equal '= (car effects))
     (tree-map-insert next-map (second effects) (third effects)))
    (t
     (tree-map-insert next-map effects truth))))

(defun parse-cpdl-plan (plan)
    (loop for (step . tf) in plan
      if (eq tf :TRUE)
	 collect (cdr step)))

(defun create-ground-action-hash (ground)
  (let* ((ground-hash (make-hash-table :test 'equal)))
    (map nil (lambda (ground-action) (setf (gethash
					      (cons (ground-action-name ground-action)
						    (ground-action-actual-arguments ground-action))
					      ground-hash) ground-action))
	 (ground-domain-operators ground))
    ground-hash))
