(in-package :tmsmt)

(defun pddl-sat-op (action)
  (cons (ground-action-name action)
        (ground-action-actual-arguments action)))

(defun pddl-sat-start (ground add-function)
  (destructuring-bind (-and &rest args) (ground-domain-start ground)
    (assert (eq 'and -and))
    (dolist (a args)
      (funcall add-function
               (if (atom a)
                   `(start ,a)
                   (destructuring-bind (op &rest args)
                       a
                     (case op
                       (not `(start ,a))
                       ;; TODO: check that we have a fluent
                       (= (assert (and (cdr args) (null (cddr args))))
                          `(start (= ,(first args) ,(second args))))
                       (otherwise
                        `(start ,a)))))))))

(defun pddl-sat-goal (ground add-function)
  (let* ((prob (ground-domain-probability-threshold ground))
	 (rewrite-func (lambda (x)
			 (list '> x prob)))
	(axioms (ground-domain-axioms ground)))
    (dolist (single-goal (ground-domain-goal ground))
      (if prob
	  (funcall add-function (apply-rewrite-exp rewrite-func (replace-axiom single-goal axioms)))
	  (funcall add-function (replace-axiom single-goal axioms))))))

(defun pddl-sat-frame (ground)
  ;(print ground-operators)
  (let* ((actions (ground-domain-operators ground))
         (fluents (ground-domain-variables ground))
         (hash (make-hash-table :test #'equal))  ;; hash: variable => (list modifiying-operators)
         (empty-set (make-tree-set #'gsymbol-compare))
	 (axioms (ground-domain-axioms ground)))
    
    ;; Index modified variables
    (loop for action in actions
       for op-exp = (fluent-now (pddl-sat-op action))
       do
         (loop for v in (ground-action-modified-variables action axioms)
            for set = (gethash v hash empty-set)
            for setp = (tree-set-insert set op-exp)
            do (setf (gethash v hash) setp)))
    
    ;; collect axioms
    (loop for f in fluents
       for now = (fluent-now f)
       for next = (fluent-next f)
       for eq = `(= ,now ,next)
       for actions = (tree-set-list (gethash f hash empty-set))
       collect
         (if actions
             `(or ,eq
                  ,(exp-or* actions))
             eq))))

(defun exclude-action (pre1 eff1 pre2 eff2)
  (cond
    ((intersection pre1 eff2 :test #'equal)
     t)
    ((intersection eff1 pre2 :test #'equal)
     t)
    ((intersection eff1 eff2 :test #'equal)
     t)
    (t
     nil)))

(defun bayes-rule (action effect other-actions)
  `(= ,(s-exp->probability (exp-next effect))
      (/ ,(s-exp->probability (exp-now action))
	 ,(s-exp->probability (exp-now (list 'not (cons 'or (cons effect other-actions))))))))

(defun pddl-sat-transition (ground add-function)
  (let* ((actions (ground-domain-operators ground))
         (action-fluents (map 'list #'pddl-sat-op actions))
	 (axioms (ground-domain-axioms ground))
	 (variable-type-tree (ground-domain-variable-type ground)))
    ;(with-collected (add)
    (flet ((add (arg) (funcall add-function arg)))
      ;; operator-encoding
      (loop for a in actions
         for op-exp in action-fluents
         do
           (let* ((pre (exp-now (replace-axiom (ground-action-precondition a) axioms)))
		  (eff (exp-next (replace-axiom (ground-action-effect a) axioms)))
		  (pre-used (find-variables pre variable-type-tree))
		  (eff-used (find-variables eff variable-type-tree)))
             (add
              `(or (not (now ,op-exp))          ; action not active
                   (and ,(or pre '|true|)       ; precondition holds
                        ,(if (equal '(and) eff) ; effect holds
                             '|true|
                             eff))))
	     ;; exclusion
	     (add `(=> ,(fluent-now op-exp)
		       (and ,@(loop for b in actions
				 for op-b in action-fluents
				 unless (or (equal op-exp op-b) (not (exclude-action
								      pre-used
								      eff-used
								      (find-variables
								       (replace-axiom
									(ground-action-precondition b)
									axioms) variable-type-tree)
								      (find-variables
								       (replace-axiom
									(ground-action-effect b)
									axioms) variable-type-tree))))
				 collect `(not ,(fluent-now op-b))))))))
      (add (cons 'or (loop for op-exp in action-fluents
		      collect `(now ,op-exp))))
      
      ;; frame
      (map nil #'add (pddl-sat-frame ground)))))

(defun replace-axiom (exp axiom-list)
  (typecase exp
    (atom exp)
    (t
     (let ((res nil))
       (dolist (a axiom-list)
	 (if (equal exp (cadr a))
	     (setf res (replace-axiom (caddr a) axiom-list))))
       (if (null res)
	   (dolist (x exp)
	     (setf res (append res (list (replace-axiom x axiom-list))))))
       res))))

(defun find-variables (exp var-tree)
  (cond
    ((eq exp nil)
     nil)
    ((numberp exp)
     nil)
    ((tree-map-find var-tree exp)
     (list exp))
    ((atom exp)
     nil)
    (t
     (loop for e in (cdr exp)
	  append (find-variables e var-tree)))))

(defun ground->cpdl (ground)
  (parse-cpdl
     (with-collected (add)
       ;; Fluents are probabilities not booleans
       (add `(probability-threshold ,(ground-domain-probability-threshold ground)))
       
       ;; State variables
       (do-map (k v (ground-domain-variable-type ground))
	 (if (is-bool v)
	     (add `(declare-fluent ,k ,v))
	     (add `(declare-fluent ,k |Real|))))
       
       ;; Action variables
       (dolist (a (ground-domain-operators ground))
	 (let ((a (pddl-sat-op a)))
	   (add `(declare-fluent ,a |Bool|))
	   (add `(output ,a))))
       
       ;; Start
       (pddl-sat-start ground #'add)
       
       ;; Goal
       (pddl-sat-goal ground (lambda (x) (add `(goal ,x))))
		   
       
       ;; Transition
       (pddl-sat-transition ground
			    (lambda (x) (add `(transition ,x))))

       ;; Metric
       (add `(metric ,(ground-domain-metric ground))))))

(defun pddl-sat-domain (operators facts)
  (let* ((operators (load-operators operators))
         (facts (load-facts facts))
         (ground (ground-domain operators facts)))
    (ground->cpdl ground)))


(defun pddl-sat (operators facts &optional options)
  (let ((cpd (pddl-sat-domain operators facts)))
    (multiple-value-bind (results sat)
        (cpd-plan cpd options)
      (values (when sat
                (cpd-actions results))
              sat))))
