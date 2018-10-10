(in-package :cl-user)

(require :fiveam)

(defpackage path-checker/test
  (:use :tmsmt :cl :alexandria :cffi :smt-symbol :sycamore :fiveam))


(in-package :path-checker/test)

(def-suite all-tests
    :description "Path checking test suite")

(in-suite all-tests)

(defun test-path-checker ()
  (run! 'all-tests))

;;;map-tf tests
(test map-true-false
  (is (equal t (tmsmt::map-tf t)))
  (is (equal t (tmsmt::map-tf 'true)))
  (is (equal t (tmsmt::map-tf :true)))
  (is (equal nil (tmsmt::map-tf nil)))
  (is (equal nil (tmsmt::map-tf 'false)))
  (is (equal nil (tmsmt::map-tf :false))))

;;;get-value tests
(defun test-hash-get-value ()
  (let ((x (make-hash-table :test 'equal)))
    (setf (gethash '(a b) x) t)
    (tmsmt::get-value '(a b) x)))

(defun test-tree-get-value ()
  (let ((x (make-tree-map #'tmsmt::gsymbol-compare)))
    (setf x (tree-map-insert x '(a b) t))
    (tmsmt::get-value '(a b) x)))

(test get-value
  (is (equal t (test-hash-get-value)))
  (is (equal t (test-tree-get-value))))

;;; compile-transition tests
;;; (checks that compile transition accepts all inputs it should and the outputs are correct)
;;; does not check that for each symbol all outputs are correct
(defun test-compile (exp setf-func &optional (set-next nil))
  (let ((now (make-hash-table :test 'equal))
	(next (make-hash-table :test 'equal))
	(s-exp (tmsmt::compile-transition exp)))
    (if set-next
	(funcall setf-func next)
	(funcall setf-func now))
    (funcall s-exp now next)))

(test compile-tranansition
  (is (equal t (test-compile '((tmsmt::now a)) ;now
			     (lambda (x)
			       (setf (gethash 'a x) t)))))
  (is (equal t (test-compile '((tmsmt::next a)) ;next
			     (lambda (x)
			       (setf (gethash 'a x) t))
			     t)))
  (is (equal t (test-compile '((and (tmsmt::now a) (tmsmt::now b))) ;and
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) t)))))
  (is (equal t (test-compile '((or (tmsmt::now a) (tmsmt::now b))) ;or
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) nil)))))
  (is (equal t (test-compile '((not (tmsmt::now a))) ;not
			     (lambda (x)
			       (setf (gethash 'a x) nil)))))
  (is (equal t (test-compile '((xor (tmsmt::now a) (tmsmt::now b))) ;xor
			     (lambda (x)
			       (setf (gethash 'a x) nil)
			       (setf (gethash 'b x) t)))))
  (is (equal t (test-compile '((=> (tmsmt::now a) (tmsmt::now b))) ;=>
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) t)))))
  (is (equal t (test-compile '((<=> (tmsmt::now a) (tmsmt::now b))) ;<=>
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) t)))))
  (is (equal t (test-compile '((= (tmsmt::now a) (tmsmt::now b))) ;=
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) t)))))
  (is (equal t (test-compile '((and (or (tmsmt::now a) (tmsmt::now b)) (tmsmt::now c))) ;nested
			     (lambda (x)
			       (setf (gethash 'a x) nil)
			       (setf (gethash 'b x) t)
			       (setf (gethash 'c x) t)))))
  (is (equal t (test-compile '((and (tmsmt::now a) (tmsmt::now b)) ;listed (should be anded together)
			       (or (tmsmt::now a) (tmsmt::now b)))
			     (lambda (x)
			       (setf (gethash 'a x) t)
			       (setf (gethash 'b x) t)))))
  (is (equal nil (test-compile '((and (tmsmt::now a) (tmsmt::now b)) ;listed
				 (xor (tmsmt::now a) (tmsmt::now b)))
			       (lambda (x)
				 (setf (gethash 'a x) t)
				 (setf (gethash 'b x) t))))))


;;;test path checker routine
(defun test-cpdl-path-checker (&optional (tf t))
  (let ((h1 (make-hash-table :test 'equal))
	(h2 (make-hash-table :test 'equal))
	(h3 (make-hash-table :test 'equal))
	
	(sexp '((or (tmsmt::now a1) (tmsmt::next a1))
		(and (tmsmt::now a2) (tmsmt::next a2)))))
    
    (setf (getHash 'a1 h1) 'false)
    (setf (getHash 'a1 h2) t)
    (setf (getHash 'a1 h3) :false)
    
    (setf (getHash 'a2 h1) 'true)
    (setf (getHash 'a2 h2) t)
    (if tf
	(setf (getHash 'a2 h3) :true)
	(setf (getHash 'a2 h3) :false))
    (tmsmt::path-checker h1 (list h2 h3) (tmsmt::compile-transition sexp))))

(test cpdl-path-checker
  (is (equal t (test-cpdl-path-checker)))
  (is (equal nil (test-cpdl-path-checker nil))))

;;;testing parse cpdl plan
(defun make-and-parse-plan ()
  (let* ((cpdl-plan (tmsmt::cpd-plan (tmsmt::pddl-sat-domain
				      (merge-pathnames #p"demo/domains/blocksworld/tm-blocks.pddl"
						       tmsmt::*tmsmt-root*)
				      (merge-pathnames #p"demo/domains/blocksworld/tm-sussman.pddl"
						       tmsmt::*tmsmt-root*))))
	 (parsed-plan (tmsmt::parse-cpdl-plan cpdl-plan)))
    (check-plan cpdl-plan parsed-plan 0)))

(defun check-plan (cpdl-plan parsed-plan step)
  (cond
    ((and (equal parsed-plan nil) (equal cpdl-plan nil))
     t)
    ((equal parsed-plan nil)
     (if (equal (cdar cpdl-plan) :TRUE)
	 nil
	 (check-plan (cdr cpdl-plan) nil step)))
    ((equal cpdl-plan nil)
     nil)
    ((equal (cdar cpdl-plan) :TRUE)
     (if (equal (caar cpdl-plan) (cons step (car parsed-plan)))
	 (check-plan (cdr cpdl-plan) (cdr parsed-plan) (+ step 1))
	 nil))
    (t
     (check-plan (cdr cpdl-plan) parsed-plan step))))

(test parse-cpdl-plan
  (is (equal t (make-and-parse-plan))))


;;test recurse effects
(test recurse-effects
  (is (equal (tree-map-alist ;base
	      (tree-map-insert-alist (make-tree-map #'tmsmt::gsymbol-compare) '(((a b) . t))))
	     (tree-map-alist
	      (tmsmt::recurse-effects '(a b) (make-tree-map #'tmsmt::gsymbol-compare) t))))
  (is (equal (tree-map-alist ;not
	      (tree-map-insert-alist (make-tree-map #'tmsmt::gsymbol-compare) '(((a b) . nil))))
	     (tree-map-alist
	      (tmsmt::recurse-effects '(not (a b)) (make-tree-map #'tmsmt::gsymbol-compare) t))))
  (is (equal (tree-map-alist ;and
	      (tree-map-insert-alist (make-tree-map #'tmsmt::gsymbol-compare) '(((a b) . t)
										((b c) . t))))
	     (tree-map-alist
	      (tmsmt::recurse-effects '(and (a b) (b c))
				      (make-tree-map #'tmsmt::gsymbol-compare) t))))
  (is (equal (tree-map-alist ;=
	      (tree-map-insert-alist (make-tree-map #'tmsmt::gsymbol-compare) '(((a b) . (c d)))))
	     (tree-map-alist
	      (tmsmt::recurse-effects '(= (a b) (c d))
				      (make-tree-map #'tmsmt::gsymbol-compare) t))))
  (is (equal (tree-map-alist ;recursion
	      (tree-map-insert-alist (make-tree-map #'tmsmt::gsymbol-compare) '(((a b) . t)
										((b c) . nil))))
	     (tree-map-alist
	      (tmsmt::recurse-effects '(and (a b) (not (b c)))
				      (make-tree-map #'tmsmt::gsymbol-compare) t)))))

;;test pddl-path-checker
(test pddl-path-checker
  (is (equal t (tmsmt::pddl-path-check (merge-pathnames #p"demo/domains/blocksworld/tm-blocks.pddl"
							tmsmt::*tmsmt-root*)
				       (merge-pathnames #p"demo/domains/blocksworld/tm-sussman.pddl"
							tmsmt::*tmsmt-root*)
				       (tmsmt::parse-cpdl-plan
					(tmsmt::cpd-plan
					 (tmsmt::pddl-sat-domain
					  (merge-pathnames
					   #p"demo/domains/blocksworld/tm-blocks.pddl"
					   tmsmt::*tmsmt-root*)
					  (merge-pathnames
					   #p"demo/domains/blocksworld/tm-sussman.pddl"
					   tmsmt::*tmsmt-root*))))))))
				      


  
  
				       
			       
      
