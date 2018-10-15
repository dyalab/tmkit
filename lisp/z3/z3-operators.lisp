;;Define operators used by z3, but are not defined in lisp or a required lisp package.
(in-package :tmsmt)

(defun implies (a b)
  (or (not a) (and a b)))

(defun => (a b)
  (implies a b))

(defun iff (a b)
  (and (implies a b) (implies b a)))

(defun <=> (a b)
  (iff a b))

(defun power (a b)
  (expt a b))

(defun distinct (a &rest b)
  (if (equal b nil)
      t
      (and (not (member a b)) (distinct-list (car b) (cdr b)))))

(defun distinct-list (a b)
  (if (equal b nil)
      t
      (and (not (member a b)) (distinct-list (car b) (cdr b)))))

(defun z3->lisp (exp)
  (typecase exp
    (boolean exp)
    (symbol
     (ecase exp
       (:true t)
       (true t)
       (:sat t)
       (:false nil)
       (false nil)
       (:unsat nil)))
    (otherwise exp)))
