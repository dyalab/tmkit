(in-package :z3)

;; (defstruct (smt-state (:constructor %make-smt-state (context solver model)))
;;   context
;;   solver
;;   model)

;; (defun make-smt-state ()
;;   (let* ((context (z3-mk-context (z3-mk-config)))
;;          (solver (z3-mk-solver context)))
;;     (%make-smt-state context solver nil)))

(defcallback error-callback
    :void
    ((context z3-context-type)
     (code z3-error-code))
  (declare (ignore context))
  (error "Z3 Error: ~A" code))


(define-condition smt-error (error)
  ((message :initarg :message :reader message)
   (expression :initarg :expression :reader expression)))

(defmethod print-object ((e smt-error) stream)
  (print-unreadable-object (e stream :type t :identity t)
    (when (slot-boundp e 'message)
      (princ (slot-value e 'message )  stream))))

(defun smt-error (fmt &rest args)
  (error 'smt-error
         ;:expression expression
         :message (apply #'format nil fmt args)))


(defmacro with-filled-array ((array-variable count-variable)
                             ((elt-variable sequence) &body elt-body)
                             &body body)
  (with-gensyms (seq-var i)
    `(let* ((,seq-var ,sequence)
            (,count-variable (length ,seq-var)))
       (with-foreign-object (,array-variable :pointer ,count-variable)
           (reduce (lambda (,i ,elt-variable)
                     (setf (mem-aref ,array-variable :pointer ,i)
                           (progn ,@elt-body))
                     (1+ ,i))
               ,seq-var
               :initial-value 0)
         ;; body
         ,@body))))


(defun make-solver (&key context trace)
  (let* ((context (or context (z3-mk-context (z3-mk-config))))
         (solver (z3-mk-solver context)))
    ;; Seems that we get an initial refcount of 0?
    (z3-solver-inc-ref context solver)
    (z3-set-error-handler context (callback error-callback))
    (setf (z3-solver-context solver) context
          (z3-solver-trace solver) trace)
    solver))

(defun make-optimizer (&key context trace)
  (let* ((context (or context (z3-mk-context (z3-mk-config))))
         (solver (z3-mk-optimize context)))
    ;; Seems that we get an initial refcount of 0?
    (z3-solver-inc-ref context solver)
    (z3-set-error-handler context (callback error-callback))
    (setf (z3-optimize-context solver) context
          (z3-optimize-trace solver) trace)
    solver))

(defmacro choose-solver ((solver use-solver &key trace) &body body)
  `(if ,use-solver
      (with-solver (,solver :trace ,trace)
	 ,@body)
      (with-optimizer (,solver :trace ,trace)
	 ,@body)))

(defmacro with-solver ((solver &key trace) &body body)
  `(let ((,solver (make-solver :trace ,trace)))
     ,@body))

(defmacro with-optimizer ((solver &key trace) &body body)
  `(let ((,solver (make-optimizer :trace ,trace)))
     ,@body))


(defvar *context*)
(defvar *solver*)

(defun smt-trace (solver thing)
  (when-let (trace (z3-get-trace solver))
    (format trace "~&~A" thing)))

(defun smt-normalize-id (key)
  (etypecase key
    (symbol (string key))
    (string key)
    (fixnum key)))

(defun smt-add-sort (context name sort)
  (let ((hash (z3-context-sorts context)))
    (when (gethash name hash)
      (smt-error "Sort `~A' already declared" name))
    ;;(z3-inc-ref context (z3-sort-to-ast context sort))
    (setf (gethash name hash) sort)))

(defun smt-sort (context sort)
  (declare (type z3-context context))
  (if (z3-sort-p sort)
      sort
      (or (gethash sort (z3-context-sorts context))
          (smt-add-sort context sort
                        (ecase sort
                          ((:bool bool |Bool|) (z3-mk-bool-sort context))
                          ((:int int |Int|) (z3-mk-int-sort context))
                          ((:real real |Real|) (z3-mk-real-sort context)))))))

(defun smt-symbol (context name)
  (etypecase name
    (string (z3-mk-string-symbol context name))
    (symbol (z3-mk-string-symbol context (string name)))
    (fixnum (z3-mk-int-symbol context name))))

(defun smt-add-declaration (context &key name sort symbol ast func-decl definition)
  (let* ((table (z3-context-symbols context))
         (name (smt-normalize-id name)))
    (when (gethash name table)
      (smt-error "Symbol `~A' already declared" name))
    (setf (gethash name table)
          (make-smt-symbol :name name
                           :sort (smt-sort context sort)
                           :ast ast
                           :func-decl func-decl
                           :symbol (or symbol (smt-symbol context name))
                           :definition definition))))


(defun smt-add-func-decl (context func-decl &key sort)
  (let* ((namesym  (z3-get-decl-name context func-decl))
         (namestring  (z3-get-symbol-string context namesym))
         (ast (z3-func-decl-to-ast context func-decl))
         (sort (or sort (z3-get-sort context ast))))
    (smt-add-declaration context
                         :name namestring
                         :sort sort
                         :symbol namesym
                         :ast ast)))

(defun smt-declare-const (context name sort)
  "Constant declaration"
  (declare (type z3-context context))
  (let* ((key (smt-normalize-id name))
         (symbol (smt-symbol context name))
         (sort (smt-sort context sort)))
    (smt-add-declaration context
                         :name key
                         :sort sort
                         :symbol symbol
                         :ast  (z3-mk-const context symbol sort))))

(defun smt-declare-fun (context name params sort)
  "Function declaration"
  (let* ((key (smt-normalize-id name))
         (symbol (smt-symbol context name))
         (sort (smt-sort context sort)))
    (with-filled-array (domain n)
        ((p params)
         (z3-sort-pointer (smt-sort context p)))
        params
      (let* ((fdec (z3-mk-func-decl context symbol
                                    n domain sort))
             (ast (z3-func-decl-to-ast context fdec)))
        (smt-add-declaration context
                             :name key
                             :sort sort
                             :symbol symbol
                             :func-decl fdec
                             :ast ast)))))

(defun smt-define-fun (context name args sort body)
  (let ((names (loop for x in args
                  collect (destructuring-bind (name sort) x
                            (declare (ignore sort))
                            name)))
        (sorts (loop for x in args
                  collect (destructuring-bind (name sort) x
                            (declare (ignore name))
                            sort))))
    (smt-add-declaration context
                         :name name
                         :sort sort
                         :definition (make-smt-definition :body body
                                                          :param-names  names
                                                          :param-sorts sorts))))

(defun smt-declare-enumeration (context sortname symbols)
  ;(format t "~&name: ~A" sortname)
  ;(format t "~&symbols: ~A" symbols)
  ;(print symbols)
  (let* ((n (length symbols))
         (consts (foreign-alloc :pointer :count n))
         (testers (foreign-alloc :pointer :count n)))
    (with-filled-array (names n)
        ((s symbols)
         (z3-symbol-pointer (smt-symbol context s)))
      ;; create sort
      (let ((sort (z3-mk-enumeration-sort context
                                          (smt-symbol context sortname)
                                          n
                                          names
                                          consts
                                          testers)))
        ;; collect consts
        (dotimes (i n)
          (smt-add-func-decl context
                             (%make-z3-func-decl (mem-aref consts :pointer i))
                             :sort sort))
        (dotimes (i n)
          (smt-add-func-decl context
                             (%make-z3-func-decl (mem-aref testers :pointer i))
                             :sort (smt-sort context 'bool)))

      (smt-add-sort context sortname sort)))))

(defun smt-lookup (context name)
  (declare (type z3-context context))
  (let ((result (gethash (smt-normalize-id name)
                         (z3-context-symbols context))))
    (unless result
      (smt-error "Symbol `~A' is unbound" name))
    result))


;; (defun smt-intern (key &optional (context *context*))
;;   (let ((table (z3-context-symbols context))
;;         (key (etypecase key
;;                  (symbol (string key))
;;                  (string key)
;;                  (fixnum key))))
;;     (multiple-value-bind (value contains)
;;         (gethash key table)
;;       (if contains
;;           value
;;           (setf (gethash key table)
;;                 (make-smt-symbol :name key
;;                 (etypecase key
;;                   (string (z3-mk-string-symbol context key))
;;                   (fixnum (z3-mk-int-symbol context key))))))))

;; (defun smt-error (e)
;;   (error "Malformed SMT Expression: ~A" e))

(defun smt-exp-variables (exp)
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((add (e)
               (setf (gethash e hash) t))
             (visit (e) ;; TODO: predicates
               (if (atom e)
                   (add e)
                   (case (car e)
                     ((=
                       and |and| :and
                       or |or| :or
                       not |not| :not
                       iff |iff| :iff
                       ite |ite| :ite
                       implies |implies| :implies)
                      (map nil #'visit (cdr e)))
                     (otherwise
                      (add e))))))
      (visit exp))
    (hash-table-keys hash)))

(defmacro with-ast-array ((var args context) &body body)
  (with-gensyms (n a)
    `(with-filled-array (,var ,n)
         ((,a ,args)
          (z3-ast-pointer (smt->ast ,context ,a)))
       ,@body)))


  ;; (with-gensyms (i e)
  ;;   `(with-foreign-object (,var :pointer (length ,args))
  ;;      (reduce (lambda (,i ,e)
  ;;                (setf (mem-aref ,var :pointer ,i)
  ;;                      (z3-ast-pointer (smt->ast ,context ,e)))
  ;;                (1+ ,i))
  ;;              ,args
  ;;              :initial-value 0)
  ;;      ,@body)))

;; (defmacro with-asts (lambda-list args context
;;                      &body body)
;;   (with-gensyms (e)
;;     `(destructuring-bind ,lambda-list
;;          (loop for ,e in ,args
;;             collect (smt->ast ,context ,e))
;;        ,@body)))


(defun smt-unop (context function args)
  (declare (type z3-context context)
           (type function function)
           (type list args))
  (unless (and args
               (null (cdr args)))
    (error "Wanted one argument: ~A" args) )
  (funcall function context (smt->ast context (car args))))

(defun smt-binop (context function args)
  (declare (type z3-context context)
           (type function function)
           (type list args))
  (unless (and args
               (cdr args)
               (null (cddr args)))
    (error "Wanted two arguments: ~A" args) )
  (funcall function context
           (smt->ast context (first args))
           (smt->ast context (second args))))

(defun smt-nary-op (context function args)
  (declare (type z3-context context)
           (type function function)
           (type list args))
  (with-ast-array (a args context)
    (funcall function context (length args) a)))

(defun smt-sub (context args)
  (if (and args (null (cdr args)))
      ;; unary
      (smt-unop context #'z3-mk-unary-minus args)
      ;; n-ary
      (smt-nary-op context #'z3-mk-sub args)))

(defun smt-ite (context args)
  (declare (type z3-context context)
           (type list args))
  (unless (and args
               (cdr args)
               (cddr args)
               (null (cdddr args)))
    (error "Wanted three arguments: ~A" args) )
  (z3-mk-ite context
             (smt->ast context (first args))
             (smt->ast context (second args))
             (smt->ast context (third args))))

(defun smt-app-macro (context op definition args)
  (let ((body (smt-definition-body definition))
        (dummy-args (smt-definition-param-names definition)))
    ;(print args)
    ;(print dummy-args)
    (unless (= (length args) (length dummy-args))
      (smt-error "Mismatched argument counts for `~A'" op))
    (smt->ast context
              (if args
                 (sublis (map 'list #'cons dummy-args args)
                          body :test #'equal)
                  body))))

(defun smt-app (context op args)
  (let* ((ent (smt-lookup context op))
         (func-decl (smt-symbol-func-decl ent))
         (definition (smt-symbol-definition ent))
         (ast (smt-symbol-ast ent)))
    (cond
      ;; Uninterpreted function
      (func-decl
       (assert (null definition))
       (with-ast-array (array args context)
         (z3-mk-app context func-decl (length args) array)))
      ;; Constant
      ((and ast (null args))
       (assert (null definition))
       ast)
      ;; Defined Function
      (definition
       (smt-app-macro context op definition args))
      ;; Error
      (t (smt-error "Symbol `~A' is not a usable function" op)))))

(defmacro def-smt-op (&rest cases)
  `(defun smt-op (context e)
     (let ((op (car e))
           (args (cdr e)))
       (case op
         ,@(loop for (kw type function) in cases
              for s = (string kw)
              for kws = (list (intern s :keyword)
                              (intern s :smt-symbol)
                              (intern (string-downcase s) :smt-symbol))
              collect
                `(,kws
                  ,(case type
                    (:unary `(smt-unop context ,function args))
                    (:binary `(smt-binop context ,function args))
                    (:nary `(smt-nary-op context ,function args))
                    (otherwise `(,type context args)))))
         (otherwise
          (smt-app context op args))))))

(def-smt-op
    (= :binary #'z3-mk-eq)

    (not :unary #'z3-mk-not)
    (and :nary #'z3-mk-and)
    (or :nary #'z3-mk-or)
    (distinct :nary #'z3-mk-distinct)
    (implies :binary #'z3-mk-implies)
    (=> :binary #'z3-mk-implies)
    (iff :binary #'z3-mk-iff)
    (<=> :binary #'z3-mk-iff)
    (xor :binary #'z3-mk-xor)
    (ite smt-ite)

    (+ :nary #'z3-mk-add)
    (* :nary #'z3-mk-mul)
    (- smt-sub)
    (/ :binary #'z3-mk-div)
    (mod :binary #'z3-mk-mod)
    (rem :binary #'z3-mk-rem)
    (power :binary #'z3-mk-power)
    (^ :binary #'z3-mk-power)
    (< :binary #'z3-mk-lt)
    (<= :binary #'z3-mk-le)
    (> :binary #'z3-mk-gt)
    (>= :binary #'z3-mk-ge)

    )


(defun smt-atom (context e)
  (declare (type z3-context context))
  (etypecase e
    (fixnum
     (z3-mk-int context e (z3-mk-int-sort context)))
    (float
     (z3-mk-float context e))
    (string (smt-app context e nil))
    (symbol
     (case e
       ((t :true true |true|)
        (z3-mk-true context))
       ((nil :false false |false|)
        (z3-mk-false context))
       (otherwise
        (smt-app context e nil))))))

(defun smt->ast (context e)
  (declare (type z3-context context))
  (if (consp e)
      (smt-op context e)
      (smt-atom context e)))

(defun smt-assert (solver exp &optional context)
  ;;(declare (type z3-solver solver))
  (let ((context  (or context (z3-get-context solver))))
    (z3-solver-assert context solver (smt->ast context exp))))

(defun smt-check  (solver &optional context)
  (ecase (z3-solver-check (or context (z3-get-context solver)) solver)
    (:true :sat)
    (:undef :unknown)
    (:false :unsat)))

(defun smt-minimize (solver exp &optional context)
  (declare (type z3-optimize solver))
  (let ((context (or context (z3-optimize-context solver))))
    (z3-optimize-minimize context solver (smt->ast context exp))))

(defun smt-maximize (solver exp &optional context)
  (declare (type z3-optimize solver))
  (let ((context (or context (z3-optimize-context solver))))
    (z3-optimize-maximize context solver (smt->ast context exp))))

(defun smt-eval (solver stmt)
  ;;(declare (type z3-solver solver))
  (smt-trace solver stmt)
  (flet ((context ()
	   (z3-get-context solver)))
    (destructuring-ecase stmt
      ;; declarations
      (((declare-fun |declare-fun| :declare-fun) name args type)
       (if args
           (smt-declare-fun (context) name args type)
           (smt-declare-const (context) name type)))
      (((declare-const |declare-const| :declare-const) name type)
       (smt-declare-const (context) name type))
      (((declare-enum |declare-enum| :declare-enum) sortname &rest symbols)
       (smt-declare-enumeration (context) sortname symbols))
      (((define-fun |define-fun| :define-fun) name args type body)
       (smt-define-fun (context) name args type body))
      ;; Asssertions
      (((assert |assert| :assert) exp)
       (smt-assert solver exp (context)))
      ;; Checking
      (((check-sat |check-sat| :check-sat))
       (smt-check solver (context)))
      (((get-value |get-value| :get-value) symbols)
       (smt-values solver symbols (context)))
      ;; Stack
      (((push |push| :push) &optional (count 1))
       (dotimes (i count)
         (z3-solver-push (context) solver)))
      (((pop |pop| :pop) &optional (count 1))
       (z3-solver-pop (context) solver count))
      ;;optimization
      (((minimize |minimize| :minimize) exp)
       (smt-minimize solver exp (context)))
      (((maximize |maximize| :maximize) exp)
       (smt-maximize solver exp (context))))))

(defun smt-value-int (context ast)
  (with-foreign-object (i :int)
    (let ((r (z3-get-numeral-int context ast i)))
      (assert (eq :true r)) ;; TODO: handle other cases
      (mem-ref i :int))))

(defun smt-value-real (context ast)
  (with-foreign-object (num :int64)
    (with-foreign-object (den :int64)
      (let ((r (z3-get-numeral-rational-int64 context ast num den)))
	(assert (eq :true r))
	(/ (mem-ref num :int64) (mem-ref den :int64))))))

(defun model-value (context model thing)
  (declare (type z3-context context))
  (let* ((ent  (smt-lookup context thing))
         (d  (z3-mk-func-decl context
                              (smt-symbol-symbol ent)
                              0 (null-pointer)
                              (smt-symbol-sort ent)))
         (a (z3-model-get-const-interp context model d)))
    (if (null-pointer-p (z3-ast-pointer a))
        :unknown
        (let ((kind (z3-get-sort-kind context (smt-symbol-sort ent))))
          (ecase kind
            (:int
             (smt-value-int context a))
            (:bool
             (z3-get-bool-value context a))
	    (:real
	     (smt-value-real context a)))))))



(defun smt-values (solver symbols &optional context)
  ;;(declare (type z3-solver solver))
  (let* ((context (or context (z3-get-context solver)))
         (m (z3-solver-get-model context solver)))
    (z3-model-inc-ref context m)
    (unwind-protect
         (loop
            for s in symbols
            for v = (model-value context m s)
            collect (cons s v))
      (z3-model-dec-ref context m))))

(defun smt-prog (stmts &key
                         solver)
  (let* ((solver (or solver (make-solver))))
    (values (reduce (lambda (x stmt)
                      (declare (ignore x))
                      (smt-eval solver stmt))
                    stmts
                    :initial-value nil)
            solver)))

(defun check-sat (exp &key solver)
  (let* ((vars (smt-exp-variables exp))
         (stmts
          `(
            ;; declare variables
            ,@(loop for v in (smt-exp-variables exp)
                 collect `(declare-fun ,v () bool))
              ;; assert
              (assert ,exp)
              ;; check
              (check-sat))))
    (multiple-value-bind (result solver)
        (smt-prog stmts :solver solver)
      (ecase result
        (:sat (values t
                      (smt-values solver vars)
                      solver))
        (:unsat (values nil nil solver))
        (:unknown (error "Could not check: ~A" exp))))))

;; (defun check-sat (exp &key)
;;   (let* ((context (z3-mk-context (z3-mk-config)))
;;          (solver (z3-mk-solver context)))
;;     ;; declare variables
;;     (loop for v in (smt-exp-variables exp)
;;        do (smt-declare v :bool context))
;;     ;; assert
;;     (smt-assert exp solver context)
;;     ;; check
;;     (ecase (smt-check solver context)
;;       (:sat t)
;;       (:unsat nil)
;;       (:unknown (error "Could not check: ~A" exp)))) )
