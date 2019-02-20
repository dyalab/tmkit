(in-package :z3)

(define-foreign-library libz3
  (t (:default "libz3")))

(use-foreign-library libz3)


(defcfun "Z3_get_full_version" :string)

;;; Config
(defcfun "Z3_mk_config" z3-config-type)

;;; Context
(defcfun "Z3_mk_context" z3-context-type
  (config z3-config-type))

(defcfun "Z3_set_error_handler" :void
  (context z3-context-type)
  (handler :pointer))

;;; Optimization
(defcfun "Z3_mk_optimize" z3-optimize-type
  (context z3-context-type))

(defcfun "Z3_optimize_minimize" :unsigned-int
  (context z3-context-type)
  (optimize z3-optimize-type)
  (ast z3-ast-type))

(defcfun "Z3_optimize_maximize" :unsigned-int
  (context z3-context-type)
  (optimize z3-optimize-type)
  (ast z3-ast-type))

(defcfun ("Z3_optimize_inc_ref" %z3-optimize-inc-ref) :void
  (context z3-context-type)
  (optimize z3-optimize-type))

(defcfun ("Z3_optimize_dec_ref" %z3-optimize-dec-ref) :void
  (context z3-context-type)
  (optimize z3-optimize-type))

(defcfun ("Z3_optimize_push" %z3-optimize-push) :void
  (context z3-context-type)
  (optimize z3-optimize-type))

(defcfun ("Z3_optimize_pop" %z3-optimize-pop) :void
  (context z3-context-type)
  (optimize z3-optimize-type)
  (n :unsigned-int))

(defcfun ("Z3_optimize_assert" %z3-optimize-assert) :void
  (context z3-context-type)
  (optimize z3-optimize-type)
  (ast z3-ast-type))

(defcfun ("Z3_optimize_check" %z3-optimize-check) z3-lbool
  (context z3-context-type)
  (optimize z3-optimize-type)
  (n :unsigned-int)
  (ast :pointer))

(defcfun ("Z3_optimize_get_model" %z3-optimize-get-model) z3-model-type
  (context z3-context-type)
  (optimize z3-optimize-type))

(defcfun "Z3_optimize_get_lower" z3-ast-type
  (context z3-context-type)
  (optimize z3-optimize-type)
  (n :unsigned-int))

(defcfun "Z3_optimize_get_reason_unknown" :string
  (context z3-context-type)
  (optimize z3-optimize-type))

;;; Solver
(defcfun "Z3_mk_solver" z3-solver-type
  (context z3-context-type))

(defcfun ("Z3_solver_inc_ref" %z3-solver-inc-ref) :void
  (context z3-context-type)
  (solver z3-solver-type))

(defcfun ("Z3_solver_dec_ref" %z3-solver-dec-ref) :void
  (context z3-context-type)
  (solver z3-solver-type))

(defcfun ("Z3_solver_push" %z3-solver-push) :void
  (context z3-context-type)
  (solver z3-solver-type))

(defcfun ("Z3_solver_pop" %z3-solver-pop) :void
  (context z3-context-type)
  (solver z3-solver-type)
  (n :unsigned-int))

(defcfun ("Z3_solver_assert" %z3-solver-assert) :void
  (context z3-context-type)
  (solver z3-solver-type)
  (ast z3-ast-type))

(defcfun ("Z3_solver_check" %z3-solver-check) z3-lbool
  (context z3-context-type)
  (solver z3-solver-type))

(defcfun ("Z3_solver_get_model" %z3-solver-get-model) z3-model-type
  (context z3-context-type)
  (solver z3-solver-type))

(defcfun "Z3_solver_get_reason_unknown" :string
  (context z3-context-type)
  (optimize z3-solver-type))

;;;Wrapper functions to properly call either the optimizer or solver depending on type of solver
(defun z3-solver-inc-ref (context solver)
  (etypecase solver
    (z3-optimize (%z3-optimize-inc-ref context solver))
    (z3-solver (%z3-solver-inc-ref context solver))))

(defun z3-solver-dec-ref (context solver)
  (etypecase solver
    (z3-optimize (%z3-optimize-dec-ref context solver))
    (z3-solver (%z3-solver-dec-ref context solver))))

(defun z3-solver-push (context solver)
  (etypecase solver
    (z3-optimize (%z3-optimize-push context solver))
    (z3-solver (%z3-solver-push context solver))))

(defun z3-solver-pop (context solver n)
  (etypecase solver
    (z3-optimize (%z3-optimize-pop context solver n))
    (z3-solver (%z3-solver-pop context solver n))))

(defun z3-solver-assert (context solver ast)
  (etypecase solver
    (z3-optimize (%z3-optimize-assert context solver ast))
    (z3-solver (%z3-solver-assert context solver ast))))

(defun z3-solver-check (context solver)
  (etypecase solver
    (z3-optimize (%z3-optimize-check context solver 0 (null-pointer)))
    (z3-solver (%z3-solver-check context solver))))

(defun z3-solver-get-model (context solver)
  (etypecase solver
    (z3-optimize (%z3-optimize-get-model context solver))
    (z3-solver (%z3-solver-get-model context solver))))

(defun z3-get-trace (solver)
  (etypecase solver
    (z3-solver (z3-solver-trace solver))
    (z3-optimize (z3-optimize-trace solver))))

(defun z3-get-context (solver)
  (etypecase solver
    (z3-solver (z3-solver-context solver))
    (z3-optimize (z3-optimize-context solver))))

(defun z3-get-reason-unknown (context solver)
  (etypecase solver
    (z3-solver (z3-solver-get-reason-unknown context solver))
    (z3-optimize (z3-optimize-get-reason-unknown context solver))))

;;;Model

(defcfun "Z3_model_inc_ref" :void
  (context z3-context-type)
  (model z3-model-type))

(defcfun "Z3_model_dec_ref" :void
  (context z3-context-type)
  (model z3-model-type))

;;; Symbols
(defcfun "Z3_mk_int_symbol" z3-symbol-type
  (context z3-context-type)
  (i :int))

(defcfun "Z3_mk_string_symbol" z3-symbol-type
  (context z3-context-type)
  (s :string))

(defcfun "Z3_get_symbol_string" :string
  (context z3-context-type)
  (sym z3-symbol-type))

;;; Sorts
(defcfun "Z3_mk_bool_sort" z3-sort-type
  (context z3-context-type))

(defcfun "Z3_mk_int_sort" z3-sort-type
  (context z3-context-type))

(defcfun "Z3_mk_real_sort" z3-sort-type
  (context z3-context-type))

(defcfun "Z3_mk_enumeration_sort" z3-sort-type
  (context z3-context-type)
  (symbol z3-symbol-type)
  (count :unsigned-int)
  (names :pointer)    ; symbols
  (consts :pointer)   ; func_decl
  (testers :pointer)) ; func_decl

(defcfun "Z3_get_sort_kind" z3-sort-kind
  (context z3-context-type)
  (sort z3-sort-type))

(defcfun "Z3_sort_to_ast" z3-ast-type
  (context z3-context-type)
  (sort z3-sort-type))

(defcfun "Z3_get_sort" z3-sort-type
  (context z3-context-type)
  (ast z3-ast-type))

;;; AST - Boolean
(defcfun "Z3_inc_ref" :void
  (context z3-context-type)
  (ast z3-ast-type))


(defmacro def-ast-unop (name)
  `(defcfun ,name z3-ast-type
     (context z3-context-type)
     (a z3-ast-type)))

(defmacro def-ast-binop (name)
  `(defcfun ,name z3-ast-type
     (context z3-context-type)
     (a z3-ast-type)
     (b z3-ast-type)))

(defmacro def-ast-nary (name)
  `(defcfun  ,name z3-ast-type
     (context z3-context-type)
     (n :unsigned-int)
     (args :pointer)))

(defmacro def-ast-ops (type &body names)
  `(progn
     ,@(loop for n in names
          collect `(,type ,n))))

(defcfun "Z3_mk_true" z3-ast-type
  (context z3-context-type))

(defcfun "Z3_mk_false" z3-ast-type
  (context z3-context-type))

(defcfun "Z3_mk_ite" z3-ast-type
  (context z3-context-type)
  (t1 z3-ast-type)
  (t2 z3-ast-type)
  (t3 z3-ast-type))

(def-ast-unop "Z3_mk_not")

(def-ast-ops def-ast-binop
  "Z3_mk_eq"
  "Z3_mk_iff"
  "Z3_mk_implies"
  "Z3_mk_xor")

(def-ast-ops def-ast-nary
  "Z3_mk_and"
  "Z3_mk_or"
  "Z3_mk_distinct")

;;; AST - Integer / Real
(def-ast-unop "Z3_mk_unary_minus")

(def-ast-ops def-ast-nary
  "Z3_mk_add"
  "Z3_mk_mul"
  "Z3_mk_sub")

(def-ast-ops def-ast-binop
  "Z3_mk_div"
  "Z3_mk_mod"
  "Z3_mk_rem"
  "Z3_mk_power"
  "Z3_mk_lt"
  "Z3_mk_le"
  "Z3_mk_gt"
  "Z3_mk_ge")

(def-ast-ops def-ast-unop
  "Z3_mk_int2real"
  "Z3_mk_real2int"
  "Z3_mk_is_int")


(defcfun "Z3_mk_const" z3-ast-type
  (context z3-context-type)
  (symbol z3-symbol-type)
  (sort z3-sort-type))

(defcfun "Z3_mk_func_decl" z3-func-decl-type
  (context z3-context-type)
  (symbol z3-symbol-type)
  (domain-size :unsigned-int)
  (domain :pointer) ;; array of sorts
  (range z3-sort-type))

(defcfun  "Z3_mk_app" z3-ast-type
     (context z3-context-type)
     (decl z3-func-decl-type)
     (n :unsigned-int)
     (args :pointer))

(defcfun "Z3_get_decl_name" z3-symbol-type
  (context z3-context-type)
  (decl z3-func-decl-type))

(defcfun "Z3_func_decl_to_ast" z3-ast-type
  (context z3-context-type)
  (decl z3-func-decl-type))

;; String conversion
(defcfun "Z3_ast_to_string" :string
  (context z3-context-type)
  (a z3-ast-type))

(defcfun "Z3_model_to_string" :string
  (context z3-context-type)
  (m z3-model-type))

(defcfun "Z3_sort_to_string" :string
  (context z3-context-type)
  (obj z3-sort-type))


(defcfun "Z3_func_decl_to_string" :string
  (context z3-context-type)
  (obj z3-func-decl-type))

;;; Accessors

(defcfun "Z3_get_bool_value" z3-lbool
  (context z3-context-type)
  (a z3-ast-type))

(defcfun "Z3_get_numeral_int" z3-lbool
  (context z3-context-type)
  (a z3-ast-type)
  (i :pointer))

(defcfun "Z3_get_numeral_rational_int64" z3-lbool
  (context z3-context-type)
  (a z3-ast-type)
  (num :pointer)
  (den :pointer))

;;; Model

(defcfun "Z3_model_get_func_interp" z3-func-interp-type
  (context z3-context-type)
  (model z3-model-type)
  (func-decl z3-func-decl-type))


(defcfun "Z3_model_get_const_interp" z3-ast-type
  (context z3-context-type)
  (model z3-model-type)
  (func-decl z3-func-decl-type))

;;; Numerals

(defcfun "Z3_mk_int" z3-ast-type
  (context z3-context-type)
  (value :int)
  (sort z3-sort-type))

(defcfun "Z3_mk_unsigned_int" z3-ast-type
  (context z3-context-type)
  (value :unsigned-int)
  (sort z3-sort-type))

(defcfun "Z3_mk_real" z3-ast-type
  (context z3-context-type)
  (numerator :int)
  (denominator :int))

(defun z3-mk-float (context num)
  (z3-mk-real context (truncate (* num 1000)) 1000))
