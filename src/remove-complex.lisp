;;;; remove-complex.lisp
;;;;
;;;; uniquify -> remove-complex -> simplify-let

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun build-let (mappings body)
  "Convert a sequence of symbol -> expression mappings to a series of nested lets."
  (if mappings
      (let ((var   (caar mappings))
            (expr  (cdar mappings))
            (inner (build-let (cdr mappings) body)))
        (if (eq var expr)
            inner
            `(let ((,var ,expr)) ,inner)))
      body))

(defun remove-complex-arg (e)
  "Recursively simplify an argument of an expression, returning a pair of values.
The first of the pair is the substituted variable, the second is a mapping from
the variable to its simplified expression."
  (if (listp e)
      (let ((var (gensym)))
        (values var (cons var (remove-complex-expr e))))
      (values e (cons e e))))

(defun remove-complex-expr (e)
  "Recursively simplify function application. Other expressions are left unchanged."
  (match e
    ( `(let ((,var ,expr)) ,body)
       (multiple-value-bind (tmp mapping) (remove-complex-arg expr)
         (build-let (list mapping) `(let ((,var ,tmp)) ,(remove-complex-expr body)))))
    ( `(,fn ,@args)
       (let* ((simplified  (mapcar (lambda (arg) (multiple-value-call #'cons (remove-complex-arg arg))) args))
              (temporaries (progn  (mapcar #'car simplified)))
              (mappings    (mapcar #'cdr simplified)))
         (build-let mappings `(,fn ,@temporaries))))
    ( _ e)))

(defun remove-complex (e)
  "Recursively simplify function application, so that all arguments are simple expressions."
  ;;
  ;; e.g. transform
  ;;
  ;; (+ (+ a b) (+ c (+ d e)))
  ;;
  ;; to
  ;;
  ;; (let ((g1 (+ a b)))
  ;;   (let ((g2 (+ d e)))
  ;;     (let ((g3 (+ c g2)))
  ;;       (+ g1 g3))))
  ;;
  (match e
    ( `(program ,info ,expr) `(program ,info ,(remove-complex-expr expr)))))
