;;;; eoc-cl.lisp

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun simplify-let (e)
  "Simplify let expressions."
  ;;
  ;; i.e. transform
  ;; ((let ((v e))
  ;;    v))
  ;;
  ;; with
  ;;
  ;; e
  ;;
  ;; Also replace expressions of the form (let ((v s)) e) where s is
  ;; a _symbol_ are replaced with the results of substituting occurrences
  ;; of v with s in e.
  ;;
  (match e
    ( `(program ,info ,expr) `(program ,info ,(simplify-let expr)))
    ( `(let ((,var ,expr)) ,body)
       (cond
         ((symbolp expr) (simplify-let (uniquify body (acons var expr ()))))
         ((eq var body) (simplify-let expr))
         (t `(let ((,var ,(simplify-let expr))) ,(simplify-let body)))))
    ( _ e)))
