;;;; uniquify.lisp
;;;;
;;;; (source) -> uniquify -> remove-complex

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun uniquify (expr &optional subs)
  "Replace every declaration of a let-scoped variable with a unique name and
update the body to reflect that renaming."

  ;; e.g. transform
  ;;
  ;; (let ((a 1))
  ;;   (let ((b 2))
  ;;     (let ((a 3))
  ;;       (+ a b)))) => 5
  ;;
  ;; to
  ;;
  ;; (let ((g1 1))
  ;;   (let ((g2 2))
  ;;     (let ((g3 3))
  ;;       (+ g3 g2)))) => 5
  ;;
  (ematch expr
    ( `(program ,info ,expr) `(program ,info ,(uniquify expr subs)))
    (  (satisfies symbolp)    (let ((uniq (assoc expr subs))) (if uniq (cdr uniq) expr)))
    (  (satisfies integerp)   expr)
    ( `(let ((,var ,expr))
         ,@body)
       (let* ((uniq  (gensym))
              (nsubs (acons var uniq subs)))
         `(let ((,uniq ,(uniquify expr subs)))
            ,@(loop for expr in body collect (uniquify expr nsubs)))))
    ( `(,fn ,@args) `(,fn ,@(loop for arg in args collect (uniquify arg subs))))))
