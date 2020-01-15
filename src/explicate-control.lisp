;;;; explicate-control.lisp
;;;;
;;;; simplify-let -> explicate-control -> uncover-locals

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun explicate-control-tail (e)
  (match e
    ( `(let ((,var ,expr)) ,tail) (explicate-control-assign var expr tail))
    ( _ `(return ,e))))

(defun explicate-control-assign (var expr tail)
  (match expr
    ( `(let ((,sub-var ,sub-expr)) ,body) (explicate-control-tail `(let ((,sub-var ,sub-expr))
                                                                     (let ((,var ,body)) ,tail))))
    ( _ `(seq (assign ,var ,expr) ,(explicate-control-tail tail)))))

(defun explicate-control (e)
  "Make the order of execution explicit. Transforms the language from R1 form to C0 form."
  ;;
  ;; E.g. transform
  ;;
  ;; (let ((a (let ((b 1))
  ;;            (let ((c 2))
  ;;              (+ b c)))))
  ;;   a)
  ;;
  ;; to
  ;;
  ;; (seq (assign b 1)
  ;;      (seq (assign c 2)
  ;;           (seq (assign a (+ b c))
  ;;                (return a))))
  ;;
  (match e
    ( `(program ,_ ,expr)
       `(program ((:lang . c0)) ((:start ,@(explicate-control-tail expr)))))))

