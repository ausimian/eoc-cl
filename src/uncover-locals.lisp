;;;; uncover-locals.lisp
;;;;
;;;; explicate-control -> uncover-locals -> select-instructions

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun uncover-locals (e)
  "Store the set of all local variables in the info block."
  (labels ((block-locals (block-seq &optional locals)
             (ematch block-seq
               (`(return ,_) locals)
               (`(seq (assign ,local ,_) ,rest) (block-locals rest (adjoin local locals)))
               (`(,_ ,@insns) (block-locals insns)))))
    (ematch e
      ( `(program ,info ,blocks)
         (let ((locals (reduce #'union (mapcar #'block-locals blocks))))
           `(program ,(acons :locals locals info) ,blocks))))))

