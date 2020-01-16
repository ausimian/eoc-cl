;;;; allocate-registers.lisp
;;;;
;;;; build-interference -> allocate-registers -> patch-instructions

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun max-by (key elems &optional (pred #'>))
  (loop with max-elem  = (car elems)
        with max-value = (funcall key max-elem)
        for elem in (cdr elems)
        do (let ((v (funcall key elem)))
             (when (funcall pred v max-value)
               (setf max-elem  elem
                     max-value v)))
        finally (return max-elem)))

(defun color-graph (g vars)
  (flet ((saturation (v) (remove nil (mapcar #'color (neighbor-vertexes v)))))
    (loop with vs = (vertexes g)
          while vs
          do (let* ((v (max-by (lambda (v) (length (saturation v))) vs))
                    (s (saturation v))
                    (c (loop for c from 1 until (not (member c s)) finally (return c))))
               (setf (color v) c
                     vs (remove v vs)))))
  (loop for v in vars collect (cons v (color (find-vertex g v)))))

(defun allocate-registers (expr)
  (flet ((gen-homes (conflicts locals)
           (loop with colors = (color-graph conflicts locals)
                 with homes  = ()
                 with spill-index = (length +caller-saved-regs+)
                 for local in locals
                 do (let* ((color (cdr (assoc local colors)))
                           (home  (if (< color spill-index)
                                      `(reg ,(nth color +caller-saved-regs+))
                                      `(deref rbp ,(* -8 (1+ (- color spill-index)))))))
                      (push (cons local home) homes))
                 finally (return homes))))
    (ematch expr
      ( `(program ,(alist (:conflicts . conflicts) (:locals . locals)) ,blocks)
         (let* ((homes (gen-homes conflicts locals))
                (spilt (loop for home in homes when (eq 'deref (cadr home)) collect (car home))))
           `(program ((:lang . x86p) (:stack-space . ,(* 8 (length spilt))))
                     ,(loop for blk in blocks collect (assign-homes-block blk homes))))))))

