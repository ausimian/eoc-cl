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
                 with avail  = (append +caller-saved-regs+ +callee-saved-regs+)
                 with spill-index = (length avail)
                 with homes  = ()
                 for local in locals
                 do (let* ((color (cdr (assoc local colors)))
                           (home  (if (< color spill-index)
                                      `(reg ,(nth color avail))
                                      `(deref rbp ,(- (- +callee-save-space+)
                                                      (* 8 (1+ (- color spill-index))))))))
                      (push (cons local home) homes))
                 finally (return homes))))
    (ematch expr
      ( `(program ,(alist (:conflicts . conflicts) (:locals . locals)) ,blocks)
         (let* ((homes (gen-homes conflicts locals)))
           (loop with callees = () ; callee-saved registers allocated to variables
                 with spilt   = () ; stack locations allocated to (spilt) varaibles
                 for home in homes
                 do (ematch (cdr home)
                      ( `(reg ,r) (when (member r +callee-saved-regs+) (setf callees (adjoin r callees))))
                      ( `(deref rbp ,d) (setf spilt (adjoin d spilt))))
                 finally
                    (return
                      `(program ((:lang . x86p) (:spilt . ,spilt) (:callee-saved . ,callees))
                                ,(loop for blk in blocks collect (assign-homes-block blk homes))))))))))

