;;;; assign-homes.lisp
;;;;
;;;; select-instructions -> assign-homes -> patch-instructions

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun assign-homes-insn (insn homes)
  (flet ((get-home (v) (cdr (assoc v homes))))
    (match insn
      (`(,op (var ,s) (var ,d)) `(,op ,(get-home s) ,(get-home d)))
      (`(,op (var ,s) ,dst)     `(,op ,(get-home s) ,dst))
      (`(,op ,src (var ,d))     `(,op ,src ,(get-home d)))
      (`(,op (var ,d))          `(,op ,(get-home d)))
      (_ insn))))

(defun assign-homes-block (b homes)
  (ematch b
    ( `(,label (block ,info ,@insns))
       `(,label (block ,info ,@(loop for insn in insns
                                     collect (assign-homes-insn insn homes)))))))

(defun assign-homes (e)
  "Assign a stack location to every local variable."
  (flet ((gen-homes (locals) (loop for i upfrom 1
                                   for local in locals
                                   collect (cons local `(deref rbp ,(* -8 i)))))
         (stack-size (locals) (* 8 (length locals))))
    (ematch e
      ( `(program ,info ,blocks)
         (let ((locals (cdr (assoc :locals info))))
           `(program ,(acons :lang 'x86
                             (acons :stack-space (stack-size locals)
                                    (remove :lang info :key #'car)))
                     ,(loop with homes = (gen-homes (reverse locals))
                            for block in blocks
                            collect (assign-homes-block block homes))))))))

