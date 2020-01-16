;;;; patch-instructions.lisp
;;;;
;;;; assign-homes -> patch-instructions -> print-x86

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun patch-instruction (insn)
  (match insn
    (`(,op (deref ,r1 ,d1) (deref ,r2 ,d2))
      ;; =>
      `((movq (deref ,r2 ,d2) (reg rax))
        (,op  (deref ,r1 ,d1) (reg rax))
        (movq (reg rax) (deref ,r2 ,d2))))

    (`(,op (deref ,r ,d))
      ;; =>
      `((movq (deref ,r ,d) (reg rax))
        (,op (reg rax))
        (movq (reg rax) (deref ,r ,d))))

    ((guard `(movq (reg ,r1) (reg ,r2)) (eq r1 r2))
     ;; =>
     ())

    (_ (list insn))))

(defun patch-instruction-block (b)
  (ematch b
    ( `(,label (block ,info ,@insns))
       `(,label (block ,info ,@(loop for insn in insns
                                     append (patch-instruction insn)))))))

(defun patch-instructions (e)
  "Instructions where both src and dst are memory operands are not valid x86, so this pass transforms them into valid sequences using the rax register as an intermediary."
  (ematch e
    ( `(program ,info ,blocks)
       `(program ,info ,(mapcar #'patch-instruction-block blocks)))))

