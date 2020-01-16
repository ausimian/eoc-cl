;;;; print-x86.lisp
;;;;
;;;; patch-instructions -> print-x86 -> <done>


(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun print-x86-arg (stream object &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (ematch object
    (`(reg ,r) (format stream "%~(~A~)" r))
    (`(int ,i) (format stream "$~A" i))
    (`(deref ,r ,i) (format stream "~A(%~(~A~))" i r))
    (_ (format stream "~(~A~)" object))))

(defun print-x86-insn (i)
  (match i
    ( `(,op ,@args) (format nil "~8T~(~A~) ~8,8T~{~/eoc-cl::print-x86-arg/~^, ~}" op args))))

(defun print-x86-block (b)
  (ematch b
    ( `(,label (block ,_ ,@insns))
       `(,(format nil "~(~A~):" label)
         ,@(mapcar #'print-x86-insn insns)))))

(defun print-x86 (e)
  "Produce valid gcc assembly source as a list of lines."
  (labels ((prologue (stack-size callee-saved)
             `((pushq (reg rbp))
               (movq  (reg rsp) (reg rbp))
               ,@(when (not (zerop stack-size))
                   `((subq (int ,stack-size) (reg rsp))
                     ,@(loop for i from 1
                             for r in callee-saved collect `(movq (reg ,r) (deref rbp ,(* -8 i))))))
               (jmp :start)))
           (epilogue (stack-size callee-saved)
             `(,@(when (not (zerop stack-size))
                   `(,@(loop for i from 1
                             for r in callee-saved collect `(movq (deref rbp ,(* -8 i)) (reg ,r)))
                     (addq (int ,stack-size) (reg rsp))))
               (popq  (reg rbp))
               (movq  (reg rax) (reg rdi))
               (callq :print_result)
               (xorq  (reg rax) (reg rax))
               (retq))))
    (ematch e
      ( `(program ,(alist (:spilt . spilt) (:callee-saved . callee-saved)) ,blocks)
         (let ((stack-size (apply #'min (if callee-saved +callee-save-space+ 0) spilt)))
           (unless (zerop (mod stack-size 16))
             (incf stack-size 8))
           `(program
             ((:lang . x86asm))
             ("       .globl main"
              "main:"
              ,@(mapcar #'print-x86-insn (prologue stack-size callee-saved))
              ,@(loop for b in blocks append (print-x86-block b))
              "finish:"
              ,@(mapcar #'print-x86-insn (epilogue stack-size callee-saved)))))))))
