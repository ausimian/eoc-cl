;;;; select-instructions.lisp
;;;;
;;;; uncover-locals -> select-instructions -> assign-homes

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun select-instructions-stmt (stmt)
  (flet ((select-arg (arg)
           (ematch arg
             ( 'rax '(reg rax))
             ( (satisfies symbolp)  `(var ,arg))
             ( (satisfies integerp) `(int ,arg)))))
    (ematch stmt
      ( `(assign ,dst (+ ,e1 ,e2)) `((movq  ,(select-arg e1) ,(select-arg dst))
                                     (addq  ,(select-arg e2) ,(select-arg dst))))
      ( `(assign ,dst (- ,e1))     `((movq  ,(select-arg e1) ,(select-arg dst))
                                     (negq  ,(select-arg dst))))
      ( `(assign ,dst (read-int))  `((callq :read_int)
                                     (movq  ,(select-arg 'rax) ,(select-arg dst))))
      ( `(assign ,dst ,e)          `((movq  ,(select-arg e) ,(select-arg dst)))))))

(defun select-instructions-tail (tail)
  (ematch tail
    ( `(seq ,stmt ,tail) `(,@(select-instructions-stmt stmt) ,@(select-instructions-tail tail)))
    ( `(return ,result)  `(,@(select-instructions-stmt `(assign rax ,result)) (jmp :finish)))))

(defun select-instructions-block (b)
  (ematch b
    ( `(,label ,@tail) `(,label (block () ,@(select-instructions-tail tail))))))

(defun select-instructions (e)
  "Translate C0 to pseudo-x86 assembly."
  (ematch e
    ( `(program ,info ,blocks)
       `(program ,(acons :lang 'x86p (remove :lang info :key #'car))
                 ,(mapcar #'select-instructions-block blocks)))))

