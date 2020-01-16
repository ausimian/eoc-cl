;;;; uncover-live.lisp
;;;;
;;;; select-instructions -> uncover-live -> build-interference

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defun uncover-live-insns (insns)
  (flet ((vars-of (insn)
           ;; All operations (except for movq) involving a destination var
           ;; are assumed to both read and write that var.
           (ematch insn
             (`(movq (var ,src) (var ,dst)) (values (list src) (list dst)))
             (`(,_   (var ,src) (var ,dst)) (values (list src dst) (list dst)))
             (`(movq ,_         (var ,dst)) (values () (list dst)))
             (`(movq (var ,src) ,_        ) (values (list src) ()))
             (`(,_   ,_         (var ,dst)) (values (list dst) (list dst)))
             (`(,_   (var ,src) ,_        ) (values (list src) ()))
             (`(,_   (var ,var))            (values (list var) (list var)))
             (_ (values () ())))))
    (loop with live-after = '(())
          for insn in (reverse insns)
          do (let ((live-after-next (car live-after)))
               (multiple-value-bind (vr vw) (vars-of insn)
                 (let ((live-after-this (union (set-difference live-after-next vw) vr)))
                   (setf live-after (cons live-after-this live-after)))))
          finally (return (cdr live-after)))))

(defun uncover-live-block (b)
  (ematch b
    ( `(,label (block ,info ,@insns))
       `(,label (block ,(acons :live-after (uncover-live-insns insns) info) ,@insns)))))

(defun uncover-live (e)
  "Determine the set of live-after variables at each instruction"
  (ematch e
    ( `(program ,info ,blocks)
       `(program ,(acons :lang 'x86p (remove :lang info :key #'car))
                 ,(mapcar #'uncover-live-block blocks)))))

