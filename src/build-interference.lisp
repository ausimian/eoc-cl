;;;; build-interference.lisp
;;;;
;;;; uncover-live -> build-interference -> allocate-registers

(in-package #:eoc-cl)

(in-readtable :fare-quasiquote)

(defconst +caller-saved-regs+ '(rax rdx rcx rsi rdi r8 r9 r10 r11))

(defun add-conflicts-from-block (conflicts block)
  (flet ((conflicts-of (insn live-after)
           (ematch insn
             ( `(movq (var ,src) (var ,dst)) (values `(,dst) (set-difference live-after `(,src ,dst))))
             ( `(,_ ,_ (var ,dst))           (values `(,dst) (set-difference live-after `(,dst))))
             ( `(,_ (var ,dst))              (values `(,dst) (set-difference live-after `(,dst))))
             ( `(callq ,_)                   (values +caller-saved-regs+ live-after))
             ( _ (values () ()))
             )))
    (ematch block
      ( `(,_ (block ,info ,@insns))
         (loop for insn in insns
               for live-after in (cdr (assoc :live-after info))
               do (multiple-value-bind (fvs tvs) (conflicts-of insn live-after)
                     (loop for fv in fvs
                           do (loop for tv in tvs
                                    do (add-edge-between-vertexes conflicts fv tv)))))))))

(defun build-interference (e)
  "Build the interference graph from the live-after-sets."
  (flet ((remove-live-after (blk)
           (ematch blk
             (`(,label (block ,info ,@insns))
               `(,label (block ,(remove-if (lambda (x) (eq :live-after (car x))) info) ,@insns))))))
    (ematch e
      ( `(program ,info ,blocks)
         (let ((locals (cdr (assoc :locals info)))
               (conflicts (make-container 'graph-container :vertex-test 'eq :default-edge-type :undirected)))
           ;; Each register is allocated to itself.
           (loop for reg in +caller-saved-regs+
                 for col from 0
                 do (add-vertex conflicts reg :color col))
           ;; Add the (as-yet unallocated) local variables
           (dolist (local locals)
             (add-vertex conflicts local))
           (dolist (block blocks)
             (add-conflicts-from-block conflicts block))
           `(program ,(acons :conflicts conflicts info)
                     ,(mapcar #'remove-live-after blocks)))))))

