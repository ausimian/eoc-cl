;;;; interpret.lisp
;;;;
;;;; Various interpreters and compilers for evaluating the output from various passes.

(in-package #:eoc-cl/tests)

(in-readtable :fare-quasiquote)

;;;
;;; R1 interpreter
;;;
(defun interp/r1 (expr &optional env)
  (ematch expr
    ( `(program ,_ ,expr) (interp/r1 expr ()))
    ( `(read-int) (read-int))
    (  (satisfies symbolp)   (cdr (or (assoc expr env) (error "Unknown variable: ~S" expr))))
    (  (satisfies integerp)  expr)
    ( `(let ((,var ,expr)) ,@body)
       (let ((let-env (acons var (interp/r1 expr env) env))
             (result  nil))
         (dolist (body-expr body result) (setf result (interp/r1 body-expr let-env)))))
    ( `(+ ,arg1 ,arg2) (+ (interp/r1 arg1 env) (interp/r1 arg2 env)))
    ( `(- ,arg) (- (interp/r1 arg env)))))


;;;
;;; C0 interpreter
;;;
(defvar *c0-mem* (make-hash-table :test 'eq))

(defun interp/c0-expr (expr)
  (ematch expr
    ( `(read-int) (read-int))
    ( `(+ ,v1 ,v2) (+ (interp/c0-expr v1) (interp/c0-expr v2)))
    ( `(- ,v1)     (- (interp/c0-expr v1)))
    (  (satisfies symbolp) (gethash expr *c0-mem*))
    (  (satisfies numberp) expr)))

(defun interp/c0-block (insns)
  (ematch insns
    ( `(seq (assign ,var ,expr) ,rest)
       (setf (gethash var *c0-mem*) (interp/c0-expr expr))
       (interp/c0-block rest))
    ( `(return ,expr) (interp/c0-expr expr))))

(defun interp/c0-blocks (blocks block-id)
  (interp/c0-block (cdr (assoc block-id blocks))))

(defun interp/c0 (expr)
  (setf *c0-mem* (make-hash-table :test 'eq))
  (ematch expr
    ( `(program ,_ ,blocks) (interp/c0-blocks blocks :start))))

;;
;; x86 interpreter
;;
(defvar *x86-mem*)
(defun interp/x86-insns (insns)
  (labels ((r/m (v) (ematch v
                      (`(var ,v)         v)
                      (`(reg ,r)         r)
                      (`(deref ,r ,disp) (+ (gethash r *x86-mem*) disp))))
           (val (v) (ematch v
                      (`(int ,i) i)
                      (`(reg ,_) (gethash (r/m v) *x86-mem*))
                      (`(var ,_) (gethash (r/m v) *x86-mem*))
                      (`(deref ,_ ,_) (gethash (r/m v) *x86-mem*))
                      ))

           (mov (src dst) (setf (gethash (r/m dst) *x86-mem*) (val src)))
           (add (src dst) (incf (gethash (r/m dst) *x86-mem*) (val src)))
           (neg (dst) (setf (gethash (r/m dst) *x86-mem*) (- (val dst)))))
    (loop for insn in insns
          do (ematch insn
               ( `(movq ,src ,dst)  (mov src dst))
               ( `(addq ,src ,dst)  (add src dst))
               ( `(negq ,dst)       (neg dst))
               ( `(callq :read_int) (mov `(int ,(read-int)) `(reg rax)))
               ( `(jmp :finish)     (return (val '(reg rax))))))))

(defun interp/x86-blocks (blocks block-id)
  (ematch (cadr (assoc block-id blocks))
    ( `(block ,_ ,@insns) (interp/x86-insns insns))))

(defun interp/x86 (expr)
  (setf *x86-mem* (make-hash-table :test 'eq))
  (setf (gethash 'rbp *x86-mem*) 0)
  (ematch expr
    ( `(program ,_ ,blocks) (interp/x86-blocks blocks :start))))

;;
;; Compile and run generated assembly
;;
(defun compile/x86 (expr)
  (ematch expr
    (`(program ,_ ,asm-lines)
      (let* ((cee-file (uiop/stream:with-temporary-file (:stream stream :pathname name :type "c" :keep t)
                         (format stream "~{~A~%~}" `("#include <stdio.h>"
                                                     "long read_int() {"
                                                     ,(format nil "    return ~A;" (read-int))
                                                     "}"
                                                     "void print_result(long l) {"
                                                     "    printf(\"%ld\\n\", l);"
                                                     "}"))
                         name))
             (asm-file (uiop/stream:with-temporary-file (:stream stream :pathname name :type "s" :keep t)
                         (format stream "~{~A~%~}" asm-lines)
                         name))
             (bin-file (uiop/pathname:split-name-type (namestring asm-file))))
        (unwind-protect
             (progn
               (uiop/run-program:run-program (format nil "gcc -o ~A ~A ~A" bin-file cee-file asm-file))
               (parse-integer (uiop/run-program:run-program bin-file :output :line)))
          (dolist (f `(,asm-file ,cee-file ,bin-file)) (uiop/filesystem:delete-file-if-exists f)))))))


(defun interp (expr)
  (ematch expr
    ( `(program ,info ,_)
       (ecase (cdr (assoc :lang info))
         (r1      (interp/r1 expr))
         (c0      (interp/c0 expr))
         (x86p    (interp/x86 expr))
         (x86     (interp/x86 expr))
         (x86asm  (compile/x86 expr))))))
