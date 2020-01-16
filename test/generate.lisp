;;;; generate.lisp

(in-package #:eoc-cl/tests)

(in-readtable :fare-quasiquote)

(defun generate-var (vars)
  (if vars
      (case (random 2)
        (0 (nth (random (length vars)) vars))
        (otherwise (gensym)))
      (gensym)))

(defun generate-expr (&optional (level 0) vars)
  "Generate a random R1 expression."
  (case (random (if (< level 25) 6 2))
    (0 (random 1000))
    (1 '(read-int))
    (2 (if vars
           (nth (random (length vars)) vars)
           (generate-expr level vars)))
    (3 (if (< level 25)
           (let ((var (generate-var vars)))
             `(let ((,var ,(generate-expr (1+ level) vars)))
                ,(generate-expr (1+ level) (cons var vars))))
           (generate-expr level vars)))
    (4 `(+ ,(generate-expr (1+ level) vars) ,(generate-expr (1+ level) vars)))
    (5 `(- ,(generate-expr (1+ level) vars)))))

(defun ignore-unused (e)
  "Returns a new expression where all let-introduced variables are declared as ignorable. Suppresses unused-variable warnings that generated expressions can otherwise trigger."
  (ematch e
    (  (satisfies symbolp)   e)
    (  (satisfies integerp)  e)
    ( `(let ((,var ,expr)) ,@body)
       `(let ((,var ,(ignore-unused expr)))
          (declare (ignorable ,var))
          ,@(mapcar #'ignore-unused body)))
    ( `(,fn ,@args) `(,fn ,@(mapcar #'ignore-unused args)))))

