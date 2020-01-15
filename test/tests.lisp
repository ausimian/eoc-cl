;;;; tests.lisp

(in-package #:eoc-cl/tests)

(defun is-equiv (expr passes)
  "Given an R1 expression and a list of passes, determine whether the output of evaluating the expression is equivalent to the result of evaluating the expression after the application of the passes."
  (let ((expected-result (eval   (ignore-unused expr)))
        (actual-result   (interp (reduce (lambda (v pass) (funcall pass v))
                                         passes
                                         :initial-value `(program ((:lang . R1)) ,expr)))))
    (is (= expected-result actual-result))))

(defun expression-generator () #'generate-expr)

(def-suite all-tests :description "All tests.")

(in-suite all-tests)

(def-test identity ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '())))

(def-test uniquify ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify))))

(def-test remove-complex ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex))))

(def-test simplify-let ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let))))

(def-test explicate-control ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control))))

(def-test uncover-locals ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control
                     uncover-locals))))

(def-test select-instructions ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control
                     uncover-locals
                     select-instructions))))

(def-test assign-homes ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control
                     uncover-locals
                     select-instructions
                     assign-homes))))

(def-test patch-instructions ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control
                     uncover-locals
                     select-instructions
                     assign-homes
                     patch-instructions))))

(def-test print-x86 ()
  (for-all ((expr (expression-generator)))
    (is-equiv expr '(uniquify
                     remove-complex
                     simplify-let
                     explicate-control
                     uncover-locals
                     select-instructions
                     assign-homes
                     patch-instructions
                     print-x86))))

(defun run-tests! ()
  (run! 'all-tests))

