;;;; eoc-cl.asd

(asdf:defsystem #:eoc-cl
  :description "Essentials of Compilation"
  :author "Nick Gunn <nick@ausimian.net>"
  :license  "BSD"
  :version "0.0.1"
  :depends-on ("fare-quasiquote" "trivia" "trivia.quasiquote" "cl-graph")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utilities")
                             (:file "runtime")
                             (:file "uniquify")
                             (:file "remove-complex")
                             (:file "simplify-let")
                             (:file "explicate-control")
                             (:file "uncover-locals")
                             (:file "select-instructions")
                             (:file "assign-homes")
                             (:file "patch-instructions")
                             (:file "uncover-live")
                             (:file "build-interference")
                             (:file "allocate-registers")
                             (:file "print-x86"))))
  :in-order-to ((test-op (test-op eoc-cl/tests))))

(asdf:defsystem #:eoc-cl/tests
  :depends-on ("eoc-cl" "fiveam")
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "generate")
                             (:file "interpret")
                             (:file "tests"))))
  :perform (test-op (op c) (uiop:symbol-call :eoc-cl/tests :run-tests!)))

