;;;; package.lisp

(defpackage #:eoc-cl/tests
  (:use #:cl #:eoc-cl #:fiveam #:trivia #:named-readtables)
  (:export #:interp #:run-tests!))
