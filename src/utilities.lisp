;;;; utilities.lisp
;;;;

(in-package #:eoc-cl)

(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
