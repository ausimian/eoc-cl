;;;; package.lisp

(defpackage #:eoc-cl
  (:use #:cl #:named-readtables #:trivia)
  (:export

   ;; languages
   #:r1 #:c0 #:x86p #:x86 #:x86asm

   ;; language keywords
   #:program #:seq #:assign #:int #:var #:reg #:read-int

   ;; assembly keywords
   #:addq #:movq #:negq #:retq #:jmp #:deref #:xorq #:callq

   ;; registers
   #:rax #:rbp #:rsp #:rdi

   ;; passes
   #:uniquify
   #:remove-complex
   #:simplify-let
   #:explicate-control
   #:uncover-locals
   #:select-instructions
   #:assign-homes
   #:patch-instructions
   #:print-x86))
