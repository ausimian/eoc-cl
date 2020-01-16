;;;; package.lisp

(defpackage #:eoc-cl
  (:use #:cl #:named-readtables #:trivia #:cl-containers #:cl-graph)
  (:export

   ;; languages
   #:r1 #:c0 #:x86p #:x86 #:x86asm

   ;; language keywords
   #:program #:seq #:assign #:int #:var #:reg #:read-int

   ;; assembly keywords
   #:addq #:movq #:negq #:retq #:jmp #:deref #:xorq #:callq

   ;; caller-saved registers
   #:rax #:rdx #:rcx #:rsi #:rdi #:r8 #:r9 #:r10 #:r11
   ;; callee-saved registers
   #:rsp #:rbp #:rbx #:r12 #:r13 #:r14 #:r15

   ;; passes
   #:uniquify
   #:remove-complex
   #:simplify-let
   #:explicate-control
   #:uncover-locals
   #:select-instructions
   #:uncover-live
   #:build-interference
   #:allocate-registers
   #:patch-instructions
   #:print-x86))
