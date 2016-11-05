#lang racket
(require "semantics.rkt")

; Interface of our actual language.
; Provide an implementation at the bottom of this file.
(provide dump
         num
         plus
         min
         mul
         div
         dup
         swap
         drop
         pp
         =
         word
         call
         (rename-out [my-module-begin #%module-begin]
                     [my-top-interaction #%top-interaction]
                     [my-datum #%datum]))

; Define an anchor within this namespace so that eval can use to
; perform within the right context (aka one that understands (num 1) and so on)
; DONT EDIT
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

; The current-state is a parameter used by the
; rest of this language.
; DONT EDIT
(define current-state (make-parameter (new-state)))

; Just to make sure that numbers are read in as numbers.
; DONT EDIT
(define-syntax-rule (my-datum . v) (#%datum . v))

; This allows users to use the REPL with s-exp syntax.
; DONT EDIT
(define-syntax-rule (my-top-interaction . v)
  (#%top-interaction . v))

; Every module in this language will make sure that it
; uses a fresh state.
; DONT EDIT
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([current-state (new-state)])
       body ...)))



; Implemenation of our actual language
; Define and implement the syntax rules for (dump), (num v),
; (word name body ...), (plus), (min), (mul), (div), (swap), (drop), (pp), (=)
; Implement (call name)
