#lang racket
 
(provide (all-defined-out)) ; export all defined functions
 
(define-struct state (stack words) #:mutable) ; look up `define-struct`
 
(define (new-state)
  (make-state '() (make-hash))) ; make-state is automagically defined because of `define-struct`

; your implemenation of the semantics

; implement (push-stack val a-state), (pop-stack a-state) and (dump-stack a-state)
; implement (get-word name a-state) and (set-word name body a-state)