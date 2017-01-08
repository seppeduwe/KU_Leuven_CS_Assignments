#lang racket
(require "semantics.rkt")
 
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
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

; The current-state is a parameter used by the
; rest of this language.
(define current-state (make-parameter (new-state)))

; Just to make sure that numbers are read in as numbers.
(define-syntax-rule (my-datum . v) (#%datum . v))

; This allows users to use the REPL with s-exp syntax.
(define-syntax-rule (my-top-interaction . v)
  (#%top-interaction . v))

; Every module in this language will make sure that it
; uses a fresh state.
(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([current-state (new-state)])
       body ...)))
 
; Implemenation of our actual language

(define-syntax-rule (dump)
  (dump-stack (current-state)))

(define-syntax-rule (num v)
  (push-stack v (current-state)))

; Transform body ... into a list and set as a word.
; The list can be empty or contain one or more elements
(define-syntax-rule (word name body ...)
  (set-word name '(body ...) (current-state)))

(define-syntax-rule (call name)
  (begin
    ; eval points to this namespace module because otherwise Racket
    ; doesn't know what to do.
    ; e.g.: (eval '(num 1)) has no meaning in plain Racket
    (map (lambda (el) (eval el ns)) (get-word name (current-state)))
    (void)))

(define-syntax-rule (plus)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (push-stack (+ a b) (current-state))))

(define-syntax-rule (min)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (push-stack (- a b) (current-state))))

(define-syntax-rule (mul)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (push-stack (* a b) (current-state))))

(define-syntax-rule (div)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (push-stack (/ a b) (current-state))))

(define-syntax-rule (dup)
  (let ([tos (pop-stack (current-state))])
    (push-stack tos (current-state))
    (push-stack tos (current-state))))

(define-syntax-rule (swap)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (push-stack b (current-state))
    (push-stack a (current-state))))

(define-syntax-rule (drop)
    (pop-stack (current-state)))

(define-syntax-rule (pp)
  (let ([tos (pop-stack (current-state))])
    (printf "~s\n" tos)))

(define-syntax-rule (=)
  (let ([a (pop-stack (current-state))]
        [b (pop-stack (current-state))])
    (cond
      [(eq? a b) (push-stack 1 (current-state))]
      [else (push-stack 0 (current-state))])))