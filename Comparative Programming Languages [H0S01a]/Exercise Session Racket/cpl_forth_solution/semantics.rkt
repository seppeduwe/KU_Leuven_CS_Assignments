#lang racket
 
(provide (all-defined-out))
 
(define-struct state (stack words) #:mutable)
 
(define (new-state)
  (make-state '() (make-hash)))

(define (push-stack val a-state)
  (set-state-stack! a-state (cons val (state-stack a-state))))

(define (pop-stack a-state)
  (let ([res (first (state-stack a-state))])
    (set-state-stack! a-state (rest (state-stack a-state)))
    res))

(define (dump-stack a-state)
  (begin
    (printf "[ ")
    (map (lambda (el) (printf "~s " el)) (state-stack a-state))
    (printf "]\n")))

(define (get-word name a-state)
  (hash-ref (state-words a-state) name))

(define (set-word name body a-state)
  (hash-set! (state-words a-state) name body))