#lang racket
(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "./semantics.rkt")
(require "./language.rkt")
(require "./parser.rkt")

; Some rudimentary tests for `semantics.rkt`
(define ts-semantics (test-suite
 "semantics.rkt"

 (test-case
  "push-stack"
  (let ([s (new-state)])
    (push-stack 1 s)
    (check-equal? (length (state-stack s)) 1)
    (push-stack 2 s)
    (check-equal? (length (state-stack s)) 2))
  )
 
 (test-case
  "pop-stack"
  (let ([s (new-state)])
    (push-stack 1 s)
    (push-stack 2 s)
    (check-equal? (pop-stack s) 2)
    (check-equal? (pop-stack s) 1))
  )
 
 (test-case
  "set-word"
  (let ([s (new-state)])
    (set-word "a" '(1 2) s)
    (check-true (dict-has-key? (state-words s) "a"))
    (set-word "a" '3 s)
    (check-true (dict-has-key? (state-words s) "a")))
  )
 
 (test-case
  "get-word"
  (let ([s (new-state)])
    (set-word "a" '(1 2) s)
    (check-equal? (get-word "a" s) '(1 2))
    (set-word "a" '3 s)
    (check-equal? (get-word "a" s) '3))
  )
 
 ))

(define ts-parser (test-suite
 "parser.rkt"

 (test-case
  "num"
  (let ([str (open-input-string "1")])
    (let ([pe (parse-expr "test.rkt" str)])
      (check-true (syntax? pe) #t)
      (check-equal? (syntax-source pe) "test.rkt")
      (check-equal? (syntax->datum pe) '(num 1))
  )))
 
 (test-case
  "plus"
  (let ([str (open-input-string "+")])
    (let ([pe (parse-expr "test.rkt" str)])
      (check-true (syntax? pe) #t)
      (check-equal? (syntax-source pe) "test.rkt")
      (check-equal? (syntax->datum pe) '(plus))
  )))
 
 (test-case
  "="
  (let ([str (open-input-string "=")])
    (let ([pe (parse-expr "test.rkt" str)])
      (check-true (syntax? pe) #t)
      (check-equal? (syntax-source pe) "test.rkt")
      (check-equal? (syntax->datum pe) '(=))
  )))
 
 (test-case
  "word"
  (let ([str (open-input-string ": negate 0 swap - ;")])
    (let ([pe (parse-expr "test.rkt" str)])
      (check-true (syntax? pe) #t)
      (check-equal? (syntax-source pe) "test.rkt")
      (check-equal? (syntax->datum pe) '(word "negate" (num 0) (swap) (min)))
  )))
 
 (test-case
  "word"
  (let ([str (open-input-string "( wiii comments don't care ) : negate 0 swap - ;")])
    (let ([pe (parse-expr "test.rkt" str)])
      (check-true (syntax? pe) #t)
      (check-equal? (syntax-source pe) "test.rkt")
      (check-equal? (syntax->datum pe) '(word "negate" (num 0) (swap) (min)))
  )))))


(test/gui ts-semantics)
;(run-tests ts-semantics) ; non-graphical test function