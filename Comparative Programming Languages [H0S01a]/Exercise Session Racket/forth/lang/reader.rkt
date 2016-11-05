#lang s-exp syntax/module-reader "./language.rkt"

; #:read and #:read-syntax (both or neither must be supplied) 
; specify alternate readers for parsing the module body—replacements 
; read and read-syntax, respectively. 

#:read my-read
#:read-syntax my-read-syntax
 
(require "../parser.rkt")

; Reads and returns a single datum from in.
(define (my-read in) (syntax->datum (my-read-syntax #f in)))

; Like read, but produces a syntax object with source-location information.
(define (my-read-syntax src in)
  ; Define me
)