#lang racket
(provide parse-expr)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(define (parse-expr src in)
  (define-values (line column position) (port-next-location in))
  (define (decorate sexp span)
    (datum->syntax #f sexp (list src line column position span)))
  
  ; If at EOF, immediatly return EOF
  (if (eof-object? (peek-char in)) eof
      ; otherwise find the next Forth s-expression via the Forth lexer
      (let ([sexp (forth-lexer in)])
        (let-values ([(l c tail-position) (port-next-location in)])
          ; and transform it into a proper syntax object
          (decorate sexp (- tail-position position))))))

; Define regular expressions for unsigned floats
(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

; Define some abbreviations that can be used within the lexer
(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (identifier-characters (re-or (char-range "A" "z")
                                "?" "!" ":" "$" "%" "^" "&"))
  (identifier (re-+ identifier-characters)))

; return a list of forth expressions within a word definition
(define forth-inner-word-lexer
  (lexer
   [any-char (let ([sexp (forth-lexer input-port)])
               (cons sexp (if (eq? sexp 'word-end) '() (forth-inner-word-lexer input-port))))]))

(define forth-lexer
  (lexer
   [#\: (let ([name (forth-identifier input-port)]
              ; get rid of 'word-end at the end of the list
              [body (reverse (cdr (reverse (forth-inner-word-lexer input-port))))])
              `(word ,name ,@body))]
   [#\; 'word-end]
   [#\+ `(plus)]
   [#\- `(min)]
   [#\* `(mul)]
   [#\/ `(div)]
   [#\. `(pp)]
   [#\= `(=)]
   ; Comments don't exist in our language, so we forget them
   [#\( (forth-comments-lexer input-port)]
   ["dump" `(dump)]
   ["swap" `(swap)]
   ["dup" `(dup)]
   [number10 `(num ,(string->number lexeme))]
   [identifier `(call ,lexeme)]
   [whitespace (forth-lexer input-port)]))

; Lexer for Forth comments
(define forth-comments-lexer
  (lexer
   [#\) (forth-lexer input-port)]
   [any-char (forth-comments-lexer input-port)]))

; Lexer for a Forth identifier that returns the identifier string
(define forth-identifier
  (lexer
   [identifier lexeme]
   [whitespace (forth-identifier input-port)]))