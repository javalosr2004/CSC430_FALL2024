#lang typed/racket

(require typed/rackunit)

; Definitions
(define-type ExprC (U binopC numC ifleq0? AppC FunDefC IdC))
(struct binopC ([ op : Symbol] [ l : ExprC] [r : ExprC] )#:transparent)
(struct numC ([n : Real])#:transparent)
(struct ifleq0? ([ test : ExprC ] [ if_cond : ExprC ] [ else_cond : ExprC])#:transparent)
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]))
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]))
(struct IdC ([id : Symbol]) #:transparent)

; (define op-table
;   (make-immutable-hash
;    (list (cons '+ +)
;          (cons '/ /)
;          (cons '* *)
;          (cons '^2 sqr))))


; (define (lookup [op : Symbol] ) : (-> Real Real Real)
;   (define op-finder ( hash-ref op-table op #f))
;   (if op-finder
;       op-finder
;       (error 'lookup "AAQZ3 - Unsupported operation used ~a" op)))

; new lookup table - not using hash
(define (lookup op)
  (match op
    ['+ +]
    ['- -]
    ['/ /]
    ['* *]
    [_else (error 'binary-operation "AAQZ unsupported op of ~e" op)]))


; This function interprets a given ExprC expression and returns the result.
(define (interp [ a : ExprC ] [ fds : (Listof FunDefC)]) : Real
  (match a
    [ (numC n) n ]
    [ (binopC op l r) ( (lookup op) (interp l fds) (interp r fds)  )]
    ; [ (ifleq0? test if_cond else_cond) (error 'interp "ifleq0? is not implemented yet.")]
    [ (ifleq0? test if_cond else_cond) (if (<= (interp test fds) 0) (interp if_cond fds) (interp else_cond fds))]
    [_else (error 'interp "Method not implemented yet, passed value: ~e" a)]))

#|

new interp not using hash lookup

(define (interp [ a : ExprC ]) : Real
  (match a
    [ (numC n) n ]
    [ (binopC op l r) ((op-table-lookup op) (interp l) (interp r)  )]
    [ (ifleq0? val) (error 'interp "AAQZ ifleq0? is not implemented yet.")]))

test cases for new interp

(check-equal? (interp (binopC '+ (numC 3.0) (numC 4.0))) 7.0)
(check-equal? (interp (binopC '- (numC 4.0) (numC 3.0))) 1.0)
(check-equal? (interp (binopC '/ (numC 4.0) (numC 2.0))) 2.0)
(check-equal? (interp (binopC '* (numC 3.0) (numC 4.0))) 12.0)

(check-exn (regexp (regexp-quote "AAQZ unsupported op")) (lambda () (interp (binopC '_ (numC 3.0) (numC 4.0)))))
(check-exn (regexp (regexp-quote "AAQZ ifleq0? is not implemented yet.")) (lambda () (interp (ifleq0? (numC 3.0)))))
|#

; This function parses function definitions in s-expression form to FunDefC.
; Input - S-Expression
; Output - FunDefC
(define (parse-fundefc [s : Sexp]) : FunDefC
  (match s
    [ (list 'def (? symbol? name) (list (list (? symbol? args) ...) '=> body))
      (define cast_args (cast args (Listof Symbol))) ; Casting to list of symbols, as specified by matching.
      (FunDefC name cast_args (parser body))]

    [_ (error 'parse-fundefc "Malformed input: ~e" s)]))


; This function parses a passed in S-Expression into the ExprC language.
; Input - S-Expression
; Output - ExprC
(define (parser [sexp : Sexp]) : ExprC
  (match sexp
    [ (? real? n) (numC n)]
    [ (? symbol? s) (IdC s)]
    [ (list '+ l r) (binopC '+ (parser l) (parser r)) ]
    [ (list '- l r) (binopC '- (parser l) (parser r)) ]
    [ (list '* l r) (binopC '* (parser l) (parser r)) ]
    [ (list '/ l r) (binopC '/ (parser l) (parser r)) ]
    [ (list 'ifleq0? test if_cond else_cond) (ifleq0? (parser test) (parser if_cond) (parser else_cond))]
    [ (list 'def _r ...) (parse-fundefc sexp)] ; Duplicate matching???? r is used to symbolize that there are properties left.
    [ (list (? symbol? fun) (list args ...))
      (define cast_args (map (lambda (arg) (parser arg)) args)) ; Parsing every argument into ExprC.
      (AppC fun cast_args)]
    [_else (error 'Input "Malformed input, passed expression: ~e" sexp)]))


; This function accepts an s-expression and calls the parser and then the interp function.
; Input - Sexp
; Output - A real number that is the interpreted result from the Arith language
(define (top-interp [sexp : Sexp] [fds : (Listof FunDefC)]) : Real
  (interp (parser sexp) fds))

; Test Cases for interp

; Test Cases for parser
; (check-exn (regexp (regexp-quote "ifleq0? is not implemented yet."))
;            (lambda () (parser '{ifleq0? 10})))

; Test Cases for lookup

; Test Cases for top-interp
(check-equal? (top-interp '{+ 1 2} '()) 3)
(check-equal? (top-interp '{* 1 {+ 2 3}} '()) 5)
(check-equal? (top-interp '{- 9 {+ 2 3}} '()) 4)
(check-equal? (top-interp '{ifleq0? -1 1 0} '()) 1)
(check-equal? (top-interp '{ifleq0? 1 1 0} '()) 0)
; (check-equal? (top-interp '{- 10 {^2 3}}) 1)
(parser '{def hello {(x y) => {+ 5 5}}})