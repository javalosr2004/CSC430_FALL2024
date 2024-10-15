#lang typed/racket

(require typed/rackunit)

; Definitions
(define-type ExprC (U binopC numC ifleq0?))
(struct binopC ([ op : Any] [ l : ExprC] [r : ExprC] )#:transparent)
(struct numC ([n : Real])#:transparent)
(struct ifleq0? ([val : ExprC]) #:transparent)

(define op-table
  (make-immutable-hash
   (list (cons '+ +)
         (cons '/ /)
         (cons '* *)
         (cons '^2 sqr))))

(define plus-funct (hash-ref op-table '+))

(define (lookup [op : Symbol] ) : Any
  (error 'lookup "Not implemented yet"))

; This function interprets a given ExprC expression and returns the result.
(define (interp [ a : ExprC ]) : Real
  (match a
    [ (numC n) n ]
    [ (binopC op l r) ( (lookup op) (interp l) (interp r)  )]
    [ (ifleq0? val) (error 'Method "ifleq0? is not implemented yet.")]))

; This function parses a passed in S-Expression into the Arith language.
; Input - S-Expression
; Output - ExprC
(define (parser [sexp : Sexp]) : ExprC
  (match sexp
    [ (? real? n) (numC n)]
    [ (list '+ l r) (binopC '+ (parser l) (parser r)) ]
    ; [ (list '- l r) (binopC '- (parser l) (parser r)) ]
    [ (list '* l r) (binopC '* (parser l) (parser r)) ]
    [ (list '/ l r) (binopC '/ (parser l) (parser r)) ]
    [ (list '^2 l) (binopC '^2 (parser l) (parser l)) ]
    [ (list 'ifleq0? val) (ifleq0? (parser val))]
    [else (error 'Input "Malformed input, passed expression: ~e" sexp)]))

; This function accepts an s-expression and calls the parser and then the interp function.
; Input - Sexp
; Output - A real number that is the interpreted result from the Arith language
(define (top-interp [sexp : Sexp]) : Real
  (interp (parser sexp)))

; Test Cases for interp

; Test Cases for lookup

; Test Cases for top-interp
(check-equal? (top-interp '(+ 1 2)) 3)
(check-equal? (top-interp '(* 1 (+ 2 3))) 5)
(check-equal? (top-interp '(- 9 (+ 2 3))) 4)
(check-equal? (top-interp '(- 10 (^2 3))) 1)