#lang typed/racket

(require typed/rackunit)

(struct binopC ([ op : Any] [ l : ArithC] [r : ArithC] )#:transparent)
(struct numC ([n : Real])#:transparent)
(struct ifleq0 ([ test : ExprC ] [ if : ExprC ] [ else: ExprC])#:transparent)


(define-type ArithC (U binopC numC))

(define op-table
  (make-immutable-hash
   (list (cons '+ +)
         (cons '/ /)
         (cons '* *)
         (cons '^2 sqr))))

(define plus-funct (hash-ref op-table '+))


(define (lookup [op : Symbol] ) : (-> Real Real Real)
     (define op-finder ( hash-ref op-table op #f))
     (if op-finder
         op-finder
         (error 'lookup "AAQZ3 - Unsupported operation used ~a" op)))
(check-equal? ((lookup '+) 3 5 ) 8)

; This function interprets a given ArithC expression and returns the result.
(define (interp [ a : ArithC ]) : Real
  (match a
    [ (numC n) n ]
    [ (binopC op l r) ( (lookup op) (interp l) (interp r)  )]))

; This function parses a passed in S-Expression into the Arith language.
; Input - S-Expression
; Output - ArithC
(define (parser [sexp : Sexp]) : ArithC
  (match sexp
    [ (? real? n) (numC n)]
    [ (list '+ l r) (binopC '+ (parser l) (parser r)) ]
    ; [ (list '- l r) (binopC '- (parser l) (parser r)) ]
    [ (list '* l r) (binopC '* (parser l) (parser r)) ]
    [ (list '/ l r) (binopC '/ (parser l) (parser r)) ]
    [ (list '^2 l) (binopC '^2 (parser l) (parser l)) ]
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
