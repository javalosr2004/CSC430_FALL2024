#lang typed/racket

(require typed/rackunit)




(struct binopC ([ op : Any] [ l : ArithC] [r : ArithC] )#:transparent)
(struct numC ([n : Real])#:transparent)

(define-type ArithC (U binopC numC))



(define op-table
  (make-immutable-hash
   (list (cons '+ +)
         (cons '/ /)
         (cons '* *))))


(hash-ref op-table '+)

(define plus-funct (hash-ref op-table '+))

(plus-funct 3 4)


(define (lookup [op : Symbol] ) : Any
  (match (

(define (interp[ a : ArithC ]) : Real
  (match a
    [ (numC n) n ]
    [ (binopC op l r) ( (lookup op) (interp l) (interp r)  )]))