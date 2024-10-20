
#lang typed/racket

(require typed/rackunit)

(define-type ExprC
  (U NumC
     IdC
     AppC
     PlusC
     MultC
     BoolC
     StringC
     IfC
     BindC
     LambdaC
     ClosureC
     PrimOpC))

(struct NumC ([n : Real]))
(struct BoolC ([b : Boolean]))
(struct StringC ([s : String]))
(struct IdC ([s : Symbol]))
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]))
(struct PlusC ([l : ExprC] [r : ExprC]))
(struct MultC ([l : ExprC] [r : ExprC]))
(struct IfC ([test : ExprC] [if_cond : ExprC] [else_cond : ExprC]))
(struct BindC ([clauses : (Listof Clause)] [body : ExprC]))
(struct LambdaC ([params : (Listof Symbol)] [body : ExprC]))
(struct ClosureC ([params : (Listof Symbol)]
                 [body : ExprC]
                 [env : Env]))
(struct PrimOpC ([op : Symbol]))
(struct Clause ([id : Symbol] [expr : ExprC]))
(struct Binding ([name : Symbol] [val : Real]))
(define-type Env (Listof Binding))
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC])#:transparent)

(define mt-env '())
(define extend-env cons)



;;evals an expression in a given env and list of funct definitions
;;inputs: The expression to evaluate - Exprc
;;current env mapping ids to their vals - Env
;;list of func defs available for interp - fds
;;Output: the result of evaling the exp - Real
 (define (interp [expr : ExprC] [env : Env] [fds : (Listof FunDefC)]) : Real
    (match expr
      [(NumC n) n]
      [(IdC s) (lookup s env)]
      ;<idC-case>
      ;<appC-case>
      ;<plusC/multC-case>
      ))


;;get the val bound to a given id in an env
;;takes 2 arguments
;; symbol whose val needs to be retrieved - for
;; env is the current environmen that maps the id to its value - env
;;Output: the val pertaining to sym
(define (lookup [for : Symbol] [env : Env]) : Real
  (match env
    ['() (error 'lookup "AAQZ: name not found: ~e" for)]
    [(cons (Binding name val) r)
     (cond
       [(symbol=? for name) val]
       [else (lookup for r)])]))


