
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
(struct Binding ([name : Symbol] [val : Value]))
(define-type Env (Listof Binding))
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC])#:transparent)



(define-type Value(U NumV BoolV StringV ClosureV PrimOpV))

(struct NumV ([n : Real])#:transparent)
(struct BoolV ([b : Boolean])#:transparent)
(struct StringV ([s : String])#:transparent)
(struct ClosureV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct PrimOpV ([op : Symbol])#:transparent)

(define mt-env '())
(define extend-env cons)




(define (+num [ l : Value] [ r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r)) (NumV (+ (NumV-n l ) (NumV-n r)))] 
    [else error '+num "AAQZ : both must be numbers : L: ~e R: ~e" l r]))
    
     

(define (*num [ l : Value] [ r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r)) (NumV (* (NumV-n l ) (NumV-n r)))] 
    [else error '*num "AAQZ : both must be numbers : L: ~e R: ~e" l r]))



;;evals an expression in a given env and list of funct definitions
;;inputs: The expression to evaluate - Exprc
;;current env mapping ids to their vals - Env
;;list of func defs available for interp - fds
;;Output: the result of evaling the exp - Real
 (define (interp [expr : ExprC] [env : Env] [fds : (Listof FunDefC)]) : Value
    (match expr
      [(NumC n) (NumV n)]
      [(BoolC b) (BoolV b)]
      [(StringC s) (StringV s)]
      [(IdC s) (lookup s env)]
      [(LambdaC par b) (ClosureV par b env)]

      ;<idC-case>
      ;<appC-case>
      ;<plusC/multC-case>
      ))


;;get the val bound to a given id in an env
;;takes 2 arguments
;; symbol whose val needs to be retrieved - for
;; env is the current environmen that maps the id to its value - env
;;Output: the val pertaining to sym
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "AAQZ: name not found: ~e" for)]
    [(cons (Binding name val) r)
     (cond
       [(symbol=? for name) val]
       [else (lookup for r)])]))



(check-equal? (interp (NumC 3) mt-env '()) (NumV 3))
(check-equal? (interp (BoolC #f) mt-env '()) (BoolV #f))
(check-equal? (interp (StringC "consort") mt-env '()) (StringV "consort"))

(define fake-env (extend-env (Binding 'index (NumV 12)) mt-env))

(check-equal? (interp (IdC 'index ) fake-env '()) (NumV 12))



;take in a value and return it as a String represenation of that value
;Input - Val
;Output String
(define (serialize [v : Value]) : String
  (cond
    [(NumV? v) (number->string (NumV-n v))]
    [(BoolV? v) (cond
                  [(BoolV-b v) "true"]
                  [else "false"])]
    [(StringV? v) (string-append "\"" (StringV-s v) "\"")]
    [(ClosureV? v) "#<procedure>"]
    [(PrimOpV? v) "#<primop>"]))
    ;[else (error 'serialize " AAQZ: Unsupported val")]))


    (check-equal? (serialize (NumV 12)) "12")
    (check-equal? (serialize (BoolV #t)) "true")
    (check-equal? (serialize (BoolV #f)) "false")
    (check-equal? (serialize (StringV "12")) "\"12\"")
    (check-equal? (serialize (ClosureV '() (NumC 12) mt-env)) "#<procedure>")
    (check-equal? (serialize (PrimOpV '+ )) "#<primop>")
    

