
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

(define-type Env (Listof Binding))
(define-type Binding bind)
(struct bind ([name : Symbol] [val : Value]))

(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC])#:transparent)



; Definitions for values in AAQZ4.
(define-type Value(U NumV BoolV StringV ClosureV PrimOpV))

(struct NumV ([n : Real])#:transparent)
(struct BoolV ([b : Boolean])#:transparent)
(struct StringV ([s : String])#:transparent)
(struct ClosureV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct PrimOpV ([op : Symbol])#:transparent)

(define mt-env '())
(define extend-env cons)

; This function binds all passed names to it's associated value, will reutrn a Binding.
; (define (bind-all [args : (Listof Symbol)] [vals : (Listof Value)])
;   (cond
;     [(= (length args) (length vals)) (
;                                       ()
;                                       )]))

; This function binds a passed ClosureV params and passed list of expressions, will return a List of bindings with extended environment.
(define (interp-extend-env [args : (Listof Symbol)] [vals : (Listof ExprC)] [env : Env] [fds : (Listof FunDefC)]) : Env
  (cond
    [= (length args) (+ (length vals) (length env)) ; Could be optimized, I believe length is linear (not constant) time.
       ( cond
          [(and (empty? args) (empty? vals)) env]
          [else (extend-env (bind (first args) (interp (first vals) env fds)) (interp-extend-env (rest args) (rest vals) env fds))])]
    [else (error 'inter-extend-env "AAQZ - Insufficient arguments passed.")]))

; This function evaluates an expression given an environment and function definitions
; to output a resultant value (Value).
(define (interp [expr : ExprC] [env : Env] [fds : (Listof FunDefC)]) : Value
  (match expr
    [(NumC n) (NumV n)]
    [(BoolC b) (BoolV b)]
    [(StringC s) (StringV s)]
    [(IdC s) (lookup s env)]
    ;<appC-case>
    [(AppC f a) (let ([f-val (interp f env fds)])
                  (cond
                    [(ClosureV? f-val) (interp (ClosureV-body f-val) (interp-extend-env (ClosureV-params f-val) a env fds) fds)]
                    [else (error 'interp "AAQZ - Incorrect type for f-val passed ~e" f-val)]))]
    ;<plusC/multC-case>
    [(PlusC l r) (let ([l-val (interp l env fds)])
                   (let ([r-val (interp r env fds)])
                     (cond
                       [(and (NumV? l-val) (NumV? r-val)) (NumV (+ (NumV-n l-val) (NumV-n r-val)))]
                       [else (error 'interp "AAQZ - Type mismatch in PlusC match, given: ~e ~e" l-val r-val)])))]
    [(MultC l r) (let ([l-val (interp l env fds)])
                   (let ([r-val (interp r env fds)])
                     (cond
                       [(and (NumV? l-val) (NumV? r-val)) (NumV (* (NumV-n l-val) (NumV-n r-val)))]
                       [else (error 'interp "AAQZ - Type mismatch in MultC match, given: ~e ~e" l-val r-val)])))]
    ))


; This function returns the value for a given symbol passed in
; if it is found in the given environment.
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "AAQZ - name not found: ~e" for)]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))


; Test Cases
; Interp
(check-equal? (interp (NumC 3) mt-env '()) (NumV 3))
(check-equal? (interp (BoolC #f) mt-env '()) (BoolV #f))
(check-equal? (interp (StringC "consort") mt-env '()) (StringV "consort"))

(define fake-env (extend-env (bind 'index (NumV 12)) mt-env))
(check-equal? (interp  (IdC 'index) fake-env '()) (NumV 12))