
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
    [<= (length args) (+ (length vals) (length env)) ; Could be optimized, I believe length is linear (not constant) time.
        ( cond
           [(and (empty? args) (empty? vals)) env]
           [else (extend-env (bind (first args) (interp (first vals) env fds)) (interp-extend-env (rest args) (rest vals) env fds))])]
    [else (error 'inter-extend-env "AAQZ - Insufficient arguments passed.")]))




(define (+num [ l : Value] [ r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r)) (NumV (+ (NumV-n l ) (NumV-n r)))]
    [else error '+num "AAQZ : both must be numbers : L: ~e R: ~e" l r]))



(define (*num [ l : Value] [ r : Value]) : Value
  (cond
    [(and (NumV? l) (NumV? r)) (NumV (* (NumV-n l ) (NumV-n r)))]
    [else error '*num "AAQZ : both must be numbers : L: ~e R: ~e" l r]))



; This function evaluates an expression given an environment and function definitions
; to output a resultant value (Value).
(define (interp [expr : ExprC] [env : Env] [fds : (Listof FunDefC)]) : Value
  (match expr
    [(NumC n) (NumV n)]
    [(BoolC b) (BoolV b)]
    [(StringC s) (StringV s)]
    [(IdC s) (lookup s env)]
    [(LambdaC par b) (ClosureV par b env)]
    [(ClosureC p b env) (ClosureV p b env)]
    [(Clause id expr) ()]
    [(BindC  (? list? s) b) (

                             )]
    ;<idC-case>
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


;;get the val bound to a given id in an env
;;takes 2 arguments
;; symbol whose val needs to be retrieved - for
;; env is the current environmen that maps the id to its value - env
;;Output: the val pertaining to sym
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

(check-equal? (interp (IdC 'index ) fake-env '()) (NumV 12))


(define top-env
  (list
   (bind '+ (PrimOpV '+))
   (bind '- (PrimOpV '-))
   (bind '/ (PrimOpV '/))
   (bind '* (PrimOpV '*))
   (bind 'error (PrimOpV 'error))
   (bind 'equal? (PrimOpV 'equal?))
   (bind 'true (BoolV #t))
   (bind 'false (BoolV #f))
   (bind '<= (PrimOpV '<=))))


;translate Concrete synt to abstract synt so that the interpreter can run
(define (parse [s : Sexp]) : ExprC
  (cond
    [(number? s) (NumC s)]
    [(string? s) (StringC s)]
    [(boolean? s) (BoolC s)]
    [(symbol? s)
     (cond
       [(or (equal? s '+)(equal? s '-)(equal? s '/)(equal? s '*)
       (equal? s 'error)(equal? s 'equal?)(equal? s 'true)(equal? s 'false)(equal? s '<=))
       (error 'parse "AAQZ : keyword as id not allowed ~e" s)]
       [else (IdC s)])]
    [(list? s)
     (match s
       [ '(if ,test, if_cond, else_cond)
         (IfC (parse test) (parse if_cond) (parse else_cond))
       [ '(bind, clause, body)
         ;(parse the bind
       [ '( (,params ... ) => ,body)
         ;(LambdaC (parse the param) (parse body))
       [(list f . args)
        (AppC (parse f) (map parse args))]
    [else (error 'parse "AAQZ : inv exp ~e" s)]   
  


;take in a value and return it as a String represenation of that value
;Input - Val
;Output String
(define (serialize [v : Value]) : String
  (cond
    [(NumV? v) (~v (NumV-n v))]
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

; interp
(define ex-closure-1 (ClosureC '(x y z) (PlusC (IdC 'x) (PlusC (IdC 'y) (IdC 'z))) mt-env))
(define test-env-1 (extend-env (bind 'example (interp ex-closure-1 mt-env '())) mt-env))
; (check-equal? (interp (AppC ex-closure-1 '(1 2 3)))
(interp (AppC (IdC 'example) (list (NumC 2) (NumC 2) (NumC 3))) test-env-1 '())
