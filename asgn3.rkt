; We completed this assignment.

#lang typed/racket

(require typed/rackunit)

; Definitions
(define-type ExprC (U binopC numC ifleq0? AppC FunDefC IdC))
(struct binopC ([ op : Symbol] [ l : ExprC] [r : ExprC] )#:transparent)
(struct numC ([n : Real])#:transparent)
(struct ifleq0? ([ test : ExprC ] [ if_cond : ExprC ] [ else_cond : ExprC])#:transparent)
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC])#:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)])#:transparent)
(struct IdC ([id : Symbol]) #:transparent)

; This function finds the corresponding binary operation based on symbol passed.
; Input - Symbol
; Output - Function
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
    [ (ifleq0? test if_cond else_cond) (if (<= (interp test fds) 0) (interp if_cond fds) (interp else_cond fds))]
    ; Taken from seciton 5.4
    [(AppC f a) (define fd (get-fundef f fds))
                (interp (subst a
                               (FunDefC-args fd)
                               (FunDefC-body fd))
                        fds)]
    [_else (error 'interp "AQQZ - Method not implemented yet, passed value: ~e" a)]))

; This function substitutes arguments into a function body.
; Input - function body - ExprC
; Input - arguments to pass into - (Listof ExprC)
; Output - ExprC
(define (subst [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match in
    [(numC _n) in]
    [(IdC s)
     ;  (printf "Value of what: ~e. Value for: ~e. Value for in: ~e\n" what for in )
     (define var_idx (index-of for s))
     (cond
       [(false? var_idx) in]
       [else (list-ref what var_idx)])]
    [(AppC f a) (AppC f (map (lambda (x) (subst what for (cast x ExprC))) a))] ; Could be making unneccesary calls.
    [(binopC s l r) (binopC s (subst what for l)
                            (subst what for r))]
    [(ifleq0? test if_cond else_cond) (ifleq0? (subst what for test)
                                               (subst what for if_cond)
                                               (subst what for else_cond))]
    ))


; This function parses function definitions in s-expression form to FunDefC.
; Input - S-Expression
; Output - FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [ (list 'def (? symbol? name) (list (list (? symbol? args) ...) '=> body))
      (define cast_args (cast args (Listof Symbol))) ; Casting to list of symbols, as specified by matching.
      ;checking that no dup args are given - 3.2
      (when (not (equal? (length cast_args) (length (remove-duplicates cast_args))))
        (error 'parse-fundef "AAQZ - Duplicate parameter names in function definition: ~e" s))
      (FunDefC name cast_args (parse body))]
    [_ (error 'parse-fundef "AQQZ - Malformed input: ~e" s)]))


; This function parses a passed in S-Expression into the ExprC language.
; Input - S-Expression
; Output - ExprC
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [ (? real? n) (numC n)]
    [(? symbol? s) (IdC s)]
    [ (list '+ l r) (binopC '+ (parse l) (parse r)) ]
    [ (list '- l r) (binopC '- (parse l) (parse r)) ]
    [ (list '* l r) (binopC '* (parse l) (parse r)) ]
    [ (list '/ l r) (binopC '/ (parse l) (parse r)) ]
    [ (list 'ifleq0? test if_cond else_cond) (ifleq0? (parse test) (parse if_cond) (parse else_cond))]
    [ (list (? symbol? fun) args ...)
      (define cast_args (map (lambda (arg) (parse arg)) args)) ; Parsing every argument into ExprC.
      (AppC fun cast_args)]
    [_else (error 'Input "AQQZ -Malformed input, passed expression: ~e" sexp)]))



; Get func definition by name.
(define (get-fundef [name : Symbol] [fdlst : (Listof FunDefC)]) : FunDefC
  (if (empty? fdlst)
      (error 'get-fundef "AAQZ reference to func not supported ~e" name)
      (let ([current-fundef (first fdlst)])
        (if (equal? name (FunDefC-name current-fundef))
            current-fundef
            (get-fundef name (rest fdlst))))))

; Helper to check if function passed is equal to the symbol.
(define main-checker (lambda ([fun : FunDefC] [val : Symbol]) (symbol=? (FunDefC-name fun) val)))
; This function interprets the function named main from the fundefs.
; Input - Funtion definitions.
; Output - Real
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (define main_index (index-of funs 'main main-checker))
  (
   if (false? main_index)
      (error 'interp-fns "AQQZ - Main not found in function definitions of ~e" funs)
      (interp  (FunDefC-body (list-ref funs main_index)) funs)))

; This function accepts an s-expression and calls the parse and then the interp function.
; Input - Sexp
; Output - A real number that is the interpreted result from the Arith language
; (define (top-interp [sexp : Sexp]) : Real
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


; Takes in single s exp and returns list of function definitions - part 5 :skull:
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    [(list fundef ...) (map parse-fundef fundef)]
    [_ (error 'parse-prog "AAQZ malformed list of func ~e" s)]))



; Test Cases for subst
(check-equal? (subst (list (numC 3)) '(x) (IdC 'x ) ) (numC 3) )
(check-equal? (subst (list (numC 4)) '(y) (binopC '+ (IdC 'y) ( IdC 'y))) (binopC '+ (numC 4) (numC 4)))
(check-equal? (subst (list (numC 12)) '(f) (IdC 'z)) (IdC 'z))
(check-equal? (subst (list (numC 20) (numC 10)) '(x y)
                     (binopC '* (IdC 'y) (IdC 'x)))
              (binopC '* (numC 10) (numC 20)))

(check-equal? (subst (list (numC -5) (numC 1) (numC 0))
                     (list 'a 'b 'c)
                     (ifleq0?  (IdC 'a) (IdC 'b) (IdC 'c)))
              (ifleq0?  (numC -5) (numC 1) (numC 0)))

; Example definitions
(define fun-ex-1 (parse-fundef '{def fun-ex-1 {() => {+ 1 1}}}))
(define fun-ex-2 (parse-fundef '{def fun-ex-2 {(x) => {+ x {fun-ex-1}}}}))
(define fun-ex-3 (parse-fundef '{def fun-ex-3 {(x y) => {+ {fun-ex-2 x} {fun-ex-2 y}}}}))

; Test Cases for interp
(check-equal? (interp (binopC '+ (numC 3.0) (numC 4.0)) '()) 7.0)
(check-equal? (interp (binopC '- (numC 4.0) (numC 3.0)) '()) 1.0)
(check-equal? (interp (binopC '/ (numC 4.0) (numC 2.0)) '()) 2.0)
(check-equal? (interp (binopC '* (numC 3.0) (numC 4.0)) '()) 12.0)
(check-equal? (interp (AppC 'fun-ex-3 (list (numC 2) (numC 3))) (list fun-ex-3 fun-ex-2 fun-ex-1)) 9)
(check-exn (regexp (regexp-quote "Method not implemented yet, passed value:")) (lambda () (interp fun-ex-1 '())))
; (check-equal? (interp (AppC ) )
(check-exn (regexp (regexp-quote "AAQZ unsupported op")) (lambda () (interp (binopC '_ (numC 3.0) (numC 4.0)) '())))

; Test Cases for parse
; (check-exn (regexp (regexp-quote "ifleq0? is not implemented yet."))
;            (lambda () (parse '{ifleq0? 10})))
(check-equal? (parse-fundef '{def funny {() => {+ 9 10}}}) (FunDefC 'funny '() (binopC '+ (numC 9) (numC 10))))
(check-equal? (parse '{+ 3 3}) (binopC '+ (numC 3) (numC 3)))
(check-equal? (parse '{- 3 3}) (binopC '- (numC 3) (numC 3)))
(check-equal? (parse '{* 3 3}) (binopC '* (numC 3) (numC 3)))
(check-equal? (parse '{/ 3 3}) (binopC '/ (numC 3) (numC 3)))
(check-exn (regexp (regexp-quote "Malformed input, passed expression:")) (lambda () (parse '{2 3})))

; Test Cases for parse-prog
(check-equal? (parse-prog '{{def fun-ex-1 {() => {+ 1 1}}}}) (list fun-ex-1))
(check-exn (regexp (regexp-quote "AAQZ malformed list of func")) (lambda () (parse-prog '4)))

; Test Cases for interp-fns
(check-exn (regexp (regexp-quote "Main not found in function definitions of")) (lambda () (interp-fns (list fun-ex-1))))

; Test Cases for lookup
(check-equal? (lookup '+) +)
(check-equal? ((lookup '+) 5 7) 12)
(check-equal? (lookup '*) *)
(check-equal? ((lookup '*) 3 2) 6)
(check-equal? (lookup '/) /)
(check-equal? (lookup '-) -)
(check-exn (regexp (regexp-quote "AAQZ unsupported op of"))
           (lambda () (lookup 's )))

; Test cases for parse-fundef
(check-exn (regexp (regexp-quote "AAQZ - Duplicate parameter names in function definition:"))
           (lambda () (parse-fundef '{def funny {(x y x) => {+ x y}}})))

; Test Cases for top-interp
; (check-equal? (top-interp '{+ 1 2}) 3)
; (check-equal? (top-interp '{* 1 {+ 2 3}} ) 5)
; (check-equal? (top-interp '{- 9 {+ 2 3}}) 4)
; (check-equal? (top-interp '{ifleq0? -1 1 0}) 1)
; (check-equal? (top-interp '{ifleq0? 1 1 0}) 0)
; (check-equal? (top-interp '{- 10 {^2 3}}) 1)
; (top-interp '{{def fun {() => {+ 5 5}}} {def main {() => {{ifleq0? -1 {+ 4 4} {+ 1 2}}}}}})
(check-equal? (top-interp '{{def fun {(x y z) => {+ {+ x y} z}}} {def main {() => {+ {fun 3 3 3} {fun 3 4 5}}}}}) 21)

; Test Cases for get-fundefc
(check-exn (regexp (regexp-quote "AAQZ reference to func not supported"))
           (lambda () (get-fundef 'funny (list fun-ex-1))))
(check-exn(regexp (regexp-quote "Malformed input"))
          (lambda () (parse-fundef '(def funny (x x => (+ x x))))))


; Test Cases for top-interp
(check-equal? (top-interp '{{def main {() => {+ 1 2}}}}) 3)
(check-equal? (top-interp '{{def main {() => {* 1 {+ 2 3}}}}}) 5)
(check-equal? (top-interp '{{def main {() => {- 9 {+ 2 3}}}}}) 4)
(check-equal? (top-interp '{{def main {() => {ifleq0? -1 1 0}}}}) 1)
(check-equal? (top-interp '{{def main {() => {ifleq0? 1 1 0}}}}) 0)
(check-equal? (top-interp '{{def fun {() => {+ 5 5}}} {def main {() => {fun}}}}) 10)
(check-equal? (top-interp '{{def fun {(x y z) => {+ {+ x y} z}}} {def main {() => {+ {fun 3 3 3} {fun 3 4 5}}}}}) 21)
(check-equal? (top-interp '{{def main {() => {+ 9 {+ 2 {+ 3 2}}}}}}) 16)

