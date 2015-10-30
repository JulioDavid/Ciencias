#lang plai

(require "practica5-base.rkt")

;(print-only-errors true)

(define (desugar expr)
  )


(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [id (v) (lookup v env)]
    [binop (f l r) (numf f (interp l env) (interp r env))]
    [fun (fun-id fun-body)
         (closureV fun-id fun-body env)]
    [app (fun-id fun-body)
         (error "not implemented yet")]
    [with (bindings body)
          (error "not implemented yet")] ))

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
          (lookup name rest-env))] ))

(define (rinterp expr)
  (interp expr (mtSub)))

(define (numf f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (cparse sexp)
  (desugar (parse sexp)))


    
    
    
    
    
    