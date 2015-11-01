#lang plai

(require "practica5-base.rkt")

;(print-only-errors true)

;desugar
(define (desugar expr)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [id (s) (id s)]
    [binop (f l r) (binop f (desugar l) (desugar r))]
    [with (bindings body env) ;this does NOT works                           
	   (app (fun (map (lambda (bind)
			    (bind-name bind)) bindings) 
		     (desugar body))                             
		(map (lambda (bind)            
		       (desugar (bind-val bind))) bindings))]                               
    [fun (params body) (fun params (desugar body))]                                 
    [app (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [if0 (i j k) 
           (error "not implemented yet")]
    [lst (i )
         (error "not implemented yet")]
    [rec (id expr body) (error "no implemented yet")] ))

(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun (list (bind-name (car bindings)))
                    (matryoshka (cdr bindings) body))
               (list (desugar (bind-val (car bindings)))))] ))

;Interp
(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [id (v) (lookup v env)]
    [binop (f l r) (numf f (interp l env) (interp r env))]
    [fun (fun-id fun-body)
         (closureV fun-id fun-body env)]
    [app (fun-id fun-body)
         (local([define fun-val(interp fun-id env)])
               (interp(closureV-body fun-val)
                      (aSub (closureV-param fun-val)
                            (interp fun-body env)
                            (closureV-env fun-val))))]
    [if0 (if then else)
         (if (num-zero? (interp test env)) ;num-zero? 
             (interp then env)
             (interp else env))]  
    [rec (id expr body)
      (interp body
              (cyclically-bind-and-interp id
                                          expr
                                          env))]
    [with (bound-id named-expr bound-body)
          (error "not implemented yet")]
    [lst (i)
         (error "not implemented yet")]  ))

;Lookup
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup name rest-env))] ))

(define (rinterp expr)
  (interp expr (mtSub)))

(define (numf f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (cparse sexp)
  (desugar (parse sexp)))