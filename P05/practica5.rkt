#lang plai

(require "practica5-base.rkt")

;(print-only-errors true)

;desugar
(define (desugar expr)
  (type-case RCFAELS expr
    [numS (n) (numV n)]
    [boolS (v) (bool v)] ;boolV?
    [idS (s) (id s)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body)                   
	   (app (fun (map (lambda (bind)
			    (bind-name bind)) bindings) 
		     (desugar body))                             
		(map (lambda (bind)            
		       (desugar (bind-val bind))) bindings))]                               
    [funS (params body) (fun params (desugar body))]                                 
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [if0S (i j k) 
           (error "not implemented yet")]
    [lstS (i )
         (error "not implemented yet")]
    [recS (id expr body) (error "no implemented yet")] ))

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
    [bool (v) (bool v)] ;boolV?
    [id (v) (lookup v env)]
    [binop (f l r) (numf f (interp l env) (interp r env))]
    [fun (fun-id fun-body)
         (closureV fun-id fun-body env)]
    [app (fun-id fun-body)
         (local([define fun-val(interp fun-id env)])
               (interp (closureV-body fun-val)
                      (aSub (closureV-param fun-val)
                            (map (lambda (arg) (interp arg env)) fun-body)
                            (closureV-env fun-val))))]
    [if0 (con then else)
         (if (bool (interp con env)) ;num-zero? 
             (interp then env)
             (interp else env))]  
    [rec (id expr body)
      (interp body
              (cyclically-bind-and-interp id
                                          expr
                                          env))]
    [with  (bound-id named-expr bound-body)
           (interp bound-body
                   (aSub bound-id
                         (interp named-expr env) env))] ;page 113 from Shriram's book
    [Mlist (i)
         (error "not implemented yet")]  ))

;;Cyclically-bind-and-interp : symbol RCFAEL env -> env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

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