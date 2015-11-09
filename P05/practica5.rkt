#lang plai

(require "practica5-base.rkt")

;(print-only-errors true)

;desugar
(define (desugar expr)
  (type-case RCFAELS expr
    [numS (n) (num n)]
    [boolS (v) (bool v)]
    [idS (s) (id s)]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [opS (f l)(op f(desugar l))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body)                   
	   (app (fun (map (lambda (bind)
			    (bind-name bind)) bindings) 
		     (desugar body))                             
		(map (lambda (bind)            
	       (desugar (bind-val bind))) bindings))]
    [with*S (bindings body)
             (matryoshka bindings body)]
    [if0S (i j k) 
           (if0 (desugar i)
                (desugar j)
                (desugar k))]
    [recS (id expr body) (rec id (desugar expr) (desugar body))]
    [equal?S (id1 id2) (isequal? (desugar id1)(desugar id2))]
    [MListS (i j) (Mlist (desugar i)(desugar j))]))

(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun (list (bind-name (car bindings)))
                    (matryoshka (cdr bindings) body))
               (list (desugar (bind-val (car bindings)))))] ))

;Interp
(define (interp expr env)
  (type-case RCFAEL expr
    [bool (v) (boolV v)]
    [id (v) (lookup v env)]
    [num (n) (numV n)]
    [op (f l)(opUna f(interp l env))]
    [binop (f l r) (opBina f (interp l env) (interp r env))]
    [fun (fun-id fun-body)
         (closureV fun-id fun-body env)]
    [app (fun-id fun-body)
         (local([define fun-val(interp fun-id env)])
               (interp (closureV-body fun-val)
                      (args (closureV-param fun-val)
                            (map (lambda (arg) (interp arg env)) fun-body)
                            (closureV-env fun-val))))]
    [if0 (con then else)
         (if (bool (interp con env))
             (interp then env)
             (interp else env))]
    [isequal? (id1 id2)(eq? (interp id1 env)(interp id2 env))]
    [rec (id expr body)
      (interp body
              (cyclically-bind-and-interp id
                                          expr
                                          env))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  (aSub bound-id
                        (interp named-expr env) env))] ;page 113 from Shriram's book
    [Mlist (e lst) (MCons (interp e env) (interp lst env))] ))

;;Mete los argumentos al ambiente
(define (args param ar env)
  (cond
    [(empty? param) env]
    [else (aSub (car param) (car ar) (args (cdr param) (cdr ar) env))]))

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

;;operacion binaria- recibe los parametros y la operacion a realizar con ellos

(define (opBina f p1 p2)
  (cond
    [(and (numV? p1) (numV? p2)) 
     (let ((res (f (numV-n p1) (numV-n p2)))) 
       (if (number? res) (numV res) (boolV res)))]
    [(and (boolV? p1) (boolV? p2))
     (let ((res (f (boolV-b p1) (boolV-b p2))))
       (if (boolean? res) (boolV res)(error "No op")))] ))


;;operacion unaria=- recibe parametro y operacion a realizar
(define (opUna f p1)
  (cond
   [(numV? p1) (let ((res (f (numV-n p1))))
                   (if (number? res) (numV res)
                       (boolV res)))]
   [(boolV? p1) (let ((res (f (boolV-b p1))))
                   (if (number? res) (numV res)
                       (boolV res)))]))

(define (eq? id1 id2)
  (cond
    [(and (numV? id1) (numV? id2))(boolV (= id1 id2))]
    [(and (boolV? id1)(boolV? id2)) (boolV (equal? id1 id2))]
    [(and (list? id1) (list? id2))(boolV (= (length id1) (length id2)))]
    [else "La aplicación de equal? no es adecuada"]))

(define (rinterp expr)
  (interp expr (mtSub)))

(define (cparse sexp)
  (desugar (parse sexp)))


;Sí son iguales a los de la practica 4. RCFAEL es una versión extendida de FAE.
;(test (rinterp (cparse '3)) (numV 3))
;(test (rinterp (cparse '{+ 3 4})) (numV 7))
;(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
;(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
;(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
;(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
;(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
;(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
;(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
;(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
;(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
;(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
;(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
;(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
;(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
;(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
;(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))

;;Pruebas para el resto de RCFAEL
;(test (rinterp (cparse '#t)) (boolV #t))
(test (rinterp (cparse '(isequal? 5 5))) (boolV #t))