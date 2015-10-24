#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

(define (desugar expr)
  (type-case FAES expr
    [numS(n) (num n)] 
    [idS (s) (id s)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body)                            
	   (app (fun (map (lambda (bind)
			    (bind-name bind)) bindings) 
		     (desugar body))                             
		(map (lambda (bind)            
		       (desugar (bind-val bind))) bindings))]                     
    [with*S (bindings body) (matryoshka bindings body)]            
    [funS (params body) (fun params (desugar body))]                                 
    [appS (fin args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]))


(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun (list (bind-name (car bindings)))
                    (matryoshka (cdr bindings) body))
               (list (desugar (bind-val (car bindings)))))] ))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))


  (define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [binop (f l r) (numf f (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body) (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)]) 
           (interp (closureV-body fun-val) 
                   (aSub (closureV-param fun-val) 
                         (interp arg-expr ds) 
                         (closureV-env fun-val))))]))

(define (lookup name ds)
  (type-case Env ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds) (if (symbol=? bound-name name)
                                               bound-value
                                               (lookup name rest-ds))]))

(define (numf f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (rinterp expr)
  (interp expr (mtSub)))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))



    ;; [withS (bindings body) (app (fun (map (lambda (x) (bind-name x)) bindings)
    ;;                                  (desugar-body))
    ;;                             (map (lambda (x) (desugar (bind-val x))) bindings))]
    ;; [idS (name) (id name)]
    ;; [funS (params body) (fin params (desugar body))]
    ;; [else '()]))
