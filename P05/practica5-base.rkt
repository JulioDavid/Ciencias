#lang plai
					;Según yo este base es identica a la de la practica 4, solo que en lugar de definir FAE definimos el RCFAEL.
					;Los parse son identicos, las bisquedas son iguales, no hay pedo con eso.

(define-type Binding
  [bind (name symbol?) (val RCFAEL?)])

(define-type RCFAEL
  [num (n number?)]
  [with (bindings (listof bind?))
	 (body RCFAEL?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [app (fun RCFAEL?)
       (args (listof RCFAEL?))]
  [binop (f procedure?)
	 (l RCFAEL?)
	 (r RCFAEL?)])

(define-type RCFAEL-Value
  [numV (n number?)]
  [closureV (param (listof symbol?))
	    (body RCFAEL?)
	    (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
	(value RCFAEL-Value?)
	(env Env?)])


; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst)
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
	(map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
	(error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]))

;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp)
(define (buscaRepetido l comp)
  (cond
   [(empty? l) #f]
   [(member? (car l) (cdr l) comp) (car l)]
   
   [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
   [(empty? l) #f]
   [(comparador (car l) x) #t]
   [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
(define (parse sexp)
  (cond
   [(symbol? sexp) (id sexp)]
   [(number? sexp) (num sexp)]
   [(list? sexp)
    (case (car sexp)
      [(with) (with (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
      ;;[(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
      [(fun) (fun (cadr sexp) (parse (caddr sexp)))]
      [(+ - / *) (binop (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
      [else (app (parse (car sexp)) (map parse (cdr sexp)))])]))
