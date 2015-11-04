#lang plai
;Según yo este base es identica a la de la practica 4, solo que en lugar de definir FAE definimos el RCFAEL.
;Los parse son identicos, las bisquedas son iguales, no hay pedo con eso.

;Bindings for RCFAELS
(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])

;RCFAELS type definition, for desugar
(define-type RCFAELS
  [numS (n number?)]
  [boolS (v boolean?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
       (body RCFAELS?)]
  [appS (fun RCFAELS?)
       (args (listof RCFAELS?))]
  [binopS (f procedure?)
         (l RCFAELS?)
         (r RCFAELS?)]
  [withS (bindings 
         (listof bind?))(body RCFAELS?)]
  [if0S (cond RCFAELS?)
       (then RCFAELS?)
       (else RCFAELS?)]
  [recS (id RCFAELS?) (expr RCFAELS?) (body RCFAELS?)]
  [lstS (error "not implemented yet")] )


;RCFAEL type definition
(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (v boolean?)]
  [Mlist (params (listof RCFAEL?))];Maybe MList? Maybe not MList ;(error "not implemented yet")]
  [with (name symbol?) (named-expr RCFAEL?) (body RCFAEL?)] ;(bindings (listof bind?))(body RCFAEL?)]
  [rec (id RCFAEL?) (expr RCFAEL?) (body RCFAEL?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [if0 (cond RCFAEL?)
        (then RCFAEL?)
        (else RCFAEL?)]
  ;[equal? (id1 id2)]
  [app (fun RCFAEL?)
       (args (listof RCFAEL?))]
  [binop (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)])


;Type-Value
(define-type RCFAEL-Value
  [numV (n number?)]
  [closureV (param (listof symbol?))
	    (body RCFAEL?)
	    (env Env?)])

;Enviroment definition
(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAEL-Value?)
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAEL-Value?)
           (env Env?)])

;box
(define (boxed-RCFAEL-Value? v)
  (and (box? v)
       (RCFAEL-Value? (unbox v))))

; FUNCIONES AUXILIARES 
; WE NEED TO MODIFY FROM DOWN HERE.

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
    [(/) /]
    [(<) <]
    [(<=) <=]
    [(>) >]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))] ))

(define-type MList
  [MEmpty]
  [MCons (e RCFAEL?) (lst MList?)])

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
