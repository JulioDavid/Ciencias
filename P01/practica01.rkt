#lang plai

;Ejercicio que dio el ayudante...
(define (mlength a-lst)
  (cond
    [(empty? a-lst) 0]
    [else (+1(mlength (cdr a-lst)))] ))


; Ejercicio 1 Pow
(define (pow x w)
  (cond
    [(equal? 0 w) 1]
    [else  (* x (pow x (- w 1)) )] ))
;pruebas

;(test (pow 3 0) 1)
;(test (pow 0 0) 1)
;(test (pow 0 4) 0)
;(test (pow 2 3) 8)




; Ejercicio 2 Average
(define (average list)
  (cond
    [(empty? list) 0]
    [(equal? (length list) 1) (car list)]    
    [else (/ (avaux list) (length list))] ))

; Aux Function adds the elements in the list
(define (avaux list)
  (cond 
    [(empty? list) 0]
    ;adds the elements from the list: first element + avaux(the rest of the list).
    [else(+ (car list) (avaux (cdr list)))] ))
;Pruebas
;(test (average '(0)) 0)
;(test (average '()) 0)
;(test (average '(5)) 5)
;(test (average '(10 3 17)) 10)




;Función primes
;(define (primes n)
;  (cond
;    [(equal? 1 n) '()]
 ;   [(equal? 2 n) '(2)]
  ;  [else (
            
            
   ;         (test (primes 30) '(2 3 5 7 11 13 17 19 23 29))
;(test (primes 11) '(2 3 5 7 11))
;(test (primes 1) '())

;1.5 Función reduce el id se usa para hacer referencia de casos base especificos
(define (reduce f lst x)
  (cond
    [(empty? lst) x]
    [else(f(car lst)(reduce f (cdr lst)x))]))

    
(test (reduce + '(1 2 3 4 5 6 7 8 9 10) 0) 55)
(test (reduce zip '((1 2 3) (4 5 6) (6 7 8))'()) '((1 (4 7)) (2 (5 8)) (3 (6 9))))
(test (reduce * '(1 3 5 7) 1) 105)
(test (reduce - '(5 4) 0) 1)


;Función zip
(define (zip list1 list2)
  (cond
    [(empty? list1) '()]
    [(empty? list2) '()]
    [else (cons (cons (car list1) (cons (car list2) empty)) (zip (cdr list1) (cdr list2)))] ))

;(test (zip '() '()) '())
;(test (zip '(2 4) '()) '())
;(test (zip '() '(4 2)) '())
;(test (zip '(4) '(3)) '((4 3)))
;(test (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))
;(test (zip '(1 3 5) '(2 4)) '((1 2) (3 4)))




;mconcat Ejercicio 1 Sección 2
{define (mconcat list1 list2)
  (cond
    [(empty? list1) list2]
    [(empty? list2) list1]
    [else (cons (car list1) list2)])}
  
 ;  (test (mconcat '() '()) '())
 ;  (test (mconcat '() '(3 1)) '(3 1))
 ;  (test (mconcat '(4 6) '()) '(4 6))
 ;  (test (mconcat '(3) '(5 1)) '(3 5 1))
 ;  (test (mconcat '(3 4) '(5 1)) '(3 4 5 1))

;2.2 Función mmap

(define (mmap f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (car lst))
                (mmap f (cdr lst)))]))


(test (mmap add1'(1 2 3 4 5 6)) '(2 3 4 5 6 7)) 
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))


;2.1 Función mconcat
(define (mconcat l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (mconcat (cdr l1) l2))] ))
                        

(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(4 5 6)) '(4 5 6))
(test (mconcat '(1 2 3) '()) '(1 2 3))
(test (mconcat '()'())'())
(test (mconcat '(1) '(1))'(1 1))




   

;any? Ejercicio 4 Seccion 2
{define (any? pred? list1)
  (cond
    [(empty? list1) #f]
    [(equal? (pred?(car list1)) #t)#t]
    [else (any? pred?(cdr list1))])}

;(test (any? symbol? '(1 2 3)) #f)
;(test (any? boolean? '(a b #t)) #t)
;(test (any? number? '()) #f)
;(test (any? symbol? '(1 a 3)) #t)
;(test (any? number? '(a b c 1 e)) #t)




;every? Ejercicio 5 Seccion 2
{define (every? pred? list1)
  (cond
    [(empty? list1) #t]
    [(equal? (pred?(car list1)) #f)#f]
    [else (every? pred?(cdr list1))])}

;(test (every? symbol? '()) #t)
;(test (every? boolean? '(a b #t))#f)
;(test (every? number? '(1 2 a)) #f)
;(test (every? symbol? '(a b c d e)) #t)
;(test (every? number? '(a b c 1 e)) #f)
 



;mpowerset
(define (mpowerset list)
  (cond 
    [(empty? list) '(())]
    [else (define powerFun (mpowerset (cdr list)))
          (append powerFun 
                  (map (lambda (s) (cons (car list) s)) powerFun))]))

; Este método está basado en un método que ví en internet, lo leí, lo revisé paso por paso,
; y me parece que si le entiendo, pero la verdad no se me ocurrió como hacerlo,
; así que mejor pongo éste.  