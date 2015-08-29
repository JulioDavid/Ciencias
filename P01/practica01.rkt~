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

(test (pow 3 0) 1)
(test (pow 0 0) 1)
(test (pow 0 4) 0)
(test (pow 2 3) 8)




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
(test (average '(0)) 0)
(test (average '()) 0)
(test (average '(5)) 5)
(test (average '(10 3 17)) 10)




;Función zip
(define (zip list1 list2)
  (cond
    [(empty? list1) '()]
    [(empty? list2) '()]
    [else (cons (cons (car list1) (cons (car list2) empty)) (zip (cdr list1) (cdr list2)))] ))

(test (zip '() '()) '())
(test (zip '(2 4) '()) '())
(test (zip '() '(4 2)) '())
(test (zip '(4) '(3)) '((4 3)))
(test (zip '(1 3 5) '(2 4 6)) '((1 2) (3 4) (5 6)))
(test (zip '(1 3 5) '(2 4)) '((1 2) (3 4)))




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
   
   