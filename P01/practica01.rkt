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


; Ejercicio 4 Zip INCOMPLETO
;(define (zip list1 list2)
 ; (cond
  ;  [(empty? list1) '()]
   ; [(empty? list2) '()]
   ; [else 
     
;Funcion aux para zip
(define (auxzi list1 list2)
  (cond
    [(null? list1) '()]
    [(null? list2) '()]
    [else (cond (car list1) (car list2))] ))

;mconcat Ejercicio 1 Sección 2
{define (mconcat list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [else (cons (car list1) (car list2)) (mconcat (cdr list1)(cdr list2))] )}