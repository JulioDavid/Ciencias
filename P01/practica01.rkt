#lang plai

;Ejercicio que dio el ayudante...
(define (mlength a-lst)
  (cond
    [(empty? a-lst) 0]
    [else (+1(mlength (cdr a-lst)))] ))

  
;Ejercicio Pow

(define (pow x w)
  (cond
    [(equal? 0 w) 1]
    [else  (* x (pow x (- w 1)) )] ))


;Ejercicio Average

(define (average list)
  (cond
    [(null? list) 0]
    [(equal? (length list) 1) (car list)]
    [else (+ (car list) (avaux (cdr list))) ]))

(define (avaux list)
  (cond 
    [(null? list) 0]
    (+ (car list) (avaux (cdr list))) ))