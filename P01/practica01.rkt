#lang plai

;Ejercicio que dio el ayudante...
(define (mlength a-lst)
  (cond
    [(empty? a-lst) 0]
    [else (+1(mlength (cdr a-lst)))])


