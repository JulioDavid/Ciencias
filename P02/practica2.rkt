#lang plai
(define-type Array
  [MArray (n number?)
          (l list?)])

(MArray 4 '(1 2 3)) 

(define-type MList
  [MEmpty]
  [MCons (e any?)(l MList?)])

;;Para aceptar elementos de cualquier tipo
(define (any? e)#t)

(define-type NTree
  [TLEmpty]
  [NodeN (n number?) (l listNTree?)])

(define (listNTree? lst)
(if (empty? lst)
#t
(and (NTree? (car lst)) (listNTree? (cdr lst)))))

(define-type Position
  [2D-Point(x number?) (y number?)])

(define-type Figure
  [Circle (c Position?) (n number?)]
  [Square(e Position?) (n number?)]
  [Rectangle(e Position?) (a number?) (l number?)])
