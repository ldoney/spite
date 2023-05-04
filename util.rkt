#lang racket
(provide (all-defined-out))

; ((Fun T V) -> Bool) T List -> List
(define (cons-nodupes eq x lst)
  (if (in-list? eq x lst) lst (cons x lst)))

; ((Fun T V) -> Bool) List List -> List
(define (append-nodupes eq lst1 lst2)
  (match lst2
    ['() lst1]
    [(cons lib rst2) (if (in-list? eq lib lst2) 
                         (append-nodupes eq lst1 rst2) 
                         (cons lib (append-nodupes eq lst1 rst2)))]))

; ((Fun T V) -> Bool) List T -> List
(define (in-list? eq value lst)
 (cond
  ['() #f]
  [(eq (car lst) value) #t]
  [else (in-list? eq (cdr lst) value)]))

