#lang racket
(define (mult n m) (if (= n 0) 0 (+ (mult (- n 1) m) m)))
(define (fact n) (if (= n 0) 1 (mult n (fact (- n 1)))))
