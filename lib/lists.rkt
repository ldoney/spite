#lang racket
(define (map f lst) (match lst
                      ['() '()]
                      [(cons x rst) (cons (f x) (map f rst))]))
(define (slice lst n m)   (if (< m n) 
                              lst
                              (sub-list (take-only lst m) n)))
(define (trim lst n) (if (= (- (list-length lst) n) 0)
                            '()
                            (cons (car lst) (trim (cdr lst) n))))
(define (take-only lst n) (if (= n 0)
                             '()
                             (cons (car lst) (take-only (cdr lst) (sub1 n)))))
(define (sub-list lst n) (if (= n 0)
                            lst
                            (sub-list (cdr lst) (sub1 n))))
(define (list-length lst) (if (empty? lst) 0 (add1 (list-length (cdr lst)))))
