#lang racket
(define (map f lst) (match lst
                      ['() '()]
                      [(cons x rst) (cons (f x) (map f rst))]))
(define (slice lst n m)   (if (< m n) 
                              lst
                              (sub-list (take-only lst m) n)))
(define (trim lst n) (if (= (- (length lst) n) 0)
                            '()
                            (cons (car lst) (trim (cdr lst) n))))
(define (take-only lst n) (if (= n 0)
                             '()
                             (cons (car lst) (take-only (cdr lst) (sub1 n)))))
(define (sub-list lst n) (if (= n 0)
                            lst
                            (sub-list (cdr lst) (sub1 n))))
(define (length lst) 
  (if (empty? lst) 0 (add1 (length (cdr lst)))))

(define (list . xs) xs)

(define (foldl f acc lst)
  (if (empty? lst) acc 
      (foldl f (f (car lst) acc) (cdr lst))))

(define (foldr f acc lst)
  (if (empty? lst) acc
      (f (car lst) (foldr f acc (cdr lst)))))

(define (reverse-aux lst tail)
  (if (empty? lst) tail
      (reverse-aux (cdr lst) (cons (car lst) tail))))

(define (reverse lst)
  (reverse-aux lst '()))

; Doesn't seem to work.. not sure why
(define (append . xs)
  (foldl append-binary '() (reverse xs)))

(define (append-binary lhs rhs)
  (if (empty? lhs)
      rhs
      (cons (car lhs) (append-binary (cdr lhs) rhs))))

(define (insert lst n e)
  (if (= 0 n)
      (cons e lst)
      (cons (car lst) 
            (insert (cdr lst) (- n 1) e))))

(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))
