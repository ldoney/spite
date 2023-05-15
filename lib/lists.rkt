#lang racket
(include "util.rkt")

; Maps f to all elements of a list lst
; (T -> V) [Listof T] -> [Listof V]
(define (map f lst) (match lst
                      ['() '()]
                      [(cons x rst) (cons (f x) (map f rst))]))

; Takes slice of list lst from n (inclusive) to m (exclusive).
; If out of bounds or invalid n, m, throws an error.
; [Listof T] n m -> [Listof T]
(define (slice lst n m)   (if (< m n) 
                              (throw-error)
                              (sub-list (take-only lst m) n)))

; Trims n elements off the end of the list lst. If trim too large, 
; returns empty list.
; [Listof T] Integer -> [Listof T]
(define (trim lst n) (if (= (- (length lst) n) 0)
                            '()
                            (cons (car lst) (trim (cdr lst) n))))

; Takes only the first n element of the list. Throws an error if
; take only is too large
; [Listof T] Integer -> [Listof T]
(define (take-only lst n) (if (= n 0)
                             '()
                             (cons (car lst) (take-only (cdr lst) (sub1 n)))))

; Computes sublist of list starting at index n. Ideally,
; (union (sub-list lst n) (take-only lst n)) is equivalent to
; lst
; [Listof T] Integer -> [Listof T]
(define (sub-list lst n) (if (= n 0)
                            lst
                            (sub-list (cdr lst) (sub1 n))))

; Check if e is in the list lst
; T [Listof T] -> Boolean
(define (in-list? e lst)
  (match lst
    ['() #f]
    [(cons x rst) (if (util:equal? x e) #t (in-list? e rst))]))

; Compares two lists elementwise and determines
; if they are equivalent
; [Listof T] [Listof T] -> Boolean
(define (list-eq? lst1 lst2)
  (match lst1
    ['() (empty? lst2)]
    [(cons x1 rst1) (match lst2
      ['() #f]
      [(cons x2 rst2) (if (util:equal? x1 x2) (list-eq? rst1 rst2) #f)])]))

; Computes the length of the list lst
; [Listof T] -> Integer
(define (length lst) 
  (if (empty? lst) 0 (add1 (length (cdr lst)))))

; Generates a list using rest arguments
; [Restof T] -> [Listof T]
(define (list . xs) xs)

; Performs left fold on the list using f
; (Fun T T -> T) T [Listof T]
(define (foldl f acc lst)
  (if (empty? lst) acc 
      (foldl f (f (car lst) acc) (cdr lst))))

; Performs right fold on the list using f
; (Fun T T -> T) T [Listof T]
(define (foldr f acc lst)
  (if (empty? lst) acc
      (f (car lst) (foldr f acc (cdr lst)))))

; Reverses list lst
; [Listof T] -> [Listof T] 
(define (reverse lst)
  (reverse-aux lst '()))

; Helper function for above
; [Listof T] [Listof T] -> [Listof T] 
(define (reverse-aux lst tail)
  (if (empty? lst) tail
      (reverse-aux (cdr lst) (cons (car lst) tail))))

; Combines N lists together
; [Restof [Listof T]] -> [Listof T]
(define (append . xs)
  (foldl append-binary '() (reverse xs)))

; Combines two lists together
; [Listof T] [Listof T] -> [Listof T]
(define (append-binary lhs rhs)
  (if (empty? lhs)
      rhs
      (cons (car lhs) (append-binary (cdr lhs) rhs))))

; Inserts element e in list lst at position n
; [Listof T] Integer T -> [Listof T]
(define (insert lst n e)
  (if (= 0 n)
      (cons e lst)
      (cons (car lst) 
            (insert (cdr lst) (- n 1) e))))

; Generates numbers from start to end, both inclusive.
; Similar to python's range()
; Integer Integer -> [Listof Integer]
(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))
