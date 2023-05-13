#lang spite
(include "strings.rkt")
(include "lists.rkt")

(define (equal? e1 e2)
 (cond
  [(and (integer? e1) (integer? e2)) (= e1 e2)]
  [(and (string? e1) (string? e2)) (strings:str-eq? e1 e2)]
  [(and (boolean? e1) (boolean? e2)) (xor e1 e2)]
  [(and (vector? e1) (vector? e2)) (vec-eq? e1 e2)]
  [(and (empty? e1) (empty? e2)) #t]
  [(and (cons? e1) (cons? e2)) (lists:list-eq? e1 e2)]
  [else #f]))


(define (vec-eq? v1 v2) 
  (if (util:and (vector? v1) (vector? v2))
    (if (= (vector-length v1) (vector-length v2))
        (vec-eq-helper v1 v2 (sub1 (vector-length v1)))
        #f)
    #f))

(define (vec-eq-helper v1 v2 index)
  (match index
    [0 (if (equal? (vector-ref v1 index) (vector-ref v2 index)) #t #f)]
    [_ (if (equal? (vector-ref v1 index) (vector-ref v2 index)) (vec-eq-helper v1 v2 (- index 1)) #f )]))

(define (xor e1 e2) (or (and e1 e2) (and (not e1) (not e2))))
(define (not e) (if e #f #t))
(define (or e1 e2) (not (and (not e1) (not e2))))
(define (and e1 e2) (if e1 (if e2 #t #f) #f))
