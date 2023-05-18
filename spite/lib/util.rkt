#lang spite
(include "strings.rkt")
(include "lists.rkt")

; General statement of equality for two expressions e1, e2 of same type
; T T -> Boolean
(define (equal? e1 e2)
 (cond
  [(and (integer? e1) (integer? e2)) (= e1 e2)]
  [(and (string? e1) (string? e2)) (strings:str-eq? e1 e2)]
  [(and (boolean? e1) (boolean? e2)) (xor e1 e2)]
  [(and (vector? e1) (vector? e2)) (vec-eq? e1 e2)]
  [(and (empty? e1) (empty? e2)) #t]
  [(and (cons? e1) (cons? e2)) (lists:list-eq? e1 e2)]
  [(and (char? e1) (char? e2)) (= (char->integer e1) (char->integer e2))]
  [else #f]))

; Checks if two vectors are equal (exactly the same as string from lib/strings.rkt 
; except with vector funs)
; Vector Vector -> Boolean
(define (vec-eq? v1 v2) 
  (if (util:and (vector? v1) (vector? v2))
    (if (= (vector-length v1) (vector-length v2))
        (vec-eq-helper v1 v2 (sub1 (vector-length v1)))
        #f)
    #f))

; Helper function for above
; Vector Vector Integer -> Boolean
(define (vec-eq-helper v1 v2 index)
  (match index
    [0 (if (equal? (vector-ref v1 index) (vector-ref v2 index)) #t #f)]
    [_ (if (equal? (vector-ref v1 index) (vector-ref v2 index)) (vec-eq-helper v1 v2 (- index 1)) #f )]))

; Computes xor of two boolean expressions
; Boolean Boolean -> Boolean
(define (xor e1 e2) (or (and e1 e2) (and (not e1) (not e2))))

; Computes not of a boolean expression
; Boolean -> Boolean
(define (not e) (if e #f #t))

; N-ary or of boolean expressions
; [Restof Boolean] -> Boolean
(define (or . es)  (lists:foldl or-bin #f es))

; N-ary and of boolean expressions
; [Restof Boolean] -> Boolean
(define (and . es) (lists:foldl and-bin #t es))

; Computes or of two boolean expressions
; Boolean Boolean -> Boolean
(define (or-bin e1 e2) (not (and (not e1) (not e2))))

; Computes and of two boolean expressions
; Boolean Boolean -> Boolean
(define (and-bin e1 e2) (if e1 (if e2 #t #f) #f))
