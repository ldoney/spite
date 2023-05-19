#lang spite

(include "lists.rkt")

;; Removes the first character of a string
(define (remove-first-char str-list)
  (cdr str-list))

;; Gets the first n chars of a string
(define (get-first-chars n str-list)
  (lists:take-only str-list n))

;; Removes first n chars of string
(define (remove-first-chars n str-list)
  (lists:slice str-list n (lists:length str-list)))

(define (append sl1 sl2)
  (lists:append-binary sl1 sl2))

(define (get-index sl c)
  (get-index-helper sl c 0))

(define (get-index-helper sl c index)
  (match sl
    ['() -1]
    [(cons char chars)
     (if (eq? char c)
       index
       (get-index-helper chars c (add1 index)))]))

(define (str-char sl c)
  (let ([index (get-index sl c)])
    (if (< -1 index)
      (remove-first-chars index sl)
      sl)))

(define (str-eq? sl1 sl2)
  (lists:list-eq? sl1 sl2))
