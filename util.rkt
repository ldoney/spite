#lang racket
(provide (all-defined-out))

(define (symlist->string lst)
  (match lst
    ['() ""]
    [(cons x rst) (string-append (tostring x) (if (empty? rst) "" ", ") (symlist->string rst))]))

(define (tostring x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(string? x) x]
    [(boolean? x) (if x "#t" "#f")]
;    [(integer? x) (integer->string x)]
    [else x]))

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

; ((Fun T V) -> Bool) T List -> Bool 
(define (in-list? cond? value lst)
  (match lst
    ['() #f]
    [(cons h rst) (if (cond? value h)
                      #t
                      (in-list? cond? value rst))]))

(define (get-file-dir s)
  (string-join (reverse (cdr (reverse (string-split s "/")))) "/"))

(define (read-all-file f)
  (let ((e (read f))) 
    (if (eof-object? e) 
      '()
      (cons e (read-all-file f)))))
