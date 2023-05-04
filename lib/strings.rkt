#lang racket
; TODO Implement assembly-level string ops... right now strings are too rudimentary
; to implement most of these, hence str->char-list, but we should remove that because
; we can't implement slice or tail-end with it. So, right now, neither of those 
; functions work.
(define (s-car s) (car s));(string-ref s 0))
(define (s-cdr s) (cdr s))
(define (s-cons s rst) (cons s rst))
(define (slice s n m)     (if (< m n) 
                              s
                              (tail-end (sub-string s n) m)))
(define (tail-end s n) (if (< (string-length s) n)
                            '()
                            (s-cons (s-car s) (tail-end (s-cdr s) n))))
(define (sub-string s n) (if (= n 0)
                            s
                            (sub-string (s-cdr s) (sub1 n))))
(define (str->char-list str) (str->char-list-rec str 0))
(define (str->char-list-rec str n) (if (= (string-length str) n)
                                '()
                                (cons (string-ref str n) (str->char-list-rec str (add1 n)))))
