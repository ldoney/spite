#lang spite

(define (and e1 e2)
  (if e1
    (if e2
      #t
      #f)
    #f))
