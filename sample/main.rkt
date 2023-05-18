#lang spite
(include "mylib.rkt")
(as "lib" (include "mylib2.rkt"))
(define (f x) (add1 x))
(f (lib:first (mylib:list 1 2 3)))
