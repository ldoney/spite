#lang racket
(require "ast.rkt")
(provide lambdas)


;; Prog -> [Listof Lam]
;; List all of the lambda expressions in p
(define (lambdas p)
  (match p
    [(Prog ds e)
     (append (lambdas-ds ds) (lambdas-e e))]))

;; Defns -> [Listof Lam]
;; List all of the lambda expressions in ds
(define (lambdas-ds ds)
  (match ds
    ['() '()]
    [(cons (Defn f fun) ds)
     (append (map-on-fun-e lambdas-e fun)
             (lambdas-ds ds))]))

;; Expr -> [Listof Lam]
;; List all of the lambda expressions in e
(define (lambdas-e e)
  (match e
    [(Prim1 p e)        (lambdas-e e)]
    [(Prim2 p e1 e2)    (append (lambdas-e e1) (lambdas-e e2))]
    [(Prim3 p e1 e2 e3) (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(PrimN p es)       (append-map lambdas-e es)]
    [(If e1 e2 e3)      (append (lambdas-e e1) (lambdas-e e2) (lambdas-e e3))]
    [(Begin es)         (append-map lambdas-e es)]
    [(Let xs es e2)     (append (append-map lambdas-e es) (lambdas-e e2))]
    [(Let* xs es e2)    (append (append-map lambdas-e es) (lambdas-e e2))]
    [(App e1 es)        (append (lambdas-e e1) (append-map lambdas-e es))]
    [(Lam f lam)        (cons (Lam f lam) (map-on-lambda-e lambdas-e lam))]
    [(Match e ps es)    (append (lambdas-e e) (append-map lambdas-e es))]
    [_                  '()]))
