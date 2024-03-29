#lang racket
(require "ast.rkt")
(provide fv fv*)

;; Expr -> [Listof Id]
;; List all of the free variables in e
(define (fv e)
  (remove-duplicates (fv* e)))

(define (fv* e)  
  (match e
    [(Var x)            (list x)]
    [(Prim1 p e)        (fv* e)]
    [(Prim2 p e1 e2)    (append (fv* e1) (fv* e2))]
    [(Prim3 p e1 e2 e3) (append (fv* e1) (fv* e2) (fv* e3))]
    [(If e1 e2 e3)      (append (fv* e1) (fv* e2) (fv* e3))]
    [(Begin es)         (append-map fv* es)]
    [(Let xs es e2)     (append (append-map fv* es) (remq* xs (fv* e2)))]
    [(Let* xs es e2)    (append (remq* xs (append-map fv* es)) (remq* xs (fv* e2)))]
    [(Cond clist el)    (append (append-map fv* clist) (fv* el))]
    [(Case ev clist el) (append (fv* ev) (append-map fv* clist) (fv* el))]
    [(Clause p b)       (append (fv* p) (fv* b))]
    [(App e1 es)        (append (fv* e1) (append-map fv* es))]
    [(Lam f lam)        (remq* (get-lambda-xs lam) (map-on-lambda-e fv* lam))]
    [(Match e ps es)    (append (fv* e) (append-map fv-clause* ps es))]
    [_                  '()]))

;; Pat Expr -> [Listof Id]
(define (fv-clause* p e)
  (remq* (bv-pat* p) (fv* e)))

;; Pat -> [Listof Id]
(define (bv-pat* p)
  (match p
    [(PVar x) (list x)]
    [(PCons p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PAnd p1 p2) (append (bv-pat* p1) (bv-pat* p2))]
    [(PBox p) (bv-pat* p)]
    [_ '()]))
