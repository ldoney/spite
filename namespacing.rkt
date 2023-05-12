#lang racket
(require "ast.rkt" "fv.rkt" "util.rkt")

(provide merge-ds-libs attach-names)

; String Defn -> Defn
(define (attach-names name d)
  (match d
    [(Defn f fun) (Defn (append-name-to-symbol name f) (match fun
      [(FunPlain xs e)  (FunPlain xs  (replace-in-namespace name (remq* xs (fv e)) e))]
      [(FunRest xs x e) (FunRest xs x (replace-in-namespace name (remq* (cons x xs) (fv e)) e))]))]))

; String Symbol -> Symbol
(define (append-name-to-symbol name x)
  (string->symbol (string-append name ":" (symbol->string x))))

; String [Listof Id] Expr -> Expr
(define (replace-in-namespace name fvs e)
  (let ((r (lambda (ex) (replace-in-namespace name fvs ex))))
    (match e
      [(cons x rst)       (cons (r x) (r rst))]
      [(Prim1 p e)        (Prim1 p (r e))]
      [(Prim2 p e1 e2)    (Prim2 p (r e1) (r e2))]
      [(Prim3 p e1 e2 e3) (Prim3 p (r e1) (r e2) (r e3))]
      [(Var x)            (Var     (if (and 
                                         (not (string-contains? (symbol->string x) ":")) 
                                         (in-list? equal? x fvs)) 
                                     (append-name-to-symbol name x) 
                                     x))]
      [(If e1 e2 e3)      (If      (r e1) (r e2) (r e3))]
      [(Begin es)         (Begin   (map r es))]
      [(Cond clist el)    (Cond (map r clist) (r el))]
      [(Case ev clist el) (Case (r ev) (map r clist) (r el))]
      [(Clause p b)       (Clause (r p) (r b))]
      [(Let x e1 e2)      (Let     x (r e1) (r e2))]
      [(App e1 es)        (App     (r e1) (map r es))]
      [(Lam f lam)        (Lam     f (match lam
        [(LamPlain xs e)  (LamPlain xs (replace-in-namespace name (remq* xs fvs) e))]
        [(LamRest xs x e) (LamRest xs x (replace-in-namespace name (remq* (cons x xs) fvs) e))]))]
      [(Match e ps es)    (Match   (r e) ps (map r es))]
      [x                  x])))

(define (merge-ds-libs ds libs)
  (match libs
    ['() ds]
    [(cons (Lib name lib-ds depend) rst) 
      (append (map (lambda (x) (attach-names name x)) lib-ds)
              (merge-ds-libs ds rst))]))
