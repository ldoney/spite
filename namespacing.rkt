#lang racket
(require "ast.rkt" "fv.rkt" "util.rkt")

(provide merge-ds-libs)

; String Defn -> Defn
(define (attach-names name d)
  (match d
    [(Defn f xs e) (Defn (append-name-to-symbol name f) xs (replace-in-namespace name (remq* xs (fv e)) e))]))

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
      [(Prim3 p e1 e2 e3) (Prim2 p (r e1) (r e2) (r e3))]
      [(Var x)            (Var     (if (and 
                                         (not (string-contains? (symbol->string x) ":")) 
                                         (in-list? equal? x fvs)) 
                                     (append-name-to-symbol name x) 
                                     x))]
      [(If e1 e2 e3)      (If      (r e1) (r e2) (r e3))]
      [(Begin e1 e2)      (Begin   (r e1) (r e2))]
      [(Let x e1 e2)      (Let     (append-name-to-symbol name x) (r e1) (r e2))]
      [(App e1 es)        (App     (r e1) (map r es))]
      [(Lam f xs e1)      (Lam     f xs (replace-in-namespace name (remq* xs fvs) e1))]
      [(Match e ps es)    (Match   (r e) ps (map r es))]
      [x                  x])))

(define (merge-ds-libs ds libs)
  (match libs
    ['() ds]
    [(cons (Lib name lib-ds depend) rst) 
      (append (map (lambda (x) (attach-names name x)) lib-ds)
              (merge-ds-libs ds rst))]))

