#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

; type Defn = (Defn Id Fun)
(struct Defn (f fun) #:prefab)
 
; type Fun = (FunPlain [Listof Id] Expr)
;          | (FunRest [Listof Id] Id Expr)
(struct FunPlain (xs e)   #:prefab)
(struct FunRest  (xs x e) #:prefab)

; type Lam = (LamPlain [Listof Id] Expr)
;          | (LamRest [Listof Id] Id Expr)
(struct LamPlain (xs e)   #:prefab)
(struct LamRest  (xs x e) #:prefab)

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Str String)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (Prim3 Op3 Expr Expr Expr)
;;           | (PrimN OpN [Listof Expr])
;;           | (If Expr Expr Expr)
;;           | (Begin (Listof Expr))
;;           | (Let  [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;;           | (Let* [Listof Id] [Listof Expr] Expr)  ; lengths must be equal
;;           | (Var Id)
;;           | (Cond [ListOf CondClause] Expr)
;;           | (Case Expr [ListOf CondClause] Expr)
;;           | (Match Expr (Listof Pat) (Listof Expr))
;;           | (App Expr (Listof Expr))
;;           | (Apply Id (Listof Expr) Expr)
;;           | (Lam Id Fun)
;; type Id   = Symbol
;; type Op0  = 'read-byte
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'empty? | 'cons? | 'box?
;;           | 'vector? | 'vector-length
;;           | 'string? | 'string-length
;;           | close    | read
;;           | write    | print
;;           | listen   | accpet
;;           | on-message | closed?
;;           | println
;; type Op2  = '+ | '- | '< | '=
;;           | 'cons
;;           | 'make-vector | 'vector-ref
;;           | 'make-string | 'string-ref
;;           |  open        | read
;;           | write        | open-sock
;; type Op3  = 'vector-set!
;; type CondClause = (Clause Expr Expr)
;; type CaseClause = (Clause [Listof Datum] Expr)
;; type Datum = Integer | Boolean | Character
;; type Pat  = (PVar Id)
;;           | (PWild)
;;           | (PLit Lit)
;;           | (PBox Pat)
;;           | (PCons Pat Pat)
;;           | (PAnd Pat Pat)
;; type Lit  = Boolean
;;           | Character
;;           | Integer
;;           | '()

(struct Eof   ()           #:prefab)
(struct Empty ()           #:prefab)
(struct Int   (i)          #:prefab)
(struct Bool  (b)          #:prefab)
(struct Char  (c)          #:prefab)
(struct Str   (s)          #:prefab)
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct PrimN (p es)       #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Begin (es)         #:prefab)
(struct Let   (xs es e)    #:prefab)
(struct Let*  (xs es e)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (e es)       #:prefab)
(struct Apply (f es e)     #:prefab)
(struct Lam   (f lam)      #:prefab)
(struct Match (e ps es)    #:prefab)
(struct Cond  (clist el)   #:prefab)
(struct Case  (ev cs el)   #:prefab)
(struct Clause (p b)       #:prefab)

(struct PVar  (x)          #:prefab)
(struct PWild ()           #:prefab)
(struct PLit  (x)          #:prefab)
(struct PBox  (p)          #:prefab)
(struct PCons (p1 p2)      #:prefab)
(struct PAnd  (p1 p2)      #:prefab)



; Util functions for AST
(define (fun->lam fun)
  (match fun
    [(Defn f fun) (Lam f (fun->lam fun))]
    [(FunPlain xs e)   (LamPlain xs e)]
    [(FunRest  xs x e) (LamRest xs x e)]))

(define (lam->fun lam)
  (match lam 
    [(Lam f lam) (Defn f (lam->fun lam))]
    [(LamPlain xs e)   (FunPlain xs e)]
    [(LamRest  xs x e) (FunRest xs x e)]))

(define (get-lambda-xs lam)
  (match lam 
    [(LamPlain xs _)   xs]
    [(LamRest  xs x e) (cons x xs)]))

(define (map-on-lambda-e f lam)
  (match lam 
    [(LamPlain _ e)   (f e)]
    [(LamRest  _ _ e) (f e)]))

(define (map-on-fun-e f fun)
  (match fun 
    [(FunPlain _ e)   (f e)]
    [(FunRest  _ _ e) (f e)]))

