#lang racket
(provide parse parse-define parse-raw parse-e)
(require "ast.rkt" "util.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match (parse-raw s)
    [(RawProg ds includes e) (Prog ds (remove-dupes (flatten-libs (includes->libs '() includes))) e)]))

;; [Listof S-Expr] -> RawProg
(define (parse-raw s) 
  (match s
    [(cons (and (cons (or 'include 'as) _) i) s) 
      (match (parse-raw s)
        [(RawProg p-ds incls e)
        (RawProg p-ds (cons (parse-include i) incls) e)])]
    [(cons (and (cons 'define _) d) s)
     (match (parse-raw s)
       [(RawProg ds incls e)
        (RawProg (cons (parse-define d) ds) incls e)])]
    [(cons e '()) (RawProg '() '() (parse-e e))]
    [_ (error "program parse error")]))

;; String [Listof S-Expr] [Listof Include] -> Lib
(define (parse-lib origin-file cur-includes s)
  (define (get-name i)
    (match i
      [(Include file-name name) name]))
  (match s
    [(cons (and (cons (or 'include 'as) _) i) s) 
     (match (parse-lib origin-file cur-includes s)
       [(Lib l-name l-ds l-depend)
        (match (parse-include i) 
               [(Include file-name name) 
                (let ((appended-file-name (string-append (get-file-dir origin-file) "/" file-name)))
                  (Lib l-name l-ds (if (in-list? equal? name (map get-name cur-includes))
                                      l-depend
                                      (cons (process-include (cons (Include appended-file-name name) cur-includes) appended-file-name) l-depend))))])])]
    [(cons (and (cons 'define _) d) s)
     (match (parse-lib origin-file cur-includes s)
       [(Lib name ds depend)
        (Lib name (cons (parse-define d) ds) depend)])]
    [_ (Lib "" '() '())])) ; How would a library get malformed? We should figure out data handling...


; S-Expr -> Include
(define (parse-include s)
  (match s
    [(list 'include f-name)
      (Include f-name (last (string-split (car (string-split f-name ".rkt")) "/")))]
    [(list 'as name s)
     (match (parse-include s)
      [(Include f-name _) (Include f-name name)])]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (cons (? symbol? f) xs) e)
     (if (all symbol? xs)
         (Defn f (parse-param-list xs e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; like andmap, but work on improper lists too
(define (all p? xs)
  (match xs
    ['() #t]
    [(cons x xs) (and (p? x) (all p? xs))]
    [x (p? x)]))

;; S-Expr S-Expr -> FunPlain or FunRest
(define (parse-param-list xs e)
  (match xs
    ['() (FunPlain '() (parse-e e))]
    [(cons x xs)
     (match (parse-param-list xs e)
       [(FunPlain xs e) (FunPlain (cons x xs) e)]
       [(FunRest xs y e) (FunRest (cons x xs) y e)])]
    [(? symbol? xs)
     (FunRest '() xs (parse-e e))]
    [_
     (error "parse parameter list error")]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? exact-integer?)            (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (alias (Prim0 p0))]
    [(list (? (op? op1) p1) e)     (alias (Prim1 p1 (parse-e e)))]
    [(list (? (op? op2) p2) e1 e2) (alias (Prim2 p2 (parse-e e1) (parse-e e2)))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (alias (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3)))]
    [(cons (? (op? opN) p) es)     (PrimN p (map parse-e es))]
    [(list 'begin es ...)          (Begin (map parse-e es))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list 'let  bs e)         (parse-let  bs e)]
    [(list 'let* bs e)         (parse-let* bs e)]
    [(cons 'cond cs)           (parse-cond cs)]
    [(cons 'case (cons ev cs)) (parse-case ev cs)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) (LamPlain xs (parse-e e)))
         (error "parse lambda error"))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]    
    [_ (error "Parse error" s)]))

;; S-Expr -> Cond
(define (parse-cond cs)
  (match cs
    [(list (list 'else e)) (Cond '() (parse-e e))]
    [(cons (list p e) css)
     (match (parse-cond css)
       [(Cond cs el)
        (Cond (cons (Clause (parse-e p) (parse-e e)) cs) el)])]
    [_ (error "parse error")]))

;; S-Expr S-Expr -> Case
(define (parse-case ev cs)
  (match cs
    [(list (list 'else e)) (Case (parse-e ev) '() (parse-e e))]
    [(cons (list ds e) css)
     (match (parse-case ev css)
       [(Case ev cs el)
        (Case ev (cons (Clause (parse-datums ds) (parse-e e)) cs) el)])]
    [_ (error "parse error")]))

;; S-Expr -> [Listof Datum]
(define (parse-datums ds)
  (match ds
    ['() '()]
    [(cons (? integer? i) ds)
     (cons i (parse-datums ds))]
    [(cons (? boolean? b) ds)
     (cons b (parse-datums ds))]
    [(cons (? char? c) ds)
     (cons c (parse-datums ds))]
    [(cons 'eof ds)
     (cons eof (parse-datums ds))]
    [_ (error "parse error")]))

;; S-Expr S-Expr -> Let
(define (parse-let bs e)
  (match bs
    ['() (Let '() '() (parse-e e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (parse-let bs e)
       [(Let xs es e)
        (Let (cons x1 xs) (cons (parse-e e1) es) e)])]
    [else (error "parse error")]))

;; S-Expr S-Expr -> Let*
(define (parse-let* bs e)
  (match bs
    ['() (Let* '() '() (parse-e e))]
    [(cons (list (? symbol? x1) e1) bs)
     (match (parse-let* bs e)
       [(Let* xs es e)
        (Let* (cons x1 xs) (cons (parse-e e1) es) e)])]
    [else (error "parse error")]))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? exact-integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (PAnd (parse-pat p1) (parse-pat p2))]))

(define (alias p)
  (match p
    [(Prim1 'println e) (Begin (cons (Prim1 'write e) (cons (Prim1 'write (Str "\n")) '())))]
    [_ p]))

(define op0
  '(read-byte peek-byte void throw-error))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         not - abs boolean? integer?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length 
         println
         close read write print listen accept on-message 
         closed?))

(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref 

         open read write open-sock on-message))

(define op3
  '(vector-set! string-assign))

(define opN 
  '(+))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

; [Listof Lib] -> [Listof Lib]
(define (remove-dupes lst)
  (match lst 
    ['() '()]
    [(cons l rst) 
     (cons l (remove-dupes (filter 
              (lambda (e) (not (libs-equal? l e))) 
              rst)))]))

; Lib | [Listof Lib] -> [Listof Lib]
(define (flatten-libs lst)
  (define (strip-defns depends)
    (match depends
      ['() '()]
      [(cons (Lib n ds d) rst) (Lib n '() '())]))
  (match lst 
    ['() '()]
    [(Lib name ds deps) (cons (Lib name ds (strip-defns deps)) (flatten-libs deps))]
    [(cons l rst) (append (flatten-libs l) (flatten-libs rst))]))


; [ListOf Include] [Listof Include] -> [Listof Lib]
(define (includes->libs cur-includes includes)
  (match includes
    ['() '()]
    [(cons (Include file name) rst) (cons-nodupes libs-equal?
                                      (match (process-include cur-includes file)
                                        [(Lib _ l-ds l-deps) (Lib name l-ds l-deps)]) 
                                      (includes->libs cur-includes rst))]))

; Lib Lib -> Bool
(define (libs-equal? lib1 lib2)
  (match lib1
    [(Lib name1 _ _)
     (match lib2
       [(Lib name2 _ _) (equal? name1 name2)])]))

;; [Listof Include] String -> Lib
(define (process-include cur-includes file-name) 
  (let ((f (open-input-file file-name)))
    (begin
      (read-line f)
      (let ((res (read-all-file f)))
        (begin 
          (close-input-port f)
          (match (parse-lib file-name cur-includes res)
            [(Lib "" ds depend) (Lib (last (string-split (car (string-split file-name ".rkt")) "/")) ds depend)]
            [x x]))))))
