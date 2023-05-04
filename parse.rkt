#lang racket
(provide parse parse-define parse-e)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match s ; TODO Figure out a better way to write include and include qualifiers... a lot of duplicated code 
    [(cons (list 'include i) s) 
      (match (parse-include i)
        [(Lib name ds depend)
         (match (parse s)
           [(Prog p-ds libs e)
            (Prog p-ds (append-nodupes (cons-nodupes (Lib name ds (strip-defns depend)) depend) libs) e)])])]
    [(cons (list 'as name (list 'include i)) s) 
      (match (parse-include i)
        [(Lib _ ds depend)
         (match (parse s)
           [(Prog p-ds libs e)
            (Prog p-ds (append-nodupes (cons-nodupes (Lib name ds (strip-defns depend)) depend) libs) e)])])]
    [(cons (and (cons 'define _) d) s)
     (match (parse s)
       [(Prog ds libs e)
        (Prog (cons (parse-define d) ds) libs e)])]
    [(cons e '()) (Prog '() '() (parse-e e))]
    [_ (error "program parse error")]))

; Lib (ListOf Lib) -> (ListOf Lib)
(define (cons-nodupes lib lst2)
  (if (in-liblist lib lst2) lst2 (cons lib lst2)))

; (ListOf Lib) (ListOf Lib) -> (ListOf Lib)
(define (append-nodupes lst1 lst2)
  (match lst2
    ['() lst1]
    [(cons lib rst2) (if (in-liblist lib lst2) (append-nodupes lst1 rst2) (cons lib (append-nodupes lst1 rst2)))]))

(define (in-liblist lib lst)
  (match lib
    [(Lib needle-name needle-ds needle-depend)
      (match lst
        ['() #f]
        [(cons (Lib name ds depend) rst) (if (equal? name needle-name) #t (in-liblist lib lst))])]))

; Lib -> Lib
(define (strip-defns depends)
  (match depends
    ['() '()]
    [(cons (Lib n ds d) rst) (Lib n '() '())]))

;; String -> Lib
(define (parse-include file-name) 
  (let ((f (open-input-file file-name)))
    (begin
      (read-line f)
      (let ((res (read-all-file f)))
        (begin 
          (close-input-port f)
          (match (parse-lib res)
            [(Lib "" ds depend) (Lib (car (string-split file-name ".rkt")) ds depend)]
            [x x]))))))

;; String [Listof S-Expr] -> Lib
(define (parse-lib s)
  (match s
    [(cons (list 'include i) s)  ;TODO Make includes be able to also include.. right now it doesn't work
      (match (parse-lib s)
        [(Lib name ds depend)
         (Lib name ds (cons (parse-include i) depend))])]
    [(cons (and (cons 'define _) d) s)
     (match (parse-lib s)
       [(Lib name ds depend)
        (Lib name (cons (parse-define d) ds) depend)])]
    [_ (Lib "" '() '())])) ; How would a library get malformed? We should figure out data handling...


; So supposedly racket is actually really good
; at reading LISP-formatted stuff. If it's properly parenthesized, 
; racket takes it all out automatically which is pretty cool
(define (read-all-file f)
  (let ((e (read f))) 
    (if (eof-object? e) 
      '()
      (cons e (read-all-file f)))))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

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
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) xs (parse-e e))
         (error "parse lambda error"))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]    
    [_ (error "Parse error" s)]))

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

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
