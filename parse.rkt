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

;; [Listof S-Expr] [Listof Include] -> Lib
(define (parse-lib cur-includes s)
  (define (get-name i)
    (match i
      [(Include file-name name) name]))
  (match s
    [(cons (and (cons (or 'include 'as) _) i) s) 
     (match (parse-lib cur-includes s)
       [(Lib l-name l-ds l-depend)
        (match (parse-include i) 
               [(Include file-name name) 
                (Lib l-name l-ds (if (in-list? equal? name (map get-name cur-includes))
                                     l-depend
                                     (cons (process-include (cons (Include file-name name) cur-includes) file-name) l-depend)))])])]
    [(cons (and (cons 'define _) d) s)
     (match (parse-lib cur-includes s)
       [(Lib name ds depend)
        (Lib name (cons (parse-define d) ds) depend)])]
    [_ (Lib "" '() '())])) ; How would a library get malformed? We should figure out data handling...


; S-Expr -> Include
(define (parse-include s)
  (match s
    [(list 'include f-name)
      (Include f-name (car (string-split f-name ".rkt")))]
    [(list 'as name s)
     (match (parse-include s)
      [(Include f-name _) (Include f-name name)])]))

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
    [(list 'begin es ...)          (Begin (map parse-e es))]
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

; [Listof Lib] -> [Listof Lib]
(define (remove-dupes lst)
  (match lst 
    ['() '()]
    [(cons l rst) 
     (cons l (remove-dupes (filter 
              (lambda (e) (not (libs-equal l e))) 
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
    [(cons (Include file name) rst) (cons-nodupes libs-equal 
                                      (match (process-include cur-includes file)
                                        [(Lib _ l-ds l-deps) (Lib name l-ds l-deps)]) 
                                      (includes->libs cur-includes rst))]))

; Lib Lib -> Bool
(define (libs-equal lib1 lib2)
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
          (match (parse-lib cur-includes res)
            [(Lib "" ds depend) (Lib (last (string-split (car (string-split file-name ".rkt")) "/")) ds depend)]
            [x x]))))))
