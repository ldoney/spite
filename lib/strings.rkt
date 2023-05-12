#lang racket
; TODO Implement assembly-level string ops... right now strings are too rudimentary
; to implement most of these, hence str->char-list, but we should remove that because
; we can't implement slice or tail-end with it. So, right now, neither of those 
; functions work.


(define (str->char-list str) (str->char-list-rec str 0))
(define (str->char-list-rec str n) (if (= (string-length str) n)
                                '()
                                (cons (string-ref str n) (str->char-list-rec str (add1 n)))))

;works!
(define (list-length lst)
    (match lst
        ['() 0]
        [(cons c cs) (+ 1 (list-length cs))]
    )
)

;do we want to be able to compare a string and something else or not?
(define (str-eq? str1 str2) 
    (if (string? str1)
        (if (string? str2) 
            (if (= (string-length str1) (string-length str2))
                (str-eq-helper str1 str2 (sub1 (string-length str1)))
                #f)
            #f)
        #f))

(define (str-eq-helper str1 str2 index)
    (match index
        [0 (if (= (char->integer (string-ref str1 index)) (char->integer (string-ref str2 index))) #t #f )]
        [_ (if (= (char->integer (string-ref str1 index)) (char->integer (string-ref str2 index))) (str-eq-helper str1 str2 (- index 1)) #f )]
    )
)


(define (contains? str c)
    (if (string? str) 
        (if (char? c) 
            (contains-helper str c (sub1 (string-length str))) 
            (throw-error)
        )
        (throw-error) 
    )
)

(define (contains-helper str c index)
    (match index
        [0 (if (= (char->integer (string-ref str index)) (char->integer c)) 
                #t 
                #f
            )
        ]
        [_ (if (= (char->integer (string-ref str index)) (char->integer c)) 
                #t 
                (contains-helper str c (- index 1))
            )]
    )
)


;get index of the first char in the string
(define (get-index str c)
    (if (string? str) 
        (if (char? c) 
            (get-index-helper str c 0) 
            (throw-error)
        ) 
        (throw-error) 
    )
)

;if the character is not in the string returns -1
(define (get-index-helper str c index)
    (if (= index (string-length str))
            -1
            (if (= (char->integer (string-ref str index)) (char->integer c))
                index
                (get-index-helper str c (add1 index))
            )
    )
)

(define (string-copy str)
    (let ((strcopy (make-string (string-length str) #\h)))
        (string-copy-helper str strcopy 0)))

(define (string-copy-helper str strcopy index)
    (if (= index (string-length str))
        strcopy
        (string-copy-helper str (string-assign strcopy index (string-ref str index)) (add1 index))
    )
)


(define (substring str index)
    (if (string? str)
        (let ((strcopy (make-string (- (string-length str) index) #\h)))
        
            (substring-helper str strcopy 0 0 index)
        
        )   
        (throw-error)
    )
)

(define (substring-helper str strcopy index1 index2 targetIndex)

    (if (< index1 targetIndex)
        (substring-helper str strcopy (add1 index1) index2 targetIndex)
        (if (= index1 targetIndex)
            (substring-helper str (string-assign strcopy index2 (string-ref str index1)) (add1 index1) (add1 index2) targetIndex)
            (if (< index1 (string-length str))
                (substring-helper str (string-assign strcopy index2 (string-ref str index1)) (add1 index1) (add1 index2) targetIndex)
                strcopy
            )
        )
    )
)


(define (str-char str char)
    (if (= (get-index str char) -1)
        (make-string 0 #\h)
        (substring str (get-index str char))
    )
)



(define (char-list->str lst)
    (char-list-helper lst (make-string (list-length lst) #\h) 0)
)

(define (char-list-helper lst str index)
    (match lst
        ['() str]
        [(cons c cs) (let (( str (string-assign str index c))) (char-list-helper cs str (+ index 1))
                      )
        ]
    )
)
