#lang racket
(include "lists.rkt")

; Converts string to a char list (probably for use in lib/lists.rkt)
; String -> [Listof Char]
(define (str->char-list str) (str->char-list-rec str 0))
(define (str->char-list-rec str n) (if (= (string-length str) n)
                                '()
                                (cons (string-ref str n) (str->char-list-rec str (add1 n)))))

; Turns a list of chars into a string list
; [Listof Char] -> String
(define (char-list->str lst)
    (char-list-helper lst (make-string (lists:length lst) #\h) 0)
)

; Helper function for above
; [Listof Char] String Integer -> String
(define (char-list-helper lst str index)
    (match lst
        ['() str]
        [(cons c cs) (let (( str (string-assign str index c))) (char-list-helper cs str (+ index 1))
                      )
        ]
    )
)

; Compares two strings and determines if they are equal
; String String -> Boolean
(define (str-eq? str1 str2) 
    (if (string? str1)
        (if (string? str2) 
            (if (= (string-length str1) (string-length str2))
                (str-eq-helper str1 str2 (sub1 (string-length str1)))
                #f)
            #f)
        #f))

; Helper function for above
; String String Integer -> Boolean
(define (str-eq-helper str1 str2 index)
    (match index
        [0 (if (= (char->integer (string-ref str1 index)) (char->integer (string-ref str2 index))) #t #f )]
        [_ (if (= (char->integer (string-ref str1 index)) (char->integer (string-ref str2 index))) (str-eq-helper str1 str2 (- index 1)) #f )]
    )
)

; Determines if character c is in string s
; String Char -> Boolean
(define (contains? str c)
    (if (string? str) 
        (if (char? c) 
            (contains-helper str c (sub1 (string-length str))) 
            (throw-error)
        )
        (throw-error) 
    )
)

; Helper function for above
; String Char Integer -> Boolean
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


; Get index of the first instance of char c in the string str
; String Char -> Integer
(define (get-index str c)
    (if (string? str) 
        (if (char? c) 
            (get-index-helper str c 0) 
            (throw-error)
        ) 
        (throw-error) 
    )
)

; Helper function for above. If the character is not in the string returns -1
; String Char Integer -> Integer
(define (get-index-helper str c index)
    (if (= index (string-length str))
            -1
            (if (= (char->integer (string-ref str index)) (char->integer c))
                index
                (get-index-helper str c (add1 index))
            )
    )
)

; Makes copy of string str
; String -> String
(define (string-copy str)
    (let ((strcopy (make-string (string-length str) #\h)))
        (string-copy-helper str strcopy 0)))

; Helper function for above
; String String Integer -> String
(define (string-copy-helper str strcopy index)
    (if (= index (string-length str))
        strcopy
        (string-copy-helper str (string-assign strcopy index (string-ref str index)) (add1 index))
    )
)


; Creates substring of string str starting from index
; String Integer -> String
(define (substring str index)
    (if (string? str)
        (let ((strcopy (make-string (- (string-length str) index) #\h)))
        
            (substring-helper str strcopy 0 0 index)
        
        )   
        (throw-error)
    )
)

; Helper function for above
; Str Str Integer Integer Integer
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

; Returns string pointing to index of first occurence of character char in str
; Str Char -> String
(define (strchr str char)
    (if (= (get-index str char) -1)
        (make-string 0 #\h)
        (substring str (get-index str char))
    )
)

;; Removes the first character of a string
;; String -> String
(define (remove-first-char s)
  (char-list->str (cdr (str->char-list s))))

;; Gets the first n chars of a string
;; Integer String -> String
(define (get-first-chars n s)
  (char-list->str (lists:take-only (str->char-list s) n)))

;; String String -> String
(define (str-append s1 s2)
  (char-list->str (lists:append (str->char-list s1) (str->char-list s2))))
