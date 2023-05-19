#lang spite
(include "lib/strings.rkt")
(include "lib/lists.rkt")
(include "lib/util.rkt")
(define (echo sock n)
  (let ((user-in (read n)))
    (if (util:or (< (string-length user-in) 2) (util:equal? (lists:take-only (strings:str->char-list user-in) 2) (lists:list #\: #\q)))
      (println "Done!") 
      (begin (println user-in) (write sock user-in) (read sock n) (echo sock n)))))
(let ([sock (open-sock "127.0.0.1" 8080)]) 
   (begin (println "Type :q to quit") (echo sock 15) (close sock)))