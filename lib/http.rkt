#lang sample
(include "strings.rkt")
(include "lists.rkt")
(include "util.rkt")

;; Gets the method from a HTTP request string
(define (get-method s)
  (strings:get-first-chars (strings:get-index s #\space) s))

;; Determines if the HTTP request is a GET request
; String -> Bool
(define (is-get? s) 
  (util:and (util:equal? (string-ref s 0) #\G)
            (util:equal? (string-ref s 1) #\E)
            (util:equal? (string-ref s 2) #\T)))

;; Determines if the HTTP request is directing to /
; String -> Bool
(define (is-index? s) 
  (util:or
    (util:and (util:equal? (string-ref s 4) #\/)
              (util:equal? (string-ref s 5)  #\i)
              (util:equal? (string-ref s 6)  #\n)
              (util:equal? (string-ref s 7)  #\d)
              (util:equal? (string-ref s 8)  #\e)
              (util:equal? (string-ref s 9)  #\x)
              (util:equal? (string-ref s 10)  #\.)
              (util:equal? (string-ref s 11)  #\h)
              (util:equal? (string-ref s 12)  #\t)
              (util:equal? (string-ref s 13)  #\m)
              (util:equal? (string-ref s 14)  #\l))
    (util:and (util:equal? (string-ref s 4) #\/)
              (util:equal? (string-ref s 5) #\space))))

;; Gets the path from a HTTP request string
(define (get-path s)
  (let ([rest (strings:remove-first-char (strings:strchr s #\space))])
    (strings:get-first-chars (strings:get-index rest #\space) rest)))

(define (send-file-resp peer file)
  (let ([f (open file #\r)] [status-line "HTTP/1.0 200 OK\r\n\r\n"])
    (begin (write peer (strings:str-append status-line (read f 4096)))
           (close f))))

;; Respond to the peer if they request /
(define (http-resp peer file)
  (lambda (msg)
    (begin (println "Got request!")
           (if (util:and (is-get? msg) (is-index? msg))
               (send-file-resp peer file)
               (begin (println "Bad request, responding with 404")
                      (write peer "HTTP/1.0 404 NOTFOUND\r\n")))
           (close peer))))

(define (http-serv-file sock file)
  (let ([peer (accept sock)])
    (begin (on-message peer (http-resp peer file))
           (http-serv-file sock file))))
