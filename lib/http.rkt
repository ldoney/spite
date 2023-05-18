#lang sample
(include "strings.rkt")
(include "lists.rkt")
(include "util.rkt")

;; Gets the method from a HTTP request string
(define (get-method s)
  (strings:get-first-chars (strings:get-index s #\space) s))

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
    (begin (println (strings:str-append (get-method msg)
                                (strings:str-append " Path: "
                                            (get-path msg))))
           (if (util:and (strings:str-eq? (get-method msg) "GET")
                         (strings:str-eq? (get-path msg) "/"))
               (send-file-resp peer file)
               (write peer "HTTP/1.0 404 NOTFOUND\r\n"))
           (close peer))))

(define (http-serv-file sock file)
  (let ([peer (accept sock)])
    (begin (on-message peer (http-resp peer file))
           (http-serv-file sock file))))
