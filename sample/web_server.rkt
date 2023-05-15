#lang spite

(include "lib/strings.rkt")
(include "lib/lists.rkt")
(include "lib/util.rkt")

;; Removes the first character of a string
(define (remove-first-char s)
  (strings:char-list->str (cdr (strings:str->char-list s))))

;; Gets the first n chars of a string
(define (get-first-chars n s)
  (strings:char-list->str (lists:take-only (strings:str->char-list s) n)))

(define (str-append s1 s2)
  (strings:char-list->str (lists:append-binary (strings:str->char-list s1) (strings:str->char-list s2))))

;; Gets the method from a HTTP request string
(define (get-method s)
  (get-first-chars (strings:get-index s #\space) s))

;; Gets the path from a HTTP request string
(define (get-path s)
  (let ([rest (remove-first-char (strings:strchr s #\space))])
    (get-first-chars (strings:get-index rest #\space) rest)))

(define (send-file-resp peer file)
  (let* ([f (open file #\r)]
         [file-contents (read f 4096)]
         [status-line "HTTP/1.0 200 OK\r\n\r\n"])
    (begin (write peer (str-append status-line file-contents))
           (close f))))

;; Respond to the peer if they request /
(define (http-resp peer file)
  (lambda (msg)
    (begin (println (str-append (get-method msg)
                                (str-append " Path: "
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

(begin (println "Starting web server on port 8080")
       (let ([serv (listen 8080)])
         (http-serv-file serv "index.html")))
