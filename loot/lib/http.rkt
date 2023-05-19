#lang sample
(include "strings.rkt")
(include "strings-list.rkt")
(include "lists.rkt")
(include "util.rkt")

;; Gets the method from a HTTP request string
(define (get-method sl)
  (strings-list:get-first-chars (strings-list:get-index sl #\space) sl))

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
              (util:equal? (string-ref s 5) #\space))))

;; Gets the path from a HTTP request string
(define (get-path sl)
  (let ([rest (strings-list:remove-first-char (strings-list:str-char sl #\space))])
    (strings-list:get-first-chars (strings-list:get-index rest #\space) rest)))

(define (send-file-resp peer file)
  (let ([f (open file #\r)] [status-line "HTTP/1.0 200 OK\r\n\r\n"])
    (begin (write peer (strings:str-append status-line (read f 4096)))
           (close f))))

;; Respond to the peer if they request /
(define (http-resp peer file get-strl path-strl)
  (lambda (msg)
    ;; We do the conversion only once to be memory efficient
    (let ([msg-list (strings:str->char-list msg)])
      (let ([method (get-method msg-list)]
            [path (get-path msg-list)])
      (begin (println (strings:char-list->str
                      (strings-list:append method
                                           (strings-list:append (cons #\space '())
                                                                path))))
             (if (util:and (strings-list:str-eq? method get-strl)
                           (strings-list:str-eq? path path-strl))
               (send-file-resp peer file)
               (write peer "HTTP/1.0 404 NOTFOUND\r\n"))
             (close peer))))))

(define (http-serv-file-rec sock file get-strl path-strl)
  (let ([peer (accept sock)])
    (begin (on-message peer (http-resp peer file get-strl path-strl))
           (http-serv-file-rec sock file get-strl path-strl))))

(define (http-serv-file sock file)
  ;; Define these here for memory efficiency
  (let ([get-strl (strings:str->char-list "GET")]
        [path-strl (strings:str->char-list "/")])
    (http-serv-file-rec sock file get-strl path-strl)))

      
