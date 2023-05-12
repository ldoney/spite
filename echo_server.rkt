#lang spite

(define (echo serv)
  (let ([peer (accept serv)])
    (begin (write "Accepted peer\n")
           (on-message peer (lambda (msg) (write peer msg)))
           (write "Peer disconnected\n")
           (echo serv))))

(let ([serv (listen 8080)])
   (begin (write "Listening on port 8080\n")
          (echo serv)))
