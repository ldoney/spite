#lang racket 
(include "lib/http.rkt")
(include "lib/strings.rkt")

(begin (println "Starting web server on port 8080")
       (let ([serv (listen 8080)])
         (http:http-serv-file serv "index.html")))
