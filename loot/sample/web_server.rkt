#lang spite
(include "lib/http.rkt")

(begin (println "Starting web server on port 8080")
       (let ([serv (listen 8080)])
         (http:http-serv-file serv "index.html")))
