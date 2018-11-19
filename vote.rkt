#lang racket

(require
  "model.rkt"
  web-server/servlet
  web-server/page)

(provide (all-defined-out))

(define (votelinks item)
      `(span ((class "votelinks"))
             (a ((href "up")) (div "▲"))
             (a ((href "down")) (div "▼"))))

