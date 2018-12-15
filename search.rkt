#lang racket

(provide (all-defined-out))

(define (search-bar)
  `(input ((class       "search")
           (type        "search")
           (placeholder "search"))))
