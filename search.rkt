#lang racket

(require
  "model.rkt"
  "post.rkt"
  web-server/page
  web-server/servlet
  )

(provide (all-defined-out))

(define/page (search/page (page 1))
  (match (request-bindings/raw (current-request))
    ((list (binding:form #"query" query))
     (curry listpage (current-request)
     ; TODO : find some way of passing the search query to the pagination function
                     #:label "search"
                     #:here (~a "search/" page "?query=" query)
                     #:items-fn (λ (a b) (search query))
                     #:render-fn render-item/single
                     #:page page))))

