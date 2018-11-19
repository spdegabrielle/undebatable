#lang racket

(require "model.rkt")

(provide render-page)

; #:preamble #"<!DOCTYPE html>"

(define menu-items
  '("newest" "submit" "upload" "uploads"))

(define (menu items (user null))
  `(div ((class "menu"))
;        (a ((href "/")) (img ((src "publish.png") (class "logo"))))
        (span
          (span (a ((href "/") (class "title")) ,(sitename)) " ")
          (span ,@(map (λ (i) `(span (a ((href ,(string-append "/" i))) ,i) " ")) items)))
        ,(if (not user)
             `(span (a ((href "/login")) "login") " ")
             `(span (span (a ((href "/user")) ,user) " ") (span (a ((href "/logout")) "logout") " ")))))

(define (render-page user title #:message (message "") . content)
  `(html
     (head
       (meta ((charset "utf-8")))
       ;(meta ((http-equiv "Content-Security-Policy") (content "script-src 'none';")))
       (link ((rel "stylesheet") (href "/minimalist.css")))
       )
     (body
       (title ,title)
       ,(menu menu-items user)
       (div ((class "message")) ,message)
       ,@content
;       (div ,search-bar)
     )))
 
