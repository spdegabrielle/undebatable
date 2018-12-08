#lang racket

(require "model.rkt")

(provide render-page)

; #:preamble #"<!DOCTYPE html>"

(define menu-items
  '("top" "newest" "comments" "submit" "uploads"))
;  '("top" "newest" "comments" "submit" "uploads" "feeds" "market" "messages" "search"))

(define (menu items (user null))
  `(div ((class "menu"))
        (span
;          (a ((href "/")) (img ((src "/favicon.ico") (class "logo"))))
          (span (a ((href "/") (class "sitename")) ,(sitename)) " ")
          (span ,@(map (λ (i) `(span (a ((href ,(string-append "/" i))) ,i) " ")) items)))
        ,(if (not user)
             `(span (a ((href "/login")) "login") " ")
             `(span (span (a ((href "/user")) ,(~a user " (" (karma user) ")")) " ")
                    (span (a ((href "/logout")) "logout") " ")))))

(define (render-page user title #:message (message "") . content)
  `(html
     (head
       (meta ((charset "utf-8")))
       (meta ((http-equiv "Content-Security-Policy") (content "script-src 'none';")))
       (link ((rel "stylesheet") (href "/minimalist.css")))
       )
     (body
       (title ,title)
       ,(menu menu-items user)
       (div ((class "message")) ,message)
       ,@content
;       (div ,search-bar)
       (div ((class "footer")) (a ((href "https://notabug.org/hjek/debate"))"AGPLv3+ © 2018 Pelle Hjek"))
     )))
