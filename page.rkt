#lang racket

(require "model.rkt")

(provide newspage)

; #:preamble #"<!DOCTYPE html>"


(define menu-items
  '("top" "newest" "comments" "submit" "uploads"))
;  '("top" "newest" "comments" "submit" "uploads" "feeds" "bazar" "messages" "search"))


(define (search-bar)
  `(form ((class "plain")
;          (method "post")
          (action "/search"))
   (input ((name        "query")
           (type        "search")
           (placeholder "search")))))

(define (menu items (user null))
  `(div ((class "menu"))
        (span
          (span (a ((href "/") (class "logo") (title ,(sitename))) ,(substring (sitename) 0 1)) " ")
          (span ,@(map (λ (i) `(span (a ((href ,(string-append "/" i))) ,i) " ")) items))
          ,(search-bar))
        ,(if (not user)
             `(span (a ((href "/login")) "login") " ")
             `(span (span (a ((href "/user")) ,(~a user)) " ")
                    (form ((class  "plain")
                           (method "post")
                           (action "/logout"))
                          (input ((type        "hidden")
                                  (name        "auth")
                                  (value       ,(user->auth user))))
                          (input ((type  "submit")
                                  (value "logout"))))))))


(define (newspage user title #:message (message "") . content)
  `(html
     (head
       (meta ((charset "utf-8")))
       (meta ((http-equiv "Content-Security-Policy") (content "script-src 'none';")))
       (link ((rel "stylesheet") (href "/minimalist.css"))))
       
     (body
       (title ,title)
       ,(menu menu-items user)
       (div ((class "message")) ,message)
       ,@content
       (div ((class "footer")) (a ((href "https://notabug.org/hjek/debate"))"AGPLv3+ © 2018 Pelle Hjek")))))
     
