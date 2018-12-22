#lang racket

(require
  "login.rkt"
  "post.rkt"
  "upload.rkt"
  "user.rkt"
  "vote.rkt"
  "bazar.rkt"
  "search.rkt"
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch)

(provide (all-defined-out))

(define (not-found req)
  (response/xexpr
    `(html (body
      (title "Not found")
      (div ((style "text-align: center; font-family: sans; font-size: 100pt; font-weight: bold;"))
         (div "404")
         (div (img ((style "width:2em") (class "fail-pet") (src "/noisebob.png")))))))
     #:code 404))

(define (serve)
  (serve/servlet app-dispatch
    #:servlet-regexp #rx""
    #:port 8080
    #:launch-browser? #f
    #:listen-ip #f
    #:file-not-found-responder not-found
;    #:log-file ".log"
    #:server-root-path (current-directory)))

(define-values (app-dispatch app-url)
  (dispatch-rules

    (("")
     top/page)
    (("top")
     top/page)
    (("top" (integer-arg))
     top/page)

    (("newest")
     newest/page)
    (("newest" (integer-arg))
     newest/page)

    (("comments")
     comments/page)
    (("comments" (integer-arg))
     comments/page)

    (("login")
     login/page)
    (("logout") #:method "post"
     logout/page)
    (("forgot")
     forgot-password/page)
    (("user" (string-arg))
     user/page)

    (("submit")
     submit/page)
    (("reply" (integer-arg))
     submit/page)
    (("edit" (integer-arg))
     edit/page)

    (("item" (integer-arg))
     item/page)

    (("vote") #:method "post"
     vote/page)

    (("search")
     search/page)
    (("search" (integer-arg))
     search/page)

    (("uploads")
     uploads/page)
    (("uploads" (integer-arg) (string-arg))
     ; the string arg is just there to make things look pretty
     download/page)

    (("bazar")
     bazar/page)
    ))
