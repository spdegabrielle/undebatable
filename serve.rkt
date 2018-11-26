#lang racket

(require
  "login.rkt"
  "post.rkt"
  "upload.rkt"
  "user.rkt"
  "vote.rkt"
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch)

(provide (all-defined-out))

(define (not-found req)
  (response/xexpr
    `(html (body (p
       ((style "text-align: center; font-family: sans; font-size: 140pt; font-weight: bold;"))
       "404")))
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
    (("newest")
     newest/page)
    (("comments")
     comments/page)
    (("login")
     login/page)
    (("logout") ; should be post
     ; HTTP GET can't change state!
     logout/page)
    (("forgot")
     forgot-password/page)
    (("user" (string-arg))
     user/page)
    (("submit")
     submit/page)
    (("item" (integer-arg))
     item/page)
    (("vote") #:method "post"
     vote/page)
    (("reply" (integer-arg))
     reply/page)
    (("uploads")
     uploads/page)
    (("uploads" (integer-arg) (string-arg))
     ; the string arg is just there to make things look pretty
     download/page)
    ))
