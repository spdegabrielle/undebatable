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

(define (serve)
  (serve/servlet app-dispatch
    #:servlet-regexp #rx""
    #:port 8080
    #:launch-browser? #f
    #:listen-ip #f
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
    (("upload")
     upload/page)
    (("uploads")
     uploads/page)
    (("uploads" (integer-arg) (string-arg))
     ; the string arg is just there to make things look pretty
     download/page)
    ))
