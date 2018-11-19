#lang racket

(require
  "login.rkt"
  "post.rkt"
  "upload.rkt"
  "user.rkt"
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
     newest/page)
    (("newest")
     newest/page)
    (("login")
     login/page)
    (("logout")
     logout/page)
    (("forgot")
     forgot-password/page)
    (("user" (string-arg))
     user/page)
    (("submit")
     submit/page)
    (("item" (integer-arg))
     item/page)
    (("reply" (integer-arg))
     reply/page)
    (("upload")
     upload/page)
    (("uploads")
     uploads/page)
    ))
