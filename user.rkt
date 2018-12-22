#lang racket

(require
  "model.rkt"
  "page.rkt"
  "login.rkt"; for get-user. should be in model, perhaps?
  web-server/servlet
  web-server/page
  xml)

(provide user/page)

(define/page (user/page subject)
  (response/xexpr
    (newspage
      (get-user (current-request))
      (~a subject)
      `(div ,(~a subject)))))

