#lang racket

(require
  "model.rkt"
  "page.rkt"
  "login.rkt"; for get-user. should be in model, perhaps?
  web-server/servlet
  web-server/page
  (only-in markdown parse-markdown)
  xml)

(provide user/page)

(define/page (user/page subject)
  (response/xexpr
    (render-page
      (get-user (current-request))
      (~a subject)
      `(div ,(~a subject)))))

