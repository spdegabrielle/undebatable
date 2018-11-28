#lang racket

(require
  "model.rkt"
  "login.rkt"
  web-server/servlet
  web-server/page)

(provide (all-defined-out))

; use different arrow chars for enabled/disabled/applied

(define (votelink item (direction "up") (user null) (then null))
  `(div (form ((method "post") (action "/vote"))
     (input ((type "hidden") (name "item") (value ,(~a item))))
     (input ((type "hidden") (name "direction") (value ,(~a direction))))
     (input ((type "hidden") (name "then") (value ,(~a then))))
     (input (,(if (not (votable? user item)) '(disabled "disabled") '(foo "bar"))
             (class "arrow") (type "submit")
             (value ,(~a (if (equal? direction "up") "▲" "▼"))))))))

(define (votable? user item)
  (and (existing-user? user)
       (empty? (voted user item))))

(define (votelinks item (user null) (then null))
      `(span ((class "votelinks"))
             ,(votelink item "up" user then)
             (div ,(~a (score item)))
             ,(votelink item "down" user then) ""))

(define/page (vote/page)
             ; TODO just redirect to whence
  (match (request-bindings/raw (current-request))
    ((list-no-order (binding:form #"item" item)
                    (binding:form #"direction" direction)
                    (binding:form #"then" then))
     (begin (~a item)
            (create-vote! (get-user (current-request))
                          (string->number (~a item))
                          (~a direction))
            (redirect-to (~a then))))))
