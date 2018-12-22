#lang racket

(require
  "model.rkt"
  "login.rkt"
  web-server/servlet
  web-server/page)

(provide (all-defined-out))

; use different arrow chars for enabled/disabled/applied

(define (votelink item (direction 1) (user null) (then #f))
  `(div
     (form ((method "post") (action "/vote"))
       (input ((type "hidden") (name "item") (value ,(~a item))))
       (input ((type "hidden") (name "direction") (value ,(~a direction))))
       (input ((type "hidden") (name "then") (value ,(~a then))))
       (input ((class "arrow")
               (type  "submit")
               ,@(match
                   (list direction
                         (voted user item)
                         (votable? user item direction))
                   ((list 1  _  #t) `((value "△")))
                   ((list 1  1   _) `((value "▲")))
                   ((list 1  _  #f) `((value "△") (disabled "disabled")))
                   ((list -1 _  #t) `((value "▽")))
                   ((list -1 -1  _) `((value "▼")))
                   ((list -1 _  #f) `((value "▽") (disabled "disabled")))))))))

(define (votelinks item (user null) (then null))
      `(span ((class "votelinks"))
             ,(votelink item 1 user then)
             (div ,(~a (score item)))
             ,(votelink item -1 user then) ""))

(define/page (vote/page)
  (match (request-bindings/raw (current-request))
    ((list-no-order (binding:form #"item" item)
                    (binding:form #"direction" direction)
                    (binding:form #"then" then))
     (let* ((user      (get-user (current-request)))
            (item      (string->number (~a item)))
            (direction (string->number (~a direction))))
           (if (not (eq? (voted user item) direction))
              (begin
                (delete-vote! user
                              item)
                (create-vote! user
                              item
                              direction))
              (delete-vote! user
                            item))
           (redirect-to (~a then "#" item))))))
