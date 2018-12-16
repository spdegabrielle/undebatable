#lang racket

(require
  "model.rkt"
  "page.rkt"
  "login.rkt"
  "vote.rkt"
  "markdown.rkt"
  web-server/servlet
  web-server/page
  xml)

(provide submit/page newest/page item/page edit/page top/page comments/page)

(define/page (process-item)
  (let ((user (get-user (current-request))))
       (redirect-to (~a "/item/"
         (match (request-bindings/raw (current-request))
           ((list-no-order (binding:form #"title" title)
                           (binding:form #"text"  text)
                           (binding:form #"url"   url)
                           (binding:form #"item"  item))
            (if (string=? (~a item) "")
                (create-item! (~a user) #f (~a title) (~a text) (~a url) #f)
                (edit-item! item (~a user) #f (~a title) (~a text) (~a url) #f)))
           ((list-no-order (binding:form #"parent" parent)
                           (binding:form #"text"   text)
                           (binding:form #"item"   item))
            (if (string=? (~a item) "")
                (create-item! (~a user) (~a parent) #f (~a text) #f #f)
                (edit-item! item (~a user) (~a parent) #f (~a text) #f #f))))))))

(define/page (edit/page item)
  ; TODO: perhaps it should be possible to change the 'parent' of an item
  (submit/page (current-request) (parent item) item (title item) (url/item item) (text item)))

; TODO: compine the submit/reply
(define/page (submit/page (parent #f) (item #f) (title #f) (url #f) (text #f))
  (let ((user (get-user (current-request))))
    (if (not user)
        (redirect-to "/login")
        (send/suspend/dispatch
          (λ (embed-url)
             (response/xexpr
               (render-page
                 user
                 (if parent "Reply" "New item")
                 ; TODO: add cookie value to form and check that it matches the cookie on server side
                 ; to protect against CSRF
;                         (input ((type "hidden")
;                                 (name "auth")))
                ; TODO: show item being edited
                 (if (and parent (not text))
                     `(ul ((class "items"))
                        ,(render-item/single user (~a "/reply/" parent) parent)) "")
                 `(form ((action ,(embed-url process-item))
                         (method "post"))
                         (div (input    ((type        "hidden")
                                         (name        "item")
                                         (value       ,(if item (~a item) "")))))
                         ,(if parent
                             `(div (input    ((type        "hidden")
                                              (name        "parent")
                                              (value       ,(~a parent)))))
                             `(span (div (input    ((class       "field")
                                                    (type        "text")
                                                    (placeholder "title")
                                                    (required    "required")
                                                    (name        "title")
                                                    (value       ,(if title title "")))))
                                    (div (input    ((class       "field")
                                                    (placeholder "url")
                                                    (type        "url")
                                                    (name        "url")
                                                    (value       ,(if url url "")))))))
                         (div (textarea ((class       "field")
                                         (placeholder "text")
                                         (rows        "5")
                                         (name        "text"))
                                         ,(if text text "")))
                         ;     ,markdown-doc)
                         (div (input    ((class       "button")
                                         (type        "submit")
                                         (value       ,(if parent "Reply" "Submit")))))))))))))


(define (plural quantity noun)
  (if (= quantity 1)
      (~a quantity " " noun)
      (~a quantity " " noun "s")))

(define (author-link item)
  `(span " by " (a ((href ,(~a "/user/" (author item)))) ,(~a (author item) " "))))

(define (created-link user item)
  `(span
     (a ((href ,(~a "/item/" item))
         (class ,(if (seen? user item)
                     "seen"
                     (begin (seen! user item)
                            "unseen"))))
        ,(~a " " (age (created item)) " "))))

(define (comments-link item)
  (if (eq? item (root item))
    `(span (a ((href ,(~a "/item/" item)))
       ,(if (> (length (descendants item)) 0)
           (~a (plural (length (descendants item)) " comment") " ")
           " discuss ")))
    ""))

(define (parent-link item)
  (if (parent item)
    `(span (a ((href ,(~a "/item/" (parent item)))) " parent "))
    ""))

(define (root-link item)
  (if (not (= item (root item)))
    `(span (a ((href ,(~a "/item/" (root item)))) " on " ,(title (root item))))
    ""))

(define (reply-link item)
  `(span (a ((href ,(~a "/reply/" item))) " reply ")))

(define (edit-link user item)
  (if (equal? user (author item))
    `(span (a ((href ,(~a "/edit/" item))) " edit "))
    ""))

(define (unpublish-link user item)
  (if (equal? user (author item))
    `(span (a ((href ,(~a "/unpublish/" item))) " unpublish "))
     ""))

(define (favorite-link user item)
  ; TODO : has to be post if it's changing state
  `(span (a ((href ,(~a "/favorite/" item))) " favorite ")))

(define (itemline user item)
  `(span ((class "itemline"))
         ,(author-link item)
         ,(created-link user item)
         ,(comments-link item)
         ,(parent-link item)
         ,(root-link item)
         ,(edit-link user item)
;         ,(unpublish-link user item)
         ,(reply-link item)))

(define (age seconds)
  (let ((since (- (current-seconds) seconds)))
    (~a (cond ((> since (* 60 60 24)) (plural (floor (/ since (* 60 60 24))) "day"))
              ((> since (* 60 60))    (plural (floor (/ since (* 60 60)))    "hour"))
              (else                   (plural (floor (/ since (* 60)))       "minute")))
        " ago")))

(define (valid-url? url)
  ; TODO: obivously improve this
  (and url
       (not (string=? url ""))))

(define (domain url)
  (cond ((regexp-match #px"(?<=https?://).*?(?=/|&|$|:)" url)
         => first)
        (else "")))

(define (titleline item)
  (cond ((title item)
         => (λ (it)
               (if (valid-url? (url/item item))
                   `(div (a ((class "title") (href ,(url/item item))) ,it)
                         (span (a ((class "domain")) " (" ,(domain (url/item item)) ")")))
                   `(div (a ((class "title") (href ,(~a "/item/" item))) ,it)))))
        (else "")))

; simplify this:
; always display the title if it's there
(define (render-item user here item
                     #:text?     (text? #f)
                     #:tree?     (children? #f))
    `(li
       (div ((class "votable"))
           ,(votelinks item user here)
           (div ((class "item"))
             ,(titleline item)
             ,(itemline user item)
             ,(if text? `(p ,(markdown->xexpr (text item))) "")))
       (div ((class "children"))
            ,(if children? `(ul ((class "items"))
                                ,@(map (curry render-item/tree user here) (children item))) ""))))

(define render-item/single
  (curry render-item #:text? #t))

(define render-item/tree
  (curry render-item #:text? #t #:tree? #t))

(define (list-page label items-fn (page 1) (perpage 5))
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        label
        `(ul ((class "items"))
              ,@(map (curry render-item user "/")
                     (items-fn perpage (* page perpage)))
             (li ,(pagination label page))))))))

;(define newest/page
;  (curry list-page "newest" newest))

(define/page (newest/page (page 1) (perpage 5))
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Newest"
        `(ul ((class "items"))
              ,@(map (curry render-item user "/")
                     (newest perpage (* (- page 1) perpage)))
             (li ,(pagination "newest" page))))))))

(define/page (top/page (page 1) (perpage 5))
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Top"
        `(ul ((class "items"))
             ,@(map (curry render-item user "/")
                    (top perpage (* (- page 1) perpage)))
             (li ,(pagination "top" page))))))))

(define/page (comments/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Comments"
        `(ul ((class "items"))
             ,@(map (curry render-item/single user "/")
                    (comments))))))))

(define/page (item/page item)
  (let ((user (get-user (current-request))))
    (response/xexpr
      (render-page
        user
        "Item"
        `(ul ((class "items"))
             ,(render-item/tree user (~a "/item/" item) item))))))

(define (pagination here page)
  (when (< page 1) (set! page 1))
  `(div ,(if (> page 1)
             `(a ((href ,(~a "/" here "/" (- page 1)))) " < ")
             "")
        ,(~a " " page " ")
        (a ((href ,(~a "/" here "/" (+ page 1)))) " > ")))
