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

; TODO refactor render/item completely
; TODO display order is: TITLE, ITEMLINE, TEXT, (children)

(provide submit/page reply/page newest/page item/page edit/page top/page comments/page)

(define/page (process-item)
  (let ((user (get-user (current-request))))
       (redirect-to (~a "/item/"
         (match (request-bindings/raw (current-request))
           ((list-no-order (binding:form #"title" title)
                           (binding:form #"text"  text)
                           (binding:form #"url"   url))
            (create-item! (~a user) #f (~a title) (~a text) (~a url) #f))
           ((list-no-order (binding:form #"parent" parent)
                           (binding:form #"text"   text))
            (create-item! (~a user) (~a parent) #f (~a text) #f #f)))))))

(define/page (edit/page item)
             'foo)

(define/page (submit/page)
  (let ((user (get-user (current-request))))
    (if (not user)
        (redirect-to "/login")
        (send/suspend/dispatch
          (λ (embed-url)
             (response/xexpr
               (render-page
                 user
                 "New item"
                 `(form ((action ,(embed-url process-item))
                         (method "post"))
                         (div (input    ((class       "field")
                                         (type        "text")
                                         (placeholder "title")
                                         (name        "title"))))
                         (div (input    ((class       "field")
                                         (placeholder "url")
                                         (type        "url")
                                         (name        "url"))))
                         (div (textarea ((class       "field")
                                         (placeholder "text")
                                         (rows        "5")
                                         (name        "text"))))
                         (div (input    ((class       "button")
                                         (type        "submit")
                                         (value       "Submit"))))))))))))


(define/page (reply/page parent)
  (let ((user (get-user (current-request))))
    (if (not user)
        (redirect-to "/login")
        (send/suspend/dispatch
          (λ (embed-url)
             (response/xexpr
               (render-page
                 user
                 "Reply"
                 `(ul ((class "items"))
                      ,(render-item/single user (~a "/reply/" parent) parent))
                 `(form ((action ,(embed-url process-item))
                         (method "post"))
                         (input         ((type        "hidden")
                                         (name        "parent")
                                         (value       ,(~a parent))))
                         (div (textarea ((class       "field")
                                         (placeholder "text")
                                         (rows        "5")
                                         (name        "text"))))
                         (div (input    ((class       "button")
                                         (type        "submit")
                                         (value       "Submit"))))))))))))

(define (plural quantity noun)
  (if (= quantity 1)
      (~a quantity " " noun)
      (~a quantity " " noun "s")))

; bars should perhaps be css?
; less hacky
(define (author-link item)
  `(span " by " (a ((href ,(~a "/user/" (author item)))) ,(author item))))

(define (created-link item)
  `(span " | " (a ((href ,(~a "/item/" item))) ,(age (created item)))))

(define (comments-link item)
  `(span " | " (a ((href ,(~a "/item/" item)))
     ,(if (> (length (descendants item)) 0)
         (plural (length (descendants item)) " comment")
         "discuss"))))

(define (parent-link item)
  `(span " | " (a ((href ,(~a "/item/" (parent item)))) "parent")))

(define (root-link item)
  `(span " | " (a ((href ,(~a "/item/" (root item)))) "on " ,(title (root item)))))

(define (reply-link item)
  `(span " | " (a ((href ,(~a "/reply/" item))) "reply")))

(define (edit-link item)
  `(span " | " (a ((href ,(~a "/edit/" item))) "edit")))

(define (delete-link item)
  'todo)

(define (itemline item)
  `(span ((class "itemline"))
         ,(author-link item)
         ,(created-link item)
         ,(if (= item (root item))
              (comments-link item) "")
         ,(if (parent item)
              (parent-link item) "")
         ,(if (not (= item (root item)))
              (root-link item) "")
         ;TODO: only for the right user. itemline needs a user arg
         ,(edit-link item)
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
  (cond ((regexp-match #px"(?<=https?://).*?(?=/|&|$)" url)
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
                     #:tree? (children? #f))
    `(li
       (div ((class "votable"))
           ,(votelinks item user here)
           (div ((class "item"))
             ,(titleline item)
             ,(itemline item)
             ,(if text? `(div ,(string->xexpr (markdown (xml-attribute-encode (text item))))) "")))
       (div ((class "children"))
            ,(if children? `(ul ((class "items"))
                                ,@(map (curry render-item/tree user here) (children item))) ""))))

(define render-item/single
  (curry render-item #:text? #t))

(define render-item/tree
  (curry render-item #:text? #t #:tree? #t))

(define/page (newest/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Newest"
        `(ul ((class "items"))
              ,@(map (curry render-item user "/") (newest))))))))

(define/page (top/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Top"
        `(ul ((class "items"))
             ,@(map (curry render-item user "/") (top))))))))

(define/page (comments/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Comments"
        `(ul ((class "items"))
             ,@(map (curry render-item/single user "/") (comments))))))))

(define/page (item/page item)
  (let ((user (get-user (current-request))))
    (response/xexpr
      (render-page
        user
        "Item"
        `(ul ((class "items"))
             ,(render-item/tree user (~a "/item/" item) item))))))
