#lang racket

(require
  "model.rkt"
  "page.rkt"
  "login.rkt"
  "vote.rkt"
  web-server/servlet
  web-server/page
  anaphoric
  (only-in markdown parse-markdown)
  xml)

(provide submit/page reply/page newest/page item/page edit/page top/page)

(define/page (process-item)
  (let ((user (get-user (current-request))))
       (redirect-to (~a "/item/"
         (match (request-bindings/raw (current-request))
           ((list-no-order (binding:form #"title" title)
                           (binding:form #"text" text))
                   (create-item! (~a user)
                       #:title  (~a title)
                       #:text (~a text)))
           ((list-no-order (binding:form #"parent" parent)
                           (binding:form #"text"   text))
            (create-item! (~a user)
                       #:parent (string->number (~a parent))
                       #:text   (~a text))))))))

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
                         (div (textarea ((class       "field")
                                         (placeholder "text")
                                         (rows        "5")
                                         (cols        "40")
                                         (name        "text"))))
                         (div (input    ((class "button")
                                         (type  "submit")
                                         (value "Submit"))))))))))))


(define/page (reply/page parent)
  (let ((user (get-user (current-request))))
    (if (not user)
        (redirect-to "/login")
        (send/suspend/dispatch
          (λ (embed-url)
             (response/xexpr
               (render-page
                 user
                 (~a "Reply to " (title parent))
                 `(ul ,(render-item/single user (~a "/reply/" parent) parent))
                 `(form ((action ,(embed-url process-item))
                         (method "post"))
                         (input ((type  "hidden")
                                 (name  "parent")
                                 (value ,(~a parent))))
                         (div (textarea ((class       "field")
                                         (placeholder "text")
                                         (rows        "5")
                                         (cols        "40")
                                         (name        "text"))))
                         (div (input    ((class "button")
                                         (type  "submit")
                                         (value "Submit"))))))))))))



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
  'todo)

(define (delete-link item)
  'todo)

(define (itemline item)
  `(span ((class "itemline"))
         ,(author-link item)
         ,(created-link item)
         ,(if (= item (root item))
              (comments-link item) "")
         ,(if (not (empty? (parent item)))
              (parent-link item) "")
         ,(if (not (= item (root item)))
              (root-link item) "")
         ,(reply-link item)))

(define (age seconds)
  (let ((since (- (current-seconds) seconds)))
    (~a (cond ((> since (* 60 60 24)) (plural (floor (/ since (* 60 60 24))) "day"))
              ((> since (* 60 60))    (plural (floor (/ since (* 60 60)))    "hour"))
              (else                   (plural (floor (/ since (* 60)))       "minute")))
        " ago")))

(define (markdown text)
  (parse-markdown (xml-attribute-encode text)))

(define (render-item user here item
                     #:title?    (title? #f)
                     #:text?     (text? #f)
                     #:children? (children? #f)
                     #:reply?    (reply? #f))
    `(li
       (div ((class "votable"))
           ,(votelinks item user here)
           (div ((class "item"))
             ,(itemline item)
             ,(if title? `(div (a ((class "title") (href ,(~a "/item/" item))) ,(title item))) "")
             ,(if text? `(div ,@(markdown (text item))) "")))
       (div ((class "children"))
            ,(if children? `(ul ,@(map (curry render-item/tree user here) (children item))) ""))))

(define render-item/list
  (curry render-item #:title? #t))

(define render-item/single
  (curry render-item #:text? #t))

(define render-item/tree
  (curry render-item #:text? #t #:reply? #t #:children? #t))

(define/page (newest/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Newest"
        `(ul ,@(map (curry render-item/list user "/") (newest))))))))

(define/page (top/page)
  (let ((user (get-user (current-request))))
    (time (response/xexpr
      (render-page
        user
        "Top"
        `(ul ,@(map (curry render-item/list user "/") (top))))))))

(define/page (item/page item)
  (let ((user (get-user (current-request))))
    (response/xexpr
      (render-page
        user
        "Item"
        `(ul ,(render-item/tree user (~a "/item/" item) item))))))
