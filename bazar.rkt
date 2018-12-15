#lang racket

(require
  web-server/servlet
  web-server/page
  xml)

(provide (all-defined-out))

; TODO: make a mock-up of a purchase
; so you can see what it looks like to buy a thing
(define/page (bazar/page)
 (response/xexpr
   `(html (body
      "todo"))))

;#### delivery options
;(seller should allowed to chose which ones they accept)
;* Flat rate of £ X.
;* I'll pick up the item(s) myself
;* I'd like to request a postage quote from seller, and confirm once I get the quote
;[ * I'd like the item(s) delivered right away, as long as the cost is less than £ X. ]
;* I'd like the item(s) delivered right away, regardless of price (within reason)
;
;#### payment options
;* currency agnostic (e.g. apples can be a currency, just like £)
;* haggling, like settlers
;* fixed price should be optional (e.g. "null" means haggle)
;
; ### sales conditions for each seller; e.g. a checkbox: returns / cancellations
