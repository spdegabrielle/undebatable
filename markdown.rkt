#lang racket
(provide (all-defined-out))
(require xml)
; hacky markdown parser
; https://commonmark.org/help/
(define (markdown text)
  (set! text (regexp-replace* #px"\\*{2}(.+?)\\*{2}" text "<strong>\\1</strong>"))
  (set! text (regexp-replace* #px"\\*{1}(.+?)\\*{1}" text "<em>\\1</em>"))
  (set! text (regexp-replace* #px"\\`{1}(.+?)\\`{1}" text "<code>\\1</code>"))
  (set! text (regexp-replace* #px"\\~{2}(.+?)\\~{2}" text "<s>\\1</s>"))
  (set! text (regexp-replace* #px"!\\[(.+?)\\]\\((.+)\\)" text "<img src=\"\\2\" alt=\"\\1\"></img>"))
  (set! text (regexp-replace* #px"\\[(.+?)\\]\\((.+)\\)" text "<a href=\"\\2\">\\1</a>"))
  (set! text (regexp-replace* #px"(\r\n&gt;)+(.+?)\r\n" text "<blockquote>\\2</blockquote>"))
; (set! text (regexp-replace* #px"^#{3}(.+)$" text "<h6>\\1</h6>"))
; (set! text (regexp-replace* #px"^#{3}(.+)$" text "<h5>\\1</h5>"))
; (set! text (regexp-replace* #px"^#{3}(.+)$" text "<h4>\\1</h4>"))
; (set! text (regexp-replace* #px"^#{3}(.+)$" text "<h3>\\1</h3>"))
; (set! text (regexp-replace* #px"^#{2}(.+)$" text "<h2>\\1</h2>"))
; (set! text (regexp-replace* #px"^#{1}(.+)$" text "<h1>\\1</h1>"))
  (set! text (regexp-replace* #px"(\r?\n){2,}" text "<br/><br/>"))
  ;hashtags and user mentions:
  (set! text (regexp-replace* #px"@(\\w+)" text "<a href=\"user/\\1\">@\\1</a>"))
  (set! text (regexp-replace* #px"#(\\w+)" text "<a href=\"tag/\\1\">#\\1</a>"))
  (~a "<span>" text "</span>"))

(define (markdown->xexpr text)
  (string->xexpr (markdown (xml-attribute-encode text))))

(define markdown-doc
  `(div ((class "md-ref"))
     (dl
       (dt "*emphasis*") (dd ,(markdown->xexpr "*emphasis*"))
       (dt "**strong**") (dd ,(markdown->xexpr "**strong**"))
       (dt "`code`")     (dd ,(markdown->xexpr "`code`"))
       (dt "[gnu](https://gnu.org)`")   (dd ,(markdown->xexpr "[gnu](https://gnu.org)"))
;       (dt "![antisocial](/antisocial.png)`") (dd ,(markdown->xexpr "![antisocial](/antisocial.png)"))
       )))
