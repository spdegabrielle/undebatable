#lang racket
(provide (all-defined-out))
(require xml)
; hacky markdown parser
; https://commonmark.org/help/
(define (markdown text)
  (set! text (regexp-replace* #px"\\*{2}(.+?)\\*{2}" text "<strong>\\1</strong>"))
  (set! text (regexp-replace* #px"\\*{1}(.+?)\\*{1}" text "<italic>\\1</italic>"))
  (set! text (regexp-replace* #px"\\`{1}(.+?)\\`{1}" text "<code>\\1</code>"))
  (set! text (regexp-replace* #px"\\~{2}(.+?)\\~{2}" text "<s>\\1</s>"))
  (set! text (regexp-replace* #px"!\\[(.+?)\\]\\((.+)\\)" text "<img src=\"\\1\" alt=\"\\2\"></img>"))
  (set! text (regexp-replace* #px"\\[(.+?)\\]\\((.+)\\)" text "<a href=\"\\2\">\\1</a>"))
  (set! text (regexp-replace* #px"(\r\n&gt;)+(.+?)\r\n" text "<blockquote>\\2</blockquote>"))
; (set! text (regexp-replace* #px"^#{3}(.+)$" text "<h3>\\1</h3>"))
; (set! text (regexp-replace* #px"^#{2}(.+)$" text "<h2>\\1</h2>"))
; (set! text (regexp-replace* #px"^#{1}(.+)$" text "<h1>\\1</h1>"))
  (set! text (regexp-replace* #px"(\r?\n){2,}" text "<br/><br/>"))
  ;hashtags and user mentions:
  (set! text (regexp-replace* #px"@(\\w+)" text "<a href=\"user/\\1\">@\\1</a>"))
  (set! text (regexp-replace* #px"#(\\w+)" text "<a href=\"tag/\\1\">#\\1</a>"))
  (~a "<div>" text "</div>"))

(define (markdown->xexpr text)
  (string->xexpr (markdown (xml-attribute-encode text))))
