#lang racket

(require
  "model.rkt"
  "login.rkt"
  "page.rkt"
  web-server/servlet
  web-server/servlet-env
  web-server/dispatch
  web-server/page
  web-server/http/id-cookie)

; TODO: optionable anonymous file upload, like wetransfer
; but these files delete themselves after 30 days, perhaps?

; TODO: post requests have a MIME type, yes?
; use that to determine file type

(provide upload/page uploads/page)

(define/page (upload/page)

  (define/page (process-upload)
    (let ((user (get-user (current-request)))
          (uploads null))
          (map (λ (binding)
                  (match binding
                    ((binding:file field filename headers content)
                     (create-upload! user (~a filename) content))))
               (request-bindings/raw (current-request)))
    ; TODO: should redirect to image list
          (redirect-to "/")))

  (let ((user (get-user (current-request))))
    (if (not user)
        (redirect-to "/login")
        (send/suspend/dispatch
          (λ (embed-url)
             (response/xexpr
               (render-page
                 user
                 "Upload"
                 `(form ((action ,(embed-url process-upload))
                         (method "post")
                         (enctype "multipart/form-data"))
                         (div (input    ((type        "file")
                                         (name        "file")
                                         (multiple    "multiple"))))
                         (div (input    ((type "submit")
                                         (value "Upload"))))))))))))


; FILES

(define/page (uploads/page)
  (response/xexpr
    (render-page
      (get-user (current-request))
      "Items"
      `(div ,(~a (uploads))))))
;     `(ul ,@(map (λ (f) `(li ,(download-link (build-path "/" f))))
;                 (directory-list))))))


(define (render-file filepath)
  (match (string-downcase (~a (path-get-extension filepath)))
    ((or ".gif" ".png" ".jpg" ".jpeg")
     `(a ((href ,filepath)) (img ((class "embed") (src ,filepath)))))
    ((or ".webm" ".mp4" ".ogv")
     `(video ((class "embed") (src ,filepath) (controls "controls"))))
    ((or ".mp3" ".m4a" ".ogg")
     `(audio ((class "embed") (src ,filepath) (controls "controls"))))
    (anything
     `(a ((href ,filepath)) "file"))))
