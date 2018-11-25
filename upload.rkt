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

(provide (all-defined-out))

(define/page (upload/page)
  (define/page (process-upload)
    (map (λ (binding)
            (match binding
              ((binding:file field filename headers content)
               (let ((user (get-user (current-request)))
                     (content-type (header-value (headers-assq #"Content-Type" headers))))
                    (create-upload! user (~a filename) content-type content)))))
               (request-bindings/raw (current-request)))
    (redirect-to "/uploads"))
    ; TODO: should redirect to image list

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
                         (div (input    ((class       "field")
                                         (type        "file")
                                         (name        "file")
                                         (multiple    "multiple"))))
                         (div (input    ((class  "button")
                                         (type   "submit")
                                         (value  "Upload"))))))))))))


; FILES

(define (download-link id/filename)
  `(a ((href ,(~a "/uploads/"
                  (vector-ref id/filename 0) "/"
                  (vector-ref id/filename 1))))
      ,(~a (vector-ref id/filename 1))))

(define/page (uploads/page)
  (response/xexpr
    (render-page
      (get-user (current-request))
      "Uploads"
     `(ul ,@(map (λ (f) `(li ,(download-link f)))
                 (uploads))))))

(define/page (download/page file-id (filename null))
  (letrec ((type/content (download file-id))
           (type (vector-ref type/content 0))
           (content (vector-ref type/content 1)))
  (response
    200 #"OK"
    (current-seconds) type ;TEXT/HTML-MIME-TYPE
    empty
    (λ (op) (write-bytes content op)))))
;    (bytes->list (file-content file-id))))

(define (render-file filepath)
  ; not in use right now
  ; TODO: base on MIME 
  (match (string-downcase (~a (path-get-extension filepath)))
    ((or ".gif" ".png" ".jpg" ".jpeg")
     `(a ((href ,filepath)) (img ((class "embed") (src ,filepath)))))
    ((or ".webm" ".mp4" ".ogv")
     `(video ((class "embed") (src ,filepath) (controls "controls"))))
    ((or ".mp3" ".m4a" ".ogg")
     `(audio ((class "embed") (src ,filepath) (controls "controls"))))
    (anything
     `(a ((href ,filepath)) "file"))))
