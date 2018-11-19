#lang racket

(require
  "model.rkt"
  "page.rkt"
  web-server/servlet
  web-server/page
  web-server/http/id-cookie
  net/smtp
  net/head
  openssl)

(provide (all-defined-out))

; LOGIN

; racket/random
; TODO: use (crypto-random-bytes) for salt, and save it in the database
(define salt (make-secret-salt/file ".salt"))

(define (make-login-cookie username)
  (make-id-cookie "login" salt (~a username)))

(define (get-user request)
  (let ((user (request-id-cookie "login" salt request)))
       (if (user? user) user #f)))

(define/page (login/page)

  (define/page (register!)
    ; TODO: what about '#' or '?' in usernames? it would break links
    (match
      (request-bindings/raw (current-request))
      ((list-no-order
         (binding:form #"username" username)
         (binding:form #"password" password)
         (binding:form #"email"    email))
         (if (not (user? (~a username)))
             (begin
               (create-user! (~a username) password (~a email))
               (redirect-to "/"
                 #:headers (list (cookie->header (make-login-cookie username)))))
             (response/xexpr
               (render-page (get-user (current-request))
                            "Error" "User already exists."))))))

  (define/page (login!)
    (match (request-bindings/raw (current-request))
           ((list-no-order
               (binding:form #"username" username)
               (binding:form #"password" password))
           (if (good-login? (~a username) password)
               (redirect-to "/"
                 #:headers (list (cookie->header (make-login-cookie username))))
               (response/xexpr
                 ; TODO: should just be shown as a message
                 (render-page (get-user (current-request)) "Error" "Login failed."))))))

  (let ((user (get-user (current-request))))
    (if user
        (redirect-to "/")
    (send/suspend/dispatch
      (λ (embed-url)
         (response/xexpr
           (render-page
             user
             "Login"
             `(form ((action ,(embed-url login!))
                     (method "post"))
                     (h3 "Log in")
                     (div (input ((type "text")
                                  (placeholder "username")
                                  (required "required")
                                  (name "username"))))
                     (div (input ((type "password")
                                  (required "required")
                                  (placeholder "password")
                                  (name "password"))))
                     (div (input ((type "submit")
                                  (value "Log in"))))
                     (br)
                     ,(when (not (null? app-email))
                          `(div (a ((href "forgot")) "Forgot password"))))
             `(form ((action ,(embed-url register!))
                     (method "post"))
                     (h3 "Register")
                     (div (input ((type "text")
                                  (required "required")
                                  (placeholder "username")
                                  (name "username"))))
                     (div (input ((type "email")
                                  ;(required "required")
                                  (placeholder "email")
                                  (name "email"))))
                     (div (input ((type "password")
                                  ; suggested min-length of password
                                  (pattern ".{4,}")
                                  (required "required")
                                  (placeholder "password")
                                  (name "password"))))
                     (div (input ((type "submit")
                                  (value "Register"))))))))))))

(define/page (logout/page)
  (redirect-to "/"
    #:headers (list (cookie->header (logout-id-cookie "login")))))



;; PASSWORD RECOVERY

(define app-email
  (hash 'address  "hjek@mail.com"
        'server   "smtp.mail.com"
        'password "krabit93113"))

(define/page (forgot-password/page)
  (define forgot-req
    (send/suspend
      (λ (k-url)
        (response/xexpr
          (render-page
                    (get-user (current-request))
                    "Forgot password"
                    `(form ((action ,k-url)
                            (method "post"))
                           "Please enter your email address:"
                           (div (input ((type        "email")
                                        (name        "email")
                                        (placeholder "email")
                                        (required    "required")))
                           (div (input ((type        "submit")
                                        (value       "Reset password")))))))))))

  (define email-req
    (send/suspend
      (λ (k-url)
        (match (request-bindings/raw forgot-req)
          ((list (binding:form #"email" email))
           (letrec ((from        (hash-ref app-email 'address))
                    (to          (list (~a email)))
                    (smtp-server (hash-ref app-email 'smtp-server))
                    (password    (hash-ref app-email 'password))
                    (subject     "Please reset your password")
                    (header      (standard-message-header from to null null subject)))
             (smtp-send-message smtp-server from to header
               (list (string-append "Please go to " sitelink k-url))
               #:auth-user from
               #:auth-passwd password
               #:tls-encode ports->ssl-ports)
             (response/xexpr
               (render-page (get-user (current-request))
                         "Reset your password"
                         "Password recovery email sent."))))))))

  ;TODO: reset password and redirect to frontpage
   (response/xexpr
     (render-page
       (get-user (current-request))
               "Reset password"
               `(form ((method "post"))
                      (input ((type        "password")
                              (placeholder "new password")
                              (required    "required")))
                      (input ((type  "submit")
                              (value "Reset password")))))))
