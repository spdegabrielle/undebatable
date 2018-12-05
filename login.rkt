#lang racket

(require
  "model.rkt"
  "page.rkt"
  web-server/servlet
  web-server/page
  web-server/http/cookie
  net/smtp
  net/head
  openssl)

(provide (all-defined-out))

; TODO: Secure cookies and add CSFR hidden form fields on all POST requests
; https://www.owasp.org/index.php/Cross-Site_Request_Forgery_%28CSRF%29_Prevention_Cheat_Sheet
; and make all state-changing requests use POST, e.g. logout
; 
; LOGIN

(define (get-user request)
  (cookie->user
    (cond
      ((findf
         (λ (maybe-cookie) (string=? (client-cookie-name maybe-cookie) "login"))
         (request-cookies request)) => client-cookie-value)
      (else #f))))

(define/page (login/page)

  (define/page (register!)
    (match
      (request-bindings/raw (current-request))
      ((list-no-order
         (binding:form #"username" username)
         (binding:form #"password" password)
         (binding:form #"email"    email))
         (if (and (not (existing-user? (~a username)))
                  (regexp-match #px"\\w+" (~a username)))
             (let ((login (create-user! (~a username) password (~a email))))
                   (redirect-to "/"
                                ; the made cookie needs to be secure and prevent CSRF
                                ; file:///usr/share/doc/racket/cookies/index.html
                                ; Set-Cookie: JSESSIONID=xxxxx; SameSite=Strict
                     #:headers (list (cookie->header (make-cookie "login" login)))))
             (response/xexpr
               (render-page (get-user (current-request))
                            "Error" "Unable to create user account."))))))

  (define/page (login!)
    (match (request-bindings/raw (current-request))
           ((list-no-order
               (binding:form #"username" username)
               (binding:form #"password" password))
           (let ((login (create-login! (~a username) password)))
                (if login
                    (redirect-to "/"
                      #:headers (list (cookie->header (make-cookie "login" login))))
                    (response/xexpr
                      (render-page (get-user (current-request)) "Error" "Login failed.")))))))

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
                     (div (input ((class       "field")
                                  (type        "text")
                                  (placeholder "username")
                                  (required    "required")
                                  (name        "username"))))
                     (div (input ((class       "field")
                                  (type        "password")
                                  (required    "required")
                                  (placeholder "password")
                                  (name        "password"))))
                     (div (input ((class       "button")
                                  (type        "submit")
                                  (value       "Log in"))))
                     (br)
                     ,(when (not (null? app-email))
                          `(div (a ((href "forgot")) "Forgot password"))))
             `(form ((action ,(embed-url register!))
                     (method "post"))
                     (h3 "Register")
                     (div (input ((class       "field")
                                  (type        "text")
                                  (pattern     "\\w+")
                                  (required    "required")
                                  (placeholder "username")
                                  (name        "username"))))
                     (div (input ((class       "field")
                                  (type        "email")
                                  ;(required "required")
                                  (placeholder "email")
                                  (name        "email"))))
                     (div (input ((class       "field")
                                  (type        "password")
                                  ; suggested min-length of password
                                  (pattern     ".{4,}")
                                  (required    "required")
                                  (placeholder "password")
                                  (name        "password"))))
                     (div (input ((class       "button")
                                  (type        "submit")
                                  (value       "Register"))))))))))))

(define/page (logout/page)
  (redirect-to "/"
    #:headers (list (cookie->header (make-cookie "login" "")))))

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
