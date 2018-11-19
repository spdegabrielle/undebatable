#lang racket
(require datalog
         sha)

(provide (all-defined-out))

(define sitename "my blog")
(define sitelink "http://localhost:8080")

; TODO: time stamp on every fact
; accumulate in memory. periodically merge into storage

; SAVE/LOAD

(define db-file (build-path (find-system-path 'orig-dir) ".db"))

(define (save-db)
  ; TODO: it is excessive to save the whole database every time
  (with-output-to-file db-file (λ () (write-theory db)) #:exists 'replace))

(define db
  (if (file-exists? db-file)
    (with-input-from-file db-file (λ () (read-theory)))
    (make-theory)))

; TODO: this will be saved twice, no?
(datalog db
  (! (:- (story A)
         (item A)
         (parent null A)))
  (! (:- (ancestor A B)
         (parent A B)))
  (! (:- (ancestor A B)
         (parent A C)
         (ancestor C B)))
  (! (:- (root A B)
         (ancestor A B)
         (parent null A)))
  (! (:- (root A B)
         (= A B)
         (parent null A))))

; recursive def to find origin of comment :-D

(define make-id
  (let ((count     (length (datalog db (? (item Item)))))
        (semaphore (make-semaphore 1)))
    ; not sure if the semaphore is necessary
    ; to prevent race condition of giving out same id twice
    (λ ()
       (semaphore-wait semaphore)
       (set! count (+ count 1))
       (semaphore-post semaphore)
       count)))

(define (create-user! name pw (email null))
  ; TODO: limits on chars in username
  (datalog db
    (! (user    name))
    (! (created name #,(current-seconds)))
    (! (pwhash  name #,(sha512 pw)))
    (! (email   name email)))
  (save-db))

(define (create-item! author
                      #:title  (title null)
                      #:text   (text null)
                      #:url    (url null)
                      #:tags   (tags null)
                      #:parent (parent null))
  (let ((id (make-id)))
    (datalog db
      (! (item    id))
      (! (text    id text))
      (! (author  id author))
      (! (created id #,(current-seconds)))
      (! (title   id title))
      (! (parent  parent id)))
    (save-db)
    id))

(define (create-vote! user item direction)
  (datalog db
    (! (voted user item direction #,(current-seconds))))
  (save-db))

(define (create-upload! user filename content)
  (datalog db
    (! (uploaded user filename content #,(current-seconds))))
  ; this is saving the db after EVERY file upload!
  ; consider a post request with 10 files!
 ; (save-db)
  )

; QUERIES

;TODO : use pattern matching to simplify deconstruction of queries

(define (author item)
  (hash-ref (first (datalog db (? (author item Author)))) 'Author))

(define (created item)
  (hash-ref (first (datalog db (? (created item Created)))) 'Created))

(define (text item)
  (hash-ref (first (datalog db (? (text item Text)))) 'Text))

(define (parent item)
  (hash-ref (first (datalog db (? (parent Parent item)))) 'Parent))

(define (root item)
  (hash-ref (first (datalog db (? (root Root item)))) 'Root))

(define (title item)
  ; TODO: yea, needs improvement
  (first (string-split (text item) "\n")))

(define (children item)
  (map (λ (q) (hash-ref q 'Child)) (datalog db (? (parent item Child)))))

(define (descendants item)
  (map (λ (q) (hash-ref q 'Child)) (datalog db (? (ancestor item Child)))))

; TODO: generalize to at get-attribute

(define (items)
  (map (λ (q) (hash-ref q 'Item)) (datalog db (? (item Item)))))

(define (stories)
  (map (λ (q) (hash-ref q 'Item)) (datalog db (? (story Item)))))



;(define (by-date stories)
;  

(define (story? item)
  (not (null?
    (datalog db
      (? (story item))))))

;(define (comment? item)
;  (not (story? item)))

(define (good-login? name pw)
  (not (null?
    (datalog db
      (? (pwhash name #,(sha512 pw)))))))

(define (user? name)
  (not (null?
    (datalog db
      (? (user name))))))


