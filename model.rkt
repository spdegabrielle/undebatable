#lang racket

(require db sha)

(provide (all-defined-out))


(define (sqlite3-init file)
  (if (file-exists? file)
      (sqlite3-connect #:database file)
      (let ((new-db (sqlite3-connect #:database file #:mode 'create)))
       (map ((curry query-exec) new-db)
            '("create table site (
                 name TEXT,
                 link TEXT)"
              "create table users (
                 name TEXT,
                 email TEXT,
                 pwhash BLOB,
                 created INTEGER)"
              "create table items (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 author TEXT,
                 parent INTEGER,
                 title TEXT,
                 text TEXT,
                 url TEXT,
                 tags TEXT,
                 created INTEGER)"
              "create table votes (
                 voter TEXT,
                 item INTEGER,
                 direction TEXT,
                 time INTEGER)"
              "create table uploads (
                 user TEXT,
                 filename TEXT,
                 content BLOB,
                 time INTEGER)"))
           new-db)))

(define dbc (sqlite3-init "db.sqlite"))

;TODO: when-exists? or something like that
; like a macro, to display xexpr when domsehting is present
(define (blank? field)
  (or (not field)
      (null? field)
      (sql-null? field)))


(define (create-user! name pw (email sql-null))
  ; PREVENT DUPLICATES. name is primary key?
  (query-exec dbc
    "insert into users values (?, ?, ?, ?)"
    name (sha512 pw) email (current-seconds)))

(define (create-item! author
                   #:parent (parent sql-null)
                   #:title  (title  sql-null)
                   #:text   (text   sql-null)
                   #:url    (url    sql-null)
                   #:tags   (tags   sql-null))
  ; TODO: should return newly created item?
  (query-exec dbc
    "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
    sql-null author parent title text url tags (current-seconds)))

(define (create-vote! voter item direction)
  (query-exec dbc
    "insert into votes values (?, ?, ?, ?)"
    voter item direction (current-seconds)))

(define (create-upload! user filename content)
  (query-exec dbc
    "insert into uploads values (?, ?, ?, ?)"
    user filename content (current-seconds)))

(define (sitename)
  (let ((name (query-maybe-value dbc "select name from site limit 1")))
   (if name name "My Forum")))

(define (sitelink)
  (query-maybe-value dbc
    "select link from site limit 1"))

(define (configure-site! name link)
  (query-exec dbc
    "insert into site values (?, ?)" name link))

(define (item column item)
  (let
   ((result
     (query-maybe-value dbc
        (format "select ~a from items where id = ?" column) item)))
   (if (sql-null? result) null result)))
(define author ((curry item) 'author))
(define created ((curry item) 'created))
(define text ((curry item) 'text))
(define title ((curry item) 'title))

(define (items) (query-list dbc "select id from items"))

(define parent ((curry item) 'parent))
(define (children item)
 (query-list dbc
   "select id from items where parent = ? order by created desc" item))

(define (uploads) (query-list dbc "select filename from uploads"))

(define (root item)
  (let ((p (parent item)))
   (if (not (empty? p))
       (root p)
       item)))

(define (user? name)
  (query-maybe-value dbc
    "select name from users where name = ?" name))

(define (stories)
 (query-list dbc
   "select id from items where parent is null"))

(define (newest)
 (query-list dbc
   "select id from items where parent is null order by created desc"))

(define (descendants item)
 (query-list dbc
   "with descendants(n) as (values(?) union select id from items, descendants where items.parent=descendants.n) select id from items where items.id in descendants and not items.id = ?" item item))

(define (search terms)
  ;TODO
  ; https://sqlite.org/fts3.html
  (query-list dbc
    "SELECT count(*) FROM enrondata1 WHERE content MATCH 'linux';"))

;TODO: put all cookies in the model and in the database
(define (good-login? name pw)
  (cond
    ((query-maybe-value dbc
      "select pwhash from users where name = ?" name)
     => (λ (it) (bytes=? (sha512 pw) it)))
    (else #f)))

;(create-item! "pelle" #:text "hello world")
;(author 1)
;(stories)
;(get-item "author" 1)
;(author 1)

;(new-user! "pelle" #"foo")
;(new-item! "pelle" #:text "hello world")
;(new-vote! "pelle" 2 "up")

;(query-list dbc "select name from users where name like '%'")

;(query-exec dbc "insert into the_numbers values (42, 'the answer')")
;(query-exec dbc "delete from the_numbers where n = $1" 42)
