#lang racket

(require db file/md5 sha)

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
                 item INTEGER PRIMARY KEY AUTOINCREMENT,
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
                 upload INTEGER PRIMARY KEY AUTOINCREMENT,
                 user TEXT,
                 filename TEXT,
                 type BLOB,
                 content BLOB,
                 time INTEGER)"
              "create view stories as
                 select * from items where parent is null"
              "create view scores as
                select item,
                       count(case direction when 'up' then true else null end) -
                         count(case direction when 'down' then true else null end)
                  as score from votes group by item"
              "create view newest as
                 select * from items where parent is null order by created desc"
              "create view top as
                 select * from stories join scores using (item) group by item having score > 0"
              ))
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
  (let ((item
          (cdr (assq 'insert-id (simple-result-info (query dbc
            "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
            sql-null author parent title text url tags (current-seconds)))))))
  ; TODO: should return newly created item?
  ; 'insert-id
  ; https://docs.racket-lang.org/db/query-api.html?q=db#%28def._%28%28lib._db%2Fbase..rkt%29._simple-result%29%29
       (create-vote! author item "up")
       item))

(define (create-upload! user filename type content)
  (query-exec dbc
    "insert into uploads values (?, ?, ?, ?, ?, ?)"
    sql-null user filename type content (current-seconds)))

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
        (format "select ~a from items where item = ?" column) item)))
   (if (sql-null? result) null result)))
(define author ((curry item) 'author))
(define created ((curry item) 'created))
(define text ((curry item) 'text))
(define title ((curry item) 'title))

(define (items) (query-list dbc "select item from items"))

(define parent ((curry item) 'parent))
(define (children item)
 (query-list dbc
   "select item from items where parent = ? order by created desc" item))

(define (uploads)
  (rows-result-rows (query dbc "select upload,filename from uploads")))

(define (download id)
  (first (rows-result-rows (query dbc "select type,content from uploads where upload=?" id))))

(define (root item)
  (let ((p (parent item)))
   (if (not (empty? p))
       (root p)
       item)))

(define (user? name)
  (query-maybe-value dbc
    "select name from users where name = ?" (~a name)))

(define (newest)
 (query-list dbc
   "select item from newest"))

(define (top)
 (query-list dbc
   "select item from top"))

(define (descendants item)
  ; recursive query. is this too ugly?
 (query-list dbc
   "with descendants(n) as (values(?) union select item from items, descendants where items.parent=descendants.n) select item from items where items.item in descendants and not items.item = ?" item item))

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

(define (delete-vote! voter item direction)
  'todo)

(define (create-vote! voter item direction)
  (query-exec dbc
    "insert into votes values (?, ?, ?, ?)"
    voter item direction (current-seconds)))

(define (voted voter item)
  (query-list dbc
    "select voter from votes where voter=? and item=?"
    voter item))

(define (score item)
  (query-value dbc
    "select coalesce((select score from scores where item=?),0)"
    item))

(define (rank item)
  'todo)

(define (user-votes voter)
  (query-list dbc
    "select voter from votes where voter=?"
    voter))

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
