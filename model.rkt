#lang racket

(require db openssl/sha1)

(provide (all-defined-out))

; TODO : do some kind of ORM or some structs to return some sane Racket objects;
; and to do less queries!
; it will give a simpler interface, too, if the entire item with all useful info is returned
; but it *does* need more fields than in the table to account for derived data, e.g. votes, etc

(define (sqlite3-init file)
  (if (file-exists? file)
      (sqlite3-connect #:database file)
      (let ((new-db (sqlite3-connect #:database file #:mode 'create)))
       (map ((curry query-exec) new-db)
            ; TABLES
            '("create table site (
                 name      TEXT,
                 link      TEXT)"

              "create table users (
                 user      TEXT,
                 email     TEXT,
                 pwhash    BLOB,
                 created   INTEGER)"

              "create table items (
                 item      INTEGER PRIMARY KEY AUTOINCREMENT,
                 user      TEXT,
                 parent    INTEGER,
                 title     TEXT,
                 text      TEXT,
                 url       TEXT,
                 tags      TEXT,
                 created   INTEGER)"

              "create table votes (
                 user      TEXT,
                 item      INTEGER,
                 direction TEXT,
                 time      INTEGER)"

              "create table uploads (
                 upload    INTEGER PRIMARY KEY AUTOINCREMENT,
                 user      TEXT,
                 filename  TEXT,
                 type      BLOB,
                 content   BLOB,
                 time      INTEGER)"

              ; VIEWS
              "create view stories as
                 select * from items
                 where parent is null"

              "create view comments as
                 select * from items
                 where parent is not null
                 order by created desc"

              "create view scores as
                select item,
                       count(case direction when 'up' then 1 else null end) -
                         count(case direction when 'down' then 1 else null end)
                  as score from votes
                  group by item"

              "create view newest as
                 select * from stories
                 order by created desc"

              "create view top as
                 select * from stories join scores using (item)
                 group by item having score > 0
                 order by score desc, created desc"

              "create view karmas as
                 select user,coalesce(sum(score),0) as karma
                 from users
                 left join items using (user)
                 left join scores using (item)
                 group by user
                 order by score desc"
              ; TODO descendants
              ; TODO children
              ; TODO add NOT NULL constraints
              ))
           new-db)))

(define dbc (sqlite3-init "db.sqlite"))

;TODO: when-exists? or something like that
; like a macro, to display xexpr when domsehting is present
(define (blank? field)
  (or (not field)
      (null? field)
      (sql-null? field)))

(define (create-user! user pw (email sql-null))
  ; PREVENT DUPLICATES. user is primary key?
  (query-exec dbc
    "insert into users values (?, ?, ?, ?)"
    user email (sha1-bytes (open-input-bytes pw)) (current-seconds)))

(define (create-item! user
                   #:parent (parent sql-null)
                   #:title  (title  sql-null)
                   #:text   (text   sql-null)
                   #:url    (url    sql-null)
                   #:tags   (tags   sql-null))
  (let ((item
          (cdr (assq 'insert-id (simple-result-info (query dbc
            "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
            sql-null user parent title text url tags (current-seconds)))))))
  ; TODO users shouldn't be able to vote for their own and gain karma
       (create-vote! user item "up")
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
(define author ((curry item) 'user))
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

(define (user? user)
  (query-maybe-value dbc
    "select user from users where user = ?" (~a user)))

(define (newest)
 (query-list dbc
   "select item from newest"))

(define (comments)
 (query-list dbc
   "select item from comments"))

(define (top)
 (query-list dbc
   "select item from top"))

(define (karma user)
 (query-value dbc
   "select karma from karmas where user=?" user))

(define (descendants item)
  ; recursive query. is this too ugly?
 (query-list dbc
   "with descendants(n) as (values(?)
    union select item from items,descendants
    where items.parent=descendants.n)
   select item from items
   where items.item in descendants and not items.item = ?"
   item item))

(define (search terms)
  ;TODO
  ; https://sqlite.org/fts3.html
  (query-list dbc
    "SELECT count(*) FROM enrondata1 WHERE content MATCH 'linux';"))

;TODO: put all cookies in the model and in the database
(define (good-login? user pw)
  (cond
    ((query-maybe-value dbc
      "select pwhash from users where user = ?" user)
     => (λ (it) (bytes=? (sha1-bytes (open-input-bytes pw)) it)))
    (else #f)))

(define (delete-vote! user item direction)
  'todo)

(define (create-vote! user item direction)
  (query-exec dbc
    "insert into votes values (?, ?, ?, ?)"
    user item direction (current-seconds)))

(define (voted user item)
  (query-list dbc
    "select user from votes where user=? and item=?"
    user item))

(define (score item)
  (query-value dbc
    "select coalesce((select score from scores where item=?),0)"
    item))

(define (rank item)
  'todo)

(define (user-votes user)
  (query-list dbc
    "select user from votes where user=?"
    user))
