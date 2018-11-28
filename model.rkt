#lang racket

(require db openssl/sha1 racket/random)

(provide (all-defined-out))

(define (sqlite3-init file)
  (if (file-exists? file)
      (sqlite3-connect #:database file)
      (let ((new-db (sqlite3-connect #:database file #:mode 'create)))
       (map ((curry query-exec) new-db)
            ; TABLES
            '("create table meta (
                 key       TEXT,
                 value     TEXT)"

              "create table users (
                 user      TEXT PRIMARY KEY,
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

              "create table messages (
                 sender    TEXT,
                 receiver  TEXT,
                 content   TEXT,
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
                       coalesce(
                         count(case direction when 'up' then 1 else null end) -
                           count(case direction when 'down' then 1 else null end),
                         0)
                  as score from items left join votes using (item)
                  group by item"

              "create view newest as
                 select * from stories
                 order by created desc"

              "create view top as
                 select * from stories join ranks using (item)
                 group by item having rank >= 0.1
                 order by rank desc, created desc"

              ; TODO: make "ages" view
              ; TODO: SQLite is probably not suitable for impelmenting ranking algorithm
              "create view ranks as
                 select item,
                 (score / (julianday('now') - julianday(created,'unixepoch'))) as rank
                 from items join scores using(item)"

              "create view karmas as
                 select user,coalesce(sum(score),0) as karma
                 from users
                 left join items using (user)
                 left join scores using (item)
                 group by user
                 order by score desc"

              ; first user is automagically admin,
              ; and should be able to invite others to be too
              "create view owner as
                 select user from users
                 order by created asc
                 limit 1"

              "create view roots as
                 with roots as (
                   select item, parent, item as root
                   from items
                   where parent is null
                   union all
                   select items.item, items.parent, root
                     from roots
                     join items
                     on items.parent = roots.item)
                 select item, root from roots"

              "create view descendants as
                 with descendants as(
                   select item, parent, item as root
                   from items
                   union all
                   select items.item, items.parent, root
                     from descendants
                     join items
                     on descendants.item = items.parent)
                 select root as item,
                        descendants.item as descendant
                   from descendants
                   where root != descendants.item
                   order by root, descendants.item"

              ; TODO descendants
              ; TODO children
              ; TODO add NOT NULL constraints
              ))
           new-db)))

(define dbc (sqlite3-init "db.sqlite"))

; TODO: sha256-bytes available in Racket 7.1
(define (create-user! user pw (email sql-null))
  (query-exec dbc
    "insert into users values (?, ?, ?, ?)"
    user (false->sql-null email)
    (sha1-bytes (open-input-bytes pw))
    (current-seconds)))

(define (create-item! user parent title text url tags)
  (let ((item
          (cdr (assq 'insert-id (simple-result-info (query dbc
            "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
            sql-null user (false->sql-null parent)
            (false->sql-null title) text (false->sql-null url)
            (false->sql-null tags) (current-seconds)))))))
  ; TODO users shouldn't be able to vote for their own and gain karma
;       (create-vote! user item "up")
       item))

(define (create-upload! user filename type content)
  (query-exec dbc
    "insert into uploads values (?, ?, ?, ?, ?, ?)"
    sql-null (if user user sql-null) filename type content (current-seconds)))

(define (sitename)
  (let ((name
   (query-maybe-value dbc "select value from meta where key='sitename' limit 1")))
   (if name name "My Forum")))

(define (sitelink)
  (query-maybe-value dbc
    "select value from meta where key='sitelink' limit 1"))

(define (set-sitelink! url)
  (query-exec dbc
    "insert into meta values ('sitelink', ?)" url))

(define (set-sitename! name)
  (query-exec dbc
    "insert into meta values ('sitename', ?)" name))

(define (salt)
  ;TODO ensure this is not randomly overwritten as it's quite important
  (let ((s (query-maybe-value dbc
              "select value from meta where key='salt' limit 1")))
       (if s s
           (begin
             (query-exec dbc
                "insert into meta values ('salt', ?)" (crypto-random-bytes 16))
             (salt)))))

(define (item column item)
  (sql-null->false
     (query-maybe-value dbc
        (format "select ~a from items where item = ?" column) item)))
(define author ((curry item) 'user))
(define created ((curry item) 'created))
(define text ((curry item) 'text))
(define title ((curry item) 'title))
(define parent ((curry item) 'parent))
(define url/item ((curry item) 'url))

(define (rows->hashes rows)
  ; is this better than having lots of individual functions?
  ; is this an ORM?
  ; it's quite possibly faster than querying multiple times
  (map (λ (row)
          (make-hash
            (map cons
                 (map cdar (rows-result-headers rows))
                 (vector->list row))))
       (rows-result-rows rows)))

;(define (item2 id)
;  (first (rows->hashes (query dbc "select * from items where item = ?" id))))
;
;(hash-ref (item2 3) "created")

(define (children item)
 (query-list dbc
   "select item from items where parent = ? order by created desc" item))

(define (uploads)
  (rows-result-rows
    (query dbc "select upload, filename from uploads")))

(define (download id)
  (first (rows-result-rows
    (query dbc "select type, content from uploads where upload=?" id))))

(define (root item)
 (query-value dbc
   "select root from roots where item = ?" item))

(define (existing-user? user)
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
 (query-list dbc
   "select descendant from descendants where item = ?" item))

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
    "select score from scores where item=?"
    item))

(define (rank item)
  'todo)

(define (votes user)
  (query-list dbc
    "select user from votes where user=?"
    user))
