#lang racket

(require db openssl/sha1 file/md5 racket/random)

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
                 salt      BLOB,
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
                 direction INTEGER,
                 time      INTEGER)"

              "create table uploads (
                 upload    INTEGER PRIMARY KEY AUTOINCREMENT,
                 user      TEXT,
                 filename  TEXT,
                 type      BLOB,
                 content   BLOB,
                 time      INTEGER)"

              "create table logins (
                 user TEXT,
                 auth BLOB PRIMARY KEY,
                 time INTEGER)"

              "create table seen (
                 user TEXT,
                 item INTEGER)"

              ; VIEWS
              "create view stories as
                 select * from items
                 where parent is null"

              "create view comments as
                 select * from items
                 where parent is not null
                 order by created desc"

              "create view scores as
                 select items.item as item,
                        sum(direction)
                        as score from items
                 left join votes on votes.item = items.item
                 group by items.item"

              "create view newest as
                 select item from stories
                 order by created desc"

              "create view top as
                 select stories.item as item from stories
                 join ranks on stories.item = ranks.item
                 group by stories.item
                 having rank >= 0
                 order by rank desc, created desc"

              ; TODO: make "ages" view
              ; TODO: ranking needsto be a table, because the algorithm is to complex for SQLite
              "create view ranks as
                 select items.item as item,
                 (score / (julianday('now') - julianday(created,'unixepoch'))) as rank
                 from items
                 join scores on items.item = scores.item"

              "create view karmas as
                 select users.user as user,
                 coalesce(sum(score), 0) as karma
                 from users
                 left join items on users.user = items.user
                 left join scores on items.item = scores.item
                 group by users.user
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
  (let ((salt (crypto-random-bytes 16)))
    (query-exec dbc
      "insert into users values (?, ?, ?, ?, ?)"
      user (false->sql-null email)
      (sha1-bytes (open-input-bytes (bytes-append pw salt)))
      salt
      (current-seconds)))
  (create-login! user pw))

(define (cookie->user auth)
  (sql-null->false
    (query-maybe-value dbc
      "select user from logins where auth = ?" (false->sql-null auth))))

(define (create-item! user parent title text url tags)
  (let ((item
          (cdr (assq 'insert-id (simple-result-info (query dbc
            "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
            sql-null user (false->sql-null parent)
            (false->sql-null title) text (false->sql-null url)
            (false->sql-null tags) (current-seconds)))))))
       (create-vote! user item 0)
       item))

(define (edit-item! item user parent title text url tags)
    (query-exec dbc
            "update items
             set
               title = ?,
               url = ?,
               text = ?,
               tags = ?
             where
               item = ?"
            (false->sql-null title)
            (false->sql-null url)
            (false->sql-null text)
            (false->sql-null tags)
            (false->sql-null (string->number (~a item))))
       item)

(define (create-upload! user filename type content)
  (query-exec dbc
    "insert into uploads values (?, ?, ?, ?, ?, ?)"
    sql-null (if user user sql-null) filename type content (current-seconds)))

(define (sitename)
  (let ((name
   (query-maybe-value dbc "select value from meta where key='sitename' limit 1")))
   (if name name "Undebatable")))

(define (sitelink)
  (query-maybe-value dbc
    "select value from meta where key='sitelink' limit 1"))

(define (set-sitelink! url)
  (query-exec dbc
    "insert into meta values ('sitelink', ?)" url))

(define (set-sitename! name)
  (query-exec dbc
    "insert into meta values ('sitename', ?)" name))

; TODO: pw recovery email details should go in 'meta' as well

(define (salt user)
  (query-value dbc
    "select salt from users where user = ?" user))

(define (seen! user item)
  (when user
    (query-exec dbc
      "insert into seen values (?, ?)" user item)))

(define (seen? user item)
  ; TODO: only stories created after user should be highlighted
  ; and not the users own stories, obvs
  (if user
      (sql-null->false
        (query-maybe-value dbc
          "select item from seen where user = ? and item = ?" user item))
      #t))

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
    "select user from users where user = ?" (false->sql-null  user)))

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

(define (good-login? user pw)
  (cond
    ((query-maybe-value dbc
      "select pwhash from users where user = ?" user)
     => (λ (it) (bytes=?
                    (sha1-bytes (open-input-bytes (bytes-append  pw (salt user))))
                     it)))
    (else #f)))

(define (create-login! user pw)
  (if (and (existing-user? user)
           (bytes=?
             (query-value dbc
               "select pwhash from users where user = ?" user)
             (sha1-bytes (open-input-bytes (bytes-append pw (salt user))))))
      ; TODO: must ensure that auths are unique
      (let ((auth (~a (md5 (crypto-random-bytes 16)))))
           (query-exec dbc
              "insert into logins values (?, ?, ?)"
              user auth (current-seconds))
           auth)
      #f))

(define (delete-vote! user item direction)
  'todo)

(define (create-vote! user item direction)
  (when (votable? user item direction)
    (query-exec dbc
      "insert into votes values (?, ?, ?, ?)"
      user item direction (current-seconds))))

(define (voted user item)
  (query-list dbc
    "select user from votes where user=? and item=?"
    user item))

(define (votable? user item (direction 1))
  (and (existing-user? user)
       (empty? (voted user item))))

(define (score item)
  (query-value dbc
    "select score from scores where item=?"
    item))

(define (votes user)
  (query-list dbc
    "select user from votes where user=?"
    user))
