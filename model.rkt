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
                        coalesce(sum(direction),0)
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

              ; TODO: perhaps make "ages" view
              ; TODO: ranking may need to be a table, because the algorithm might be too complex for SQLite

              "create view ranks as
                 select items.item as item,
                 (score * score * score / (julianday('now') - julianday(created,'unixepoch'))) as rank
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

                "create view expiries as
                 select upload,
                 time+(60*60*24*30*365*1000000/length(content)) as expiry
                 from uploads")

              ; TODO add NOT NULL constraints
           new-db))))

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

(define (auth->user auth)
  (sql-null->false
    (query-maybe-value dbc
      "select user from logins
       where auth = ?"
      (false->sql-null auth))))

(define (user->auth user)
  (sql-null->false
    (query-maybe-value dbc
      "select auth from logins
       where user = ? limit 1"
      (false->sql-null user))))

(define (create-item! user parent title text url tags)
  (let ((item
          (cdr (assq 'insert-id (simple-result-info (query dbc
            "insert into items values (?, ?, ?, ?, ?, ?, ?, ?)"
            sql-null user (false->sql-null parent)
            (false->sql-null title) text (false->sql-null url)
            (false->sql-null tags) (current-seconds)))))))
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
          "select item from seen
           where (user = ? and item = ?)
           " user item))
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
  (first
    (rows-result-rows
      (query dbc "select type, content from uploads where upload=?" id))))

(define (expiry upload)
  (query-maybe-value dbc
    "select expiry from expiries
     where upload = ?"
    upload))

(define (root item)
 (query-value dbc
   "select root from roots where item = ?" item))

(define (existing-user? user)
  (query-maybe-value dbc
    "select user from users where user = ?" (false->sql-null  user)))

(define (newest (limit -1) (offset -1))
 (query-list dbc
   "select item from newest
    limit ? offset ?"
   limit offset))

(define (comments (limit -1) (offset -1))
 (query-list dbc
   "select comments.item as item from comments
    join ranks on ranks.item = comments.item
    order by rank desc
    limit ? offset ?"
   limit offset))

(define (top (limit -1) (offset -1))
 (query-list dbc
   "select item from top
    limit ? offset ?"
   limit offset))

(define (karma user)
 (query-value dbc
   "select karma from karmas where user=?" user))

(define (descendants item)
 (query-list dbc
   "select descendant from descendants
    where item = ?" item))

(define (search term (limit -1) (offset -1))
  (sql-null->false
    (query-list dbc
      "select item from items
       where text like $1
       or title like $1
       limit $2 offset $3"
      (~a "%" term "%")
      limit offset)))

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

(define (delete-vote! user item)
  (query-exec dbc
    "delete from votes where user = ? and item = ?"
    user item))

(define (create-vote! user item direction)
  (when (votable? user item direction)
      (query-exec dbc
        "insert into votes values (?, ?, ?, ?)"
        user item direction (current-seconds))))

(define (voted user item)
  (sql-null->false
    (query-maybe-value dbc
      "select direction from votes where user=? and item=? limit 1"
      (false->sql-null user) item)))

(define (votable? user item (direction 1))
  (and (existing-user? user)
       (not (equal? (author item) user))
       (not (equal? (voted user item) direction))))

(define (score item)
  (query-value dbc
    "select score from scores where item=?"
    item))

(define (votes user)
  (query-list dbc
    "select user from votes where user=?"
    user))
