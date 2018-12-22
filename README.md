# Undebatable
A [**HN**](https://news.ycombinator.com)-like written in Racket.

**Not yet stable! But feel free to try it out.**

## How to run
- install [**Racket**](https://racket-lang.org) version >= 7.0
- in the **undebatable** folder run `./undebatable.rkt`

## Feature development

### Need
- [ ] site admin
- [ ] change password
- [ ] friendly error messages
- [ ] correct redirection after action
- [ ] collapsible comments
- [ ] post deletion
- [x] search
- [ ] threads participated in
- [ ] notification on reply
- [ ] limit login tries
- [x] unvoting
- [ ] password recovery
- [x] file upload
- [ ] download user data in machine readable format (GDPR)
- [ ] "forget me" / profile deletion (GDPR)
- [x] pagination
- [x] graceful degredation (js not required)
- [ ] sockpuppet detection
- [ ] spam filter
- [ ] bazar / shop, but without paypal
- [ ] event planner (doodle-like)
- [ ] title length limit

### Want
- [ ] dupe detection
- [x] zero-config install
- [x] easy backup (all state in one file)
- [ ] a general way of listing items. (DRY)
- [ ] proper ranking algorithm
- [ ] tags
- [ ] provide rss feeds
- [ ] user profiles
- [ ] upload expiriy
- [ ] anonymous file upload (with shorter expiry times, perhaps)
- [ ] multimedia gallery
- [ ] closed threads (via password or via user list? probably user list)

### Like
  - [ ] advanced search
  - [ ] optional *filter bubble view*, e.g. things similar to what you have upvoted before (or dissimilar to downvoted items)
  - [ ] avoid PLOP
  - [x] [numbered pagination](https://logrocket.com/blog/infinite-scroll/)
  - [ ] user styles
  - [ ] feed reader
  - [ ] geographical blogging


## Security
* [x] use prepared SQL statements to prevent injection
* [ ] escape user input before displayed
* [ ] use POST requests for state change (is *login* relevant here?)
* [x] require auth tokens in every POST request to prevent CSRF


## Code style
- use SQL as much as possible (be declarative)
- use currying when applying general functions for more specific purposes
- use keyword arguments when there are too many of them
- use pattern matching as much as possible

