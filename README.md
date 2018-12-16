**Work in progress! Breakage guaranteed!**

# Undebatable
A [**HN**](https://news.ycombinator.com)-clone written in Racket.

## How to run
- install [**Racket**](https://racket-lang.org) version >= 7.0
- in the **undebatable** folder run `./undebatable.rkt`

## Features

* Need
  - [ ] site admin
  - [ ] change password
  - [ ] friendly error messages
  - [ ] correct redirection after action
  - [ ] collapsible comments
  - [ ] post deletion
  - [ ] search
  - [ ] notification on reply
  - [ ] limit login tries
  - [ ] prevent CSRF
  - [x] unvoting
  - [ ] password recovery
  - [x] file upload
  - [ ] download user data in machine readable format (GDPR)
  - [ ] "forget me" / profile deletion (GDPR)
  - [x] pagination
  - [x] graceful degredation (js not required)

* Want
  - [x] zero-config install
  - [x] easy backup (all state in one file)
  - [ ] a general way of listing items. (DRY)
  - [ ] proper ranking algorithm
  - [ ] tags
  - [ ] provide rss feeds
  - [ ] user profiles
  - [ ] upload expiriy
  - [ ] shop
  - [ ] multimedia gallery

* Like
  - [ ] avoid PLOP
  - [x] [numbered pagination](https://logrocket.com/blog/infinite-scroll/)
  - [ ] user styles
  - [ ] feed reader
  - [ ] geographical blogging

## Security

* [x] use prepared SQL statements to prevent injection
* [ ] escape user input before displayed
* [ ] use POST requests for state change
* [ ] require auth tokens in every POST request to prevent CSRF



## Code style
  - use SQL as much as possible (be declarative)
  - use currying when applying general functions for more specific purposes
  - use keyword arguments when there are too many of them
  - use pattern matching as much as possible

