**Work in progress! Breakage guaranteed!**

# Undebatable
A web forum written in Racket.

## How to run
- install [**Racket**](https://racket-lang.org) version >= 7.0
- in the **undebatable** folder run `racket undebatable.rkt`

## Features
Many of [HN](https://news.ycombinator.com)'s features and then:
- [numbered pagination](https://logrocket.com/blog/infinite-scroll/)
- file sharing

## Design principles
- graceful degredation (js not required)
- simple, light, minimal, fast
- zero-config install
- easy backup (all state in one file)
- as much SQL as possible
- <s>avoid PLOP when possible</s> (not easy in SQL)

## Security

* user input escaped when displayed
* POST request for state change
* auth tokens in POST requests to prevent CSRF

## TODO
- better error messages
- correct redirection after certain actions
- a general way of listing items. (DRY)
  * general functions with lots of keyword args that are then curried when specificity is needed. seems like a good idea
- deletion
- user styles
- proper ranking algorithm
- feeds (should appear as normal items)
- tags
- search
- notification on reply
- user profiles
- limit login tries
- prevent CSRF
- unvoting
- upload expiriy
- password recovery
- GDPR
  * download user data in machine readable format
  * "forget me" / profile deletion
