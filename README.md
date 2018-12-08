**Work in progress! Breakage guaranteed!**

# Undebatable
A web forum written in Racket.

## How to run
- install [**Racket**](https://racket-lang.org) version >= 7.0
- run `./undebatable`

## Design goals
My goal is to make a simple hybrid of HN/Lobsters and Feedly/Google Reader and DropBox/WeTransfer/Google Drive/OneDrive.

- graceful degredation (work fine w/o js)
- simple, light, minimal, fast
- zero-config install
- easy backup (all state in one file)
- as much SQL as possible
- no state change through GET requests
- avoid PLOP when possible and convenient (Note: SQL is rather PLOP)

## TODO
- anti-anti-social-media top buttons
- deletion
- user styles
- pagination
- proper ranking algorithm
- feeds (should appear as normal items)
- tags
- search
- user profiles
- limit login tries
- prevent CSRF
- unvoting
- upload expiriy
- password recovery
- GDPR
  * download user data in machine readable format
  * "forget me" / profile deletion
