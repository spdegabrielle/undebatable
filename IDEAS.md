## Ideas and resources

- [SLC](https://blog.asmartbear.com/slc.html)

- toggle between anonymous posts


### builtin network games

- [phutball](https://en.wikipedia.org/wiki/Phutball)
- hnefatafl

### User styles that are sharable

- like myspace
- but just plain CSS you add in there
- has to be optional, otherwise may crash other people's browsers

### Ranking
* http://www.evanmiller.org/how-not-to-sort-by-average-rating.html
* https://medium.com/hacking-and-gonzo/how-hacker-news-ranking-algorithm-works-1d9b0cf2c08d
* https://medium.com/hacking-and-gonzo/how-reddit-ranking-algorithms-work-ef111e33d0d9
* https://redditblog.com/2009/10/15/reddits-new-comment-sorting-system/
* trending domains: https://hntrending.com/domains/year/index.html

### Other
* make things separateable by tabs, so allow for niche communities
* must!: seen/replies, like lobsters. good for old threads.
* check referrer to prevent CSRF attacks (or add an autho token). See "this post upvotes itself":
Set-Cookie: JSESSIONID=xxxxx; SameSite=Strict
https://www.owasp.org/index.php/Cross-Site_Request_Forgery_%28CSRF%29_Prevention_Cheat_Sheet
https://www.owasp.org/index.php/Cross-Site_Request_Forgery_%28CSRF%29_Prevention_Cheat_Sheets://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)
* check if referrer is from facebook or google, and if so, tell people about issues. same with chrome or internet explorer, etc.
* login-less file upload / anonymous, with file expiry based on user/item score and size of file. small files should be kept indefinitely.
* [Google Reader](http://googlereader.blogspot.com/2008/12/square-is-new-round.html)
* [Pretty buttons](https://picturepan2.github.io/spectre/elements/buttons.html)
* [Color scheme](https://refactoringui.com/previews/building-your-color-palette/)
* privacy: post is visible to list of users, or has a password? (password would work for non-logged in users)
* separate media library?// use square mosiac for that
* make gif thumbnails for galleries? instaed of mosaics
* arrows to go to next/prev item. pagination. [infinite scroll?](https://logrocket.com/blog/infinite-scroll/)
* label as 'flame' 'troll' 'clickbait' 'spam'...
* add post language (e.g. English)
* "ignore" a user, and bayesian spam filter, but no voting (click-economy like facebook)
* perhaps look into ansible / puppet / chef for deployment
* page cache and a search engine, alternative to google
* hash-tags and @username ... perhaps
* word cloud / tag graph
* voting system // petition system, for small democracy
* doodle / calendar. for appointments for event plans
* shop / swap system. perhaps some sort of "forms" system?
* shop should perhaps just be an item that shows a form to others to reply/buy/make suggested price?
* upload videos with video tag ( perhaps )
* image upload with optional geotag
* rss reader, like feedly, with recommendations / ranking. but what about offline?? -- naaaaah
* make youtube-cache app / better youtube-interface, youtube-dl-as-a-service
* perhaps use logic programming for queries
* perhaps custom forms '(form action /custrom ,@user-input) ... no - what about reply?
* [not a general purpose cms](http://hakunin.com/cms-trap)
* markdown preview button is necessary!
* no folders, just tags, for files
