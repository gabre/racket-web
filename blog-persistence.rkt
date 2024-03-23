#lang typed/racket

; (require
;   (prefix-in core:
;              (only-in "blog-core-model.rkt"
;                       blog-insert-post!
;                       post-insert-comment!)))
(require "blog-core-model.rkt")
(require
  typed/db)
(require/typed
 racket/base
 [(read read-blog) (-> blog)])

(provide
 initialize-blog!
 blog-insert-post!
 post-insert-comment!
 post-title
 post-body
 post-comments
 blog-posts
 )

(: initialize-blog! (-> String blog))
(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
                (string-append
                 "CREATE TABLE posts "
                 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)")))
  (unless (table-exists? db "comments")
    (query-exec db
                "CREATE TABLE comments (postid INTEGER, author TEXT, content TEXT)")
    )
  the-blog)

(: blog-insert-post! (-> blog post-title+body Void))
(define (blog-insert-post! a-blog a-post-title+body)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO posts (title, body) VALUES (?, ?)"
   (post-title+body-title a-post-title+body) (post-title+body-body a-post-title+body)))

(: post-insert-comment! (-> blog post post-comment Void))
(define (post-insert-comment! a-blog post comment)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO comments (postid, author, content) VALUES (?, ?, ?)"
   (post-id post) (post-comment-author comment) (post-comment-content comment)))

(: blog-posts (-> blog (Listof post)))
(define (blog-posts a-blog)
  (define (id->post an-id)
    (post a-blog (cast an-id Integer)))
  (map id->post
       (query-list
        (blog-db a-blog)
        "SELECT id FROM posts")))

(: post-title (-> post String))
(define (post-title a-post)
  (cast (query-value
         (blog-db (post-blog a-post))
         "SELECT title FROM posts WHERE id = ?"
         (post-id a-post)) String))

(: post-body (-> post String))
(define (post-body a-post)
  (cast (query-value
         (blog-db (post-blog a-post))
         "SELECT body FROM posts WHERE id = ?"
         (post-id a-post)) String))

(: post-comments (-> post (Listof post-comment)))
(define (post-comments a-post)
  (define (row->comment row)
    (match row
      [(vector _pid a c) (post-comment (cast a String) (cast c String))]))
  (map row->comment
       (query-rows
        (blog-db (post-blog a-post))
        "SELECT * FROM comments WHERE postid = ?"
        (post-id a-post))))
