#lang web-server/insta

(require "blog-core-model.rkt")
(require "blog-pages.rkt")
(require
  (prefix-in persistence: "blog-persistence.rkt"))

(define blog-db-path
  (path->string
   (build-path (current-directory) "the-blog-data.db")))
(define blog-settings
  (settings
  "My Blog"
  (persistence:initialize-blog! blog-db-path)))

(define (start request)
  (response/xexpr
   (post-list-page blog-settings request)))

(static-files-path "static")
