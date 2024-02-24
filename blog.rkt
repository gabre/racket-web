#lang web-server/insta

(require "blog-core-model.rkt")
(require "blog-pages.rkt")

(define the-blog
  (blog
   (list
    (post "My first post" "Under Construction"
          (list
            (post-comment "Someone" "No.")
            (post-comment "User123" "Are you there?")))
    (post "Second post" "I don't know yet." '())
    (post "Third post" "3rd post: still empty." '()))))

(define blog-settings (settings "My Blog" the-blog))

(define (start request)
  (post-list-page blog-settings request))
