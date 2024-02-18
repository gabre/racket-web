#lang web-server/insta
(require "blog-pages.rkt")


(define (start request)
  (post-list-page request))
