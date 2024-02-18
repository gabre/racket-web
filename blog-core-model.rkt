#lang typed/racket

(require typed/web-server/http)
(require/typed
 web-server/http/bindings
 [exists-binding? (-> Symbol (Listof (Pairof Symbol String)) Boolean)]
 [request-bindings (-> Request (Listof (Pairof Symbol String)))]
 [extract-binding/single (-> Symbol (Listof (Pairof Symbol String)) String)])

(provide
 settings
 settings-blog-main-title
 settings-blog
 post
 post-title
 post-body
 post-comments
 post-insert-comment!
 blog
 blog-posts
 blog-insert-post!
 set-blog-posts!
 bindings->post
 request->post-bindings
 request->comment
 )

(struct post
  (
   [title : String]
   [body : String]
   [comments : (Listof String)]
   )
  #:mutable
  )

(struct blog
  (
   [posts : (Listof post)]
   )
  #:mutable
  )

(struct settings
  (
   [blog-main-title : String]
   [blog : blog]
   )
  )

(: request->post-bindings (-> Request (Option (Listof (Pairof Symbol String)))))
(define (request->post-bindings request)
  (let
      ([bindings (request-bindings request)])
    (if
     (and
      (exists-binding? 'title bindings)
      (exists-binding? 'post bindings))
     bindings
     #f)))

(: bindings->post (-> (Listof (Pairof Symbol String)) post))
(define (bindings->post bindings)
  (post
   (extract-binding/single 'title bindings)
   (extract-binding/single 'post bindings)
   (list)))

(: request->comment (-> Request (Option String)))
(define (request->comment request)
  (let
      ([bindings (request-bindings request)])
    (if
     (exists-binding? 'comment bindings)
     (extract-binding/single 'comment bindings)
     #f)))

(: blog-insert-post! (-> blog post Void))
(define (blog-insert-post! blog new-post)
  (set-blog-posts!
   blog
   (cons
    new-post
    (blog-posts blog))))

(: post-insert-comment! (-> post String Void))
(define (post-insert-comment! post comment)
  (set-post-comments! post (cons comment (post-comments post))))