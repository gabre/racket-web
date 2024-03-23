#lang typed/racket

(require typed/web-server/http)
(require typed/db)
(require/typed
 web-server/http/bindings
 [exists-binding? (-> Symbol (Listof (Pairof Symbol String)) Boolean)]
 [request-bindings (-> Request (Listof (Pairof Symbol String)))]
 [extract-binding/single (-> Symbol (Listof (Pairof Symbol String)) String)])

(provide
 settings
 settings-blog-main-title
 settings-blog
 post-title+body
 post-title+body-title
 post-title+body-body
 post-comment
 post-comment-author
 post-comment-content
 post
 post-id
 post-blog
 blog
 blog-db
 bindings->post
 request->post-bindings
 request->comment
 )

(struct post-comment
  (
   [author : String]
   [content : String]
   )
  )

(struct post
  (
   [blog : blog]
   [id : Integer]
   )
  )

(struct post-title+body
  (
   [title : String]
   [body : String]
   )
  )

(struct blog
  (
   [db : Connection]
   )
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

(: bindings->post (-> (Listof (Pairof Symbol String)) post-title+body))
(define (bindings->post bindings)
  (post-title+body
   (extract-binding/single 'title bindings)
   (extract-binding/single 'post bindings)))

(: request->comment (-> Request (Option post-comment)))
(define (request->comment request)
  (let
      ([bindings (request-bindings request)])
    (if
     (and
      (exists-binding? 'author bindings)
      (exists-binding? 'comment bindings))
     (post-comment
      (extract-binding/single 'author bindings)
      (extract-binding/single 'comment bindings)
      )
     #f)))
