#lang typed/racket

(require typed/xml)
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
 XExprForest
 Formlet
 )

(define-type XExprForest
  (Listof XExpr))
(define-type (Formlet A)
  (-> Integer
      (Values
       XExprForest
       (-> (Listof binding:form) Any)
       Integer)))

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
  #:transparent)

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
