#lang typed/racket
(require typed/xml)
(require typed/web-server/http)
(require racket/match)
(require/typed
 web-server/http/bindings
 [exists-binding? (-> Symbol (Listof (Pairof Symbol String)) Boolean)]
 [request-bindings (-> Request (Listof (Pairof Symbol String)))]
 [extract-binding/single (-> Symbol (Listof (Pairof Symbol String)) String)])

(provide
 render-new-post-form
 render-posts
 render-post-details
 render-post
 post
 blog
 blog-posts
 set-blog-posts!
 bindings->post
 request->post-bindings
 blog-insert-post!
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

(define short-hr
  '(hr ((style "width:40%; margin-inline-start: 0;"))))

(define long-hr
  '(hr ((style "width:70%; margin-inline-start: 0;"))))

(: render-post-comments (-> (Listof String) XExpr))
(define (render-post-comments comments)
  (: render-comment-with-separator (-> String (Listof XExpr) (Listof XExpr)))
  (define (render-comment-with-separator comment result-so-far)
    (let
        ([rendered-comment (render-comment comment)])
      (match
          result-so-far
        [(cons _ _) (append result-so-far `(,short-hr ,rendered-comment))]
        [_          (list rendered-comment)]
        )))
  (define rendered-comments (foldl render-comment-with-separator '() comments))
  `(div
    ,@(if (empty? rendered-comments)
          '((p "/no comments/"))
          rendered-comments)))

(: render-comment (-> String XExpr))
(define (render-comment comment)
  `(p ,comment))

(: render-post-body (-> post (Option String) (Listof XExpr)))
(define (render-post-body post maybe-detailed-view-link)
  (define title (post-title post))
  (define title-tag
    (if
      maybe-detailed-view-link
      `(a ((href ,maybe-detailed-view-link)) ,title)
      (post-title post)))
  `((h2 ,title-tag)
    (p  ,(post-body post))))

(: render-post (-> post String XExpr))
(define (render-post post detailed-view-link)
  `(div
    ,@(render-post-body post detailed-view-link)))

(: render-post-details (-> post String XExpr))
(define (render-post-details post back-link)
  `(div
    ,@(render-post-body post #f)
    ,long-hr
    (p  ,(render-post-comments (post-comments post)))
    ,long-hr
    (a ((href ,back-link)) "Go back")))

(: render-posts (-> String (Listof (Pairof String post)) String XExpr))
(define (render-posts title links+posts new-post-creator-link)
  `(div
    (h1 ,title)
    ,@(map (lambda
               ([detailed-view-link+post : (Pairof String post)])
             (render-post (cdr detailed-view-link+post) (car detailed-view-link+post)))
           links+posts)
    ,long-hr
    (a ((href ,new-post-creator-link)) "Create a new post")))

(: render-new-post-form (-> String XExpr))
(define (render-new-post-form link)
  `(div
    (h2 "New Post")
    (form
     (label "Title")(br)
     (input ((type "text") (id "title") (name "title")))(br)
     (label "Text")(br)
     (input ((type "text") (id "post") (name "post")))(br)(br)
     (input ((type "submit") (value "Submit"))))(br)(br)
                                                (a ((href ,link)) "Go back")))

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