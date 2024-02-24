#lang typed/racket

(require typed/xml)
(require racket/match)
(require "blog-core-model.rkt")

(provide
 render-new-post-form
 render-posts
 render-post-details
 render-post
 render-comment-comfirmantion
 )

(define short-hr
  '(hr ((style "width:40%; margin-inline-start: 0;"))))

(define long-hr
  '(hr ((style "width:70%; margin-inline-start: 0;"))))

(: render-post-comments (-> (Listof post-comment) XExpr))
(define (render-post-comments comments)
  (: render-comment-with-separator (-> post-comment (Listof XExpr) (Listof XExpr)))
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

(: render-comment (-> post-comment XExpr))
(define (render-comment comment)
  `(p (b ,(post-comment-author comment)) ": " ,(post-comment-content comment)))

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

(: render-post-details (-> post String String XExpr))
(define (render-post-details post confirm-page-link back-link)
  `(div
    ,@(render-post-body post #f)
    ,long-hr
    (p  ,(render-post-comments (post-comments post)))
    ,long-hr
    (form
     ((action ,confirm-page-link))
     (label "Add a new comment:")(br)
     (label "Username") nbsp (input ((type "text") (id "author") (name "author")))(br)
     (label "Content") nbsp (input ((type "text") (id "comment") (name "comment")))(br)
     (input ((type "submit") (value "Submit"))))
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

(: render-comment-comfirmantion (-> post-comment String String XExpr))
(define (render-comment-comfirmantion comment yes-link no-link)
  `(div
    (h3 "Do you really want to submit the following comment?")
    (p ,(post-comment-content comment))(br)
    (a ((href ,yes-link)) "Yes")(br)
    (a ((href ,no-link)) "No")(br)))
