#lang typed/racket

(require typed/web-server/http)
(require "blog-core-model.rkt")
(require "blog-rendering.rkt")
(require/typed
 web-server/servlet/web
 [send/suspend/dispatch (-> (-> (-> (-> Request Response) String) Response) Response)])

(provide post-list-page)

(define blog-main-title "My Blog")

(define BLOG
  (blog
   (list
    (post "My first post" "Under Construction"
          '("No." "Are you there?"))
    (post "Second post" "I don't know yet." '())
    (post "Third post" "3rd post: still empty." '()))))

(: post-list-page (-> Request Response))
(define (post-list-page _request)
  (: post->link+post (-> (-> (-> Request Response) String) post (Pairof String post)))
  (define (post->link+post embed/url post)
    (cons (embed/url (curry post-details-page post)) post))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-posts
      blog-main-title
      (for/list ([post (blog-posts BLOG)]) (post->link+post embed/url post))
      (embed/url post-creator-page))))
  (send/suspend/dispatch response-generator))

(: post-details-page (-> post Request Response))
(define (post-details-page post _request)
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-post-details
      post
      (embed/url (curry confirm-comment-page post))
      (embed/url post-list-page))))
  (send/suspend/dispatch response-generator))

(: confirm-comment-page (-> post Request Response))
(define (confirm-comment-page post request)
  (: add-comment-then-post-details-page (-> String Request Response))
  (define (add-comment-then-post-details-page comment request)
    (post-insert-comment! post comment)
    (post-details-page post request))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (let ([maybe-parsed-comment (request->comment request)])
       (cond
         [maybe-parsed-comment
          (render-comment-comfirmantion
           maybe-parsed-comment
           (embed/url (curry add-comment-then-post-details-page maybe-parsed-comment))
           (embed/url (curry post-details-page post)))]))))
  (send/suspend/dispatch response-generator))

(: post-creator-page (-> Request Response))
(define (post-creator-page request)
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (let ([maybe-parsed-bindings (request->post-bindings request)])
       (cond
         [maybe-parsed-bindings
          (let
              ([new-post (bindings->post maybe-parsed-bindings)])
            (blog-insert-post! BLOG new-post))])
       (render-new-post-form (embed/url post-list-page)))))
  (send/suspend/dispatch response-generator))
