#lang typed/racket

(require typed/web-server/http)
(require "blog-core-model.rkt")
(require "blog-rendering.rkt")
(require/typed
 web-server/servlet/web
 [send/suspend/dispatch (-> (-> (-> (-> Request Response) String) Response) Response)])

(provide post-list-page)

(: post-list-page (-> settings Request Response))
(define (post-list-page settings _request)
  (: post->link+post (-> (-> (-> Request Response) String) post (Pairof String post)))
  (define (post->link+post embed/url post)
    (cons (embed/url (lambda (r) (post-details-page settings post r))) post))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-posts
      (settings-blog-main-title settings)
      (for/list ([post (blog-posts (settings-blog settings))]) (post->link+post embed/url post))
      (embed/url (curry post-creator-page settings)))))
  (send/suspend/dispatch response-generator))

(: post-details-page (-> settings post Request Response))
(define (post-details-page settings post _request)
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-post-details
      post
      (embed/url (lambda (r) (confirm-comment-page settings post r)))
      (embed/url (curry post-list-page settings)))))
  (send/suspend/dispatch response-generator))

(: confirm-comment-page (-> settings post Request Response))
(define (confirm-comment-page settings post request)
  (: add-comment-then-post-details-page (-> String Request Response))
  (define (add-comment-then-post-details-page comment request)
    (post-insert-comment! post comment)
    (post-details-page settings post request))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (let ([maybe-parsed-comment (request->comment request)])
       (cond
         [maybe-parsed-comment
          (render-comment-comfirmantion
           maybe-parsed-comment
           (embed/url (curry add-comment-then-post-details-page maybe-parsed-comment))
           (embed/url (lambda (r) (post-details-page settings post r))))]))))
  (send/suspend/dispatch response-generator))

(: post-creator-page (-> settings Request Response))
(define (post-creator-page settings request)
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (let ([maybe-parsed-bindings (request->post-bindings request)])
       (cond
         [maybe-parsed-bindings
          (let
              ([new-post (bindings->post maybe-parsed-bindings)])
            (blog-insert-post! (settings-blog settings) new-post))])
       (render-new-post-form (embed/url (curry post-list-page settings))))))
  (send/suspend/dispatch response-generator))
