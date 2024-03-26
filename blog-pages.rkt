#lang typed/racket/shallow

(require typed/web-server/http)
(require "blog-core-model.rkt")
(require "blog-rendering.rkt")
(require/typed
 "blog-formlets.rkt"
 [new-comment-formlet (Formlet post-comment)]
 [new-post-formlet (Formlet String)])
(require
  (prefix-in persistence: "blog-persistence.rkt"))
(require/typed
 web-server/servlet/web
 [redirect/get (-> Request)]
 [send/suspend/dispatch (-> (-> (-> (-> Request Response) String) Response) Response)])
(require/typed
 web-server/formlets
 [formlet-process (All (A) (-> (Formlet A) Request (Option A)))])

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
      (for/list ([post (persistence:blog-posts (settings-blog settings))]) (post->link+post embed/url post))
      (embed/url (lambda (r) (post-creator-page settings #f r))))))
  (send/suspend/dispatch response-generator))

(: post-details-page (-> settings post Request Response))
(define (post-details-page settings post _request)
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-post-details
      (settings-blog-main-title settings)
      post
      (embed/url (lambda (r) (confirm-comment-page settings post r)))
      (embed/url (curry post-list-page settings)))))
  (send/suspend/dispatch response-generator))

(: confirm-comment-page (-> settings post Request Response))
(define (confirm-comment-page settings post request)
  (: handle-add-comment-then-post-details-page (-> post-comment Request Response))
  (define (handle-add-comment-then-post-details-page comment _)
    (persistence:post-insert-comment! (settings-blog settings) post comment)
    (post-details-page settings post (redirect/get)))
  (: handle-no-comment-then-post-details-page (-> Request Response))
  (define (handle-no-comment-then-post-details-page _)
    (post-details-page settings post (redirect/get)))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (let ([maybe-parsed-comment ((inst formlet-process post-comment) new-comment-formlet request)])
       (cond
         [maybe-parsed-comment
          (render-comment-comfirmantion
           (settings-blog-main-title settings)
           maybe-parsed-comment
           (embed/url (curry handle-add-comment-then-post-details-page maybe-parsed-comment))
           (embed/url handle-no-comment-then-post-details-page))]
         [else (error "Unable to parse the comment.")]))))
  (send/suspend/dispatch response-generator))

(: post-creator-page (-> settings (Option post-title+body) Request Response))
(define (post-creator-page settings maybe-previous-new-post _request)
  (: handle-add-post-then-post-creator-page (-> Request Response))
  (define (handle-add-post-then-post-creator-page request)
    (let ([maybe-previous-new-post ((inst formlet-process post-title+body) new-post-formlet request)])
      (when maybe-previous-new-post
        (persistence:blog-insert-post! (settings-blog settings) maybe-previous-new-post))
      (post-creator-page settings maybe-previous-new-post (redirect/get))))
  (: response-generator (-> (-> (-> Request Response) String) Response))
  (define (response-generator embed/url)
    (response/xexpr
     (render-new-post-form
      (settings-blog-main-title settings)
      (embed/url handle-add-post-then-post-creator-page)
      (embed/url (curry post-list-page settings))
      maybe-previous-new-post)))
  (send/suspend/dispatch response-generator))
