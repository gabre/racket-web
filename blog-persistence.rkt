#lang typed/racket

(require
  (prefix-in core:
             (only-in "blog-core-model.rkt"
                      blog-insert-post!
                      post-insert-comment!)))
(require
  (except-in "blog-core-model.rkt"
             blog-insert-post!
             post-insert-comment!))
(require/typed
 racket/base
 [(read read-blog) (-> blog)])


(provide
 initialize-blog!
 blog-insert-post!
 post-insert-comment!
 )

(: blog-insert-post! (-> blog post Void))
(define (blog-insert-post! blog new-post)
  (core:blog-insert-post! blog new-post)
  (save-blog! blog))

(: post-insert-comment! (-> blog post post-comment Void))
(define (post-insert-comment! blog post comment)
  (core:post-insert-comment! post comment)
  (save-blog! blog))

(: initialize-blog! (-> String blog))
(define (initialize-blog! home)
  (: saved-blog-missing-exn-handler (-> Any blog))
  (define (saved-blog-missing-exn-handler exn)
    (displayln (~a "[Warning] The saved blog is missing or incorrect: " exn))
    (blog
     home
     '()))
  (: the-blog blog)
  (define the-blog
    (with-handlers ([exn? saved-blog-missing-exn-handler])
      (with-input-from-file home read-blog)))
  (set-blog-home! the-blog home)
  the-blog)

(: save-blog! (-> blog Void))
(define (save-blog! a-blog)
  (with-output-to-file (blog-home a-blog)
    (lambda () (write a-blog))
    #:exists 'replace))
