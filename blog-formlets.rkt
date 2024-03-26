#lang racket

(require web-server/formlets/syntax)
(require web-server/formlets/input)
(require "blog-core-model.rkt")

(provide
 new-post-formlet
 new-comment-formlet
 )

(define (if-all-non-empty f . items)
  (if
   (andmap non-empty-string? items)
   (apply f items)
   #f
   ))

(define new-post-formlet
  (formlet
   (#%# (label "Title")(br)
        ,{input-string . => . title}
        (br)(br)
        (label "Text")(br)
        ,{input-string . => . body}
        (br)(br))
   (if-all-non-empty post-title+body title body)))

(define new-comment-formlet
  (formlet
   (#%# (label "Username") nbsp
        ,{input-string . => . author}
        (br)
        (label "Content") nbsp
        ,{input-string . => . content}
        (br)(br))
   (if-all-non-empty post-comment author content)))