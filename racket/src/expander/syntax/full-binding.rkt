#lang racket/base
(require "../compile/serialize-property.rkt")

(provide (struct-out full-binding)
         binding-frame-id
         binding-free=id
         binding-const-stx)

;; A base struct for bindings with a frame identity or
;; `free-identifier=?` equivalence
(struct full-binding (frame-id   ; used to trigger use-site scopes
                      free=id    ; `free-identifier=?` equivalence via a rename-transformer binding
                      const-stx) ; constant-transformer binding: syntax (or fixnum key in provides)
  #:authentic
  #:property prop:binding-reach-scopes
  (lambda (b)
    (or (binding-free=id b)
        (binding-const-stx b))))

(define (binding-frame-id b)
  (and (full-binding? b)
       (full-binding-frame-id b)))

(define (binding-free=id b)
  (and (full-binding? b)
       (full-binding-free=id b)))

(define (binding-const-stx b)
  (and (full-binding? b)
       (full-binding-const-stx b)))
