#lang racket/base
(require "full-binding.rkt"
         "../compile/serialize-property.rkt")

(provide make-local-binding
         local-binding-update
         local-binding?
         
         local-binding-key

         deserialize-full-local-binding)

(define (local-binding? b)
  ;; must not overlap with `module-binding?`
  (or (full-local-binding? b)
      (symbol? b)))

;; Represent a local binding with a key, where the value of
;; the key is kept in a separate environment. That indirection
;; ensures that a fully expanded program doesn't reference
;; compile-time values from local bindings, but it records that
;; the binding was local. The `frame-id` field is used to
;; trigger use-site scopes as needed
(struct full-local-binding full-binding (key)
  #:authentic
  #:property prop:serialize
  (lambda (b ser-push! state)
    ;; Data that is interpreted by the deserializer:
    (ser-push! 'tag '#:local-binding)
    (ser-push! (full-local-binding-key b))
    (ser-push! (full-binding-free=id b))
    (ser-push! (full-binding-const-stx b))))

(define (deserialize-full-local-binding key free=id const-stx)
  (full-local-binding #f free=id const-stx key))

(define (make-local-binding key
                            #:frame-id [frame-id #f]
                            #:free=id [free=id #f]
                            #:const-stx [const-stx #f])
  (cond
   [(and (not frame-id)
         (not free=id)
         (not const-stx))
    key]
   [else
    (full-local-binding frame-id free=id const-stx key)]))

(define (local-binding-update b
                              #:key [key (local-binding-key b)]
                              #:frame-id [frame-id (binding-frame-id b)]
                              #:free=id [free=id (binding-free=id b)]
                              #:const-stx [const-stx (binding-const-stx b)])
  (make-local-binding key
                      #:frame-id frame-id
                      #:free=id free=id
                      #:const-stx const-stx))

(define (local-binding-key b)
  (if (full-local-binding? b)
      (full-local-binding-key b)
      b))
