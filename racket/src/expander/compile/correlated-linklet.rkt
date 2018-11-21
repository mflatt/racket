#lang racket/base
(require racket/fasl
         "../host/linklet.rkt"
         "../host/correlate.rkt"
         "write-linklet.rkt"
         "read-linklet.rkt")

(provide (struct-out correlated-linklet)
         make-correlated-linklet

         hash->correlated-linklet-directory
         hash->correlated-linklet-bundle

         linklet-directory*?
         linklet-bundle*?

         linklet-directory*->hash
         linklet-bundle*->hash

         hash->linklet-directory*
         hash->linklet-bundle*

         read-compiled-linklet-or-correlated-linklet)

;; A "compiled" linklet that's still in correlated (i.e., S-expression
;; with source locations and properties) form, but also caches a
;; compiled/evaled form
(struct correlated-linklet (expr [compiled #:mutable]))

(define (make-correlated-linklet expr)
  (correlated-linklet expr #f))

;; ----------------------------------------

(define vm-bytes #"linklet")

(struct correlated-linklet-directory (ht)
  #:property prop:custom-write (lambda (ld port mode)
                                 (write-linklet-directory ld
                                                          correlated-linklet-directory-ht
                                                          vm-bytes
                                                          write-correlated-linklet-bundle
                                                          port)))

(struct correlated-linklet-bundle (ht)
  #:property prop:custom-write (lambda (b port mode)
                                 (write-linklet-bundle b
                                                       vm-bytes
                                                       write-correlated-linklet-bundle
                                                       port)))

(define (hash->correlated-linklet-directory ht)
  (correlated-linklet-directory ht))

(define (hash->correlated-linklet-bundle ht)
  (correlated-linklet-bundle ht))

(define (linklet-directory*? v)
  (or (linklet-directory? v)
      (correlated-linklet-directory? v)))
      
(define (linklet-bundle*? v)
  (or (linklet-bundle? v)
      (correlated-linklet-bundle? v)))

(define (linklet-directory*->hash ld)
  (if (linklet-directory? ld)
      (linklet-directory->hash ld)
      (correlated-linklet-directory-ht ld)))
  
(define (linklet-bundle*->hash lb)
  (if (linklet-bundle? lb)
      (linklet-bundle->hash lb)
      (correlated-linklet-bundle-ht lb)))

(define (hash->linklet-directory* ld ht)
  (if (linklet-directory? ld)
      (hash->linklet-directory ht)
      (hash->correlated-linklet-directory ht)))

(define (hash->linklet-bundle* ld ht)
  (if (linklet-bundle? ld)
      (hash->linklet-bundle ht)
      (hash->correlated-linklet-bundle ht)))

;; ----------------------------------------

(define (read-compiled-linklet-or-correlated-linklet in)
  ;; `#~` has already been read
  (define vers-len (min 63 (peek-byte in)))
  (define vers (peek-bytes vers-len 1 in))
  (define vm-len (min 63 (peek-byte in (+ 1 vers-len))))
  (define vm (peek-bytes vm-len (+ 2 vers-len) in))
  (cond
    [(equal? vm vm-bytes)
     (read-linklet-bundle-or-directory in
                                       vm-bytes
                                       hash->correlated-linklet-directory
                                       read-correlated-linklet-hash
                                       hash->correlated-linklet-bundle)]
    [else
     (read-compiled-linklet in)]))

;; ----------------------------------------

(define (write-correlated-linklet-bundle b)
  (s-exp->fasl (->faslable (correlated-linklet-bundle-ht b))))

(struct faslable-correlated (e source position line column span name)
  #:prefab)
(struct faslable-correlated-linklet (expr)
  #:prefab)

(define (->faslable v)
  (cond
    [(pair? v)
     (define a (->faslable (car v)))
     (define d (->faslable (cdr v)))
     (if (and (eq? a (car v))
              (eq? d (cdr v)))
         v
         (cons a d))]
    [(correlated? v)
     (faslable-correlated
      (->faslable (correlated-e v))
      (correlated-source v)
      (correlated-position v)
      (correlated-line v)
      (correlated-column v)
      (correlated-span v)
      (correlated-property v 'inferred-name))]
    [(hash? v)
     (cond
       [(hash-eq? v)
        (for/hasheq ([(key value) (in-hash v)])
          (values (->faslable key) (->faslable value)))]
       [(hash-eqv? v)
        (for/hasheqv ([(key value) (in-hash v)])
          (values (->faslable key) (->faslable value)))]
       [else
        (for/hash ([(key value) (in-hash v)])
          (values (->faslable key) (->faslable value)))])]
    [(correlated-linklet? v)
     (faslable-correlated-linklet (->faslable (correlated-linklet-expr v)))]
    [else v]))

;; ----------------------------------------

(define (read-correlated-linklet-hash in)
  (faslable-> (fasl->s-exp in)))

(define (faslable-> v)
  (cond
    [(pair? v)
     (define a (faslable-> (car v)))
     (define d (faslable-> (cdr v)))
     (if (and (eq? a (car v))
              (eq? d (cdr v)))
         v
         (cons a d))]
    [(faslable-correlated? v)
     (define name (faslable-correlated-name v))
     (define c (datum->correlated (faslable-> (faslable-correlated-e v))
                                  (vector
                                   (faslable-correlated-source v)
                                   (faslable-correlated-line v)
                                   (faslable-correlated-column v)
                                   (faslable-correlated-position v)
                                   (faslable-correlated-span v))))
     (if name
         (correlated-property c 'inferred-name name)
         c)]
    [(hash? v)
     (cond
       [(hash-eq? v)
        (for/hasheq ([(key value) (in-hash v)])
          (values (faslable-> key) (faslable-> value)))]
       [(hash-eqv? v)
        (for/hasheqv ([(key value) (in-hash v)])
          (values (faslable-> key) (faslable-> value)))]
       [else
        (for/hash ([(key value) (in-hash v)])
          (values (faslable-> key) (faslable-> value)))])]
    [(faslable-correlated-linklet? v)
     (make-correlated-linklet (faslable-> (faslable-correlated-linklet-expr v)))]
    [else v]))
