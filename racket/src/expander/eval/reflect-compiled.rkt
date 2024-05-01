#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "../common/contract.rkt"
         "../namespace/provided.rkt"
         "../namespace/provide-for-api.rkt")

(provide compiled-expression?
         compiled-module-expression?
         compiled-module-amalgam-expression?

         compiled->linklet-directory-or-bundle
         normalize-to-linklet-directory
         compiled->linklet-directory-or-bundle
         compiled->linklet-bundle)

(define (compiled-expression? c)
  (or (compiled-in-memory? c)
      (linklet-directory? c)
      (linklet-bundle? c)))

(define (compiled-module-or-amalgam-expression? c key)
  (define ld (compiled->linklet-directory-or-bundle c))
  (or (and (linklet-directory? ld)
           (let ([b (hash-ref (linklet-directory->hash ld) #f #f)])
             (and b (hash-ref (linklet-bundle->hash b) key #f)))
           #t)
      (and (linklet-bundle? ld)
           (hash-ref (linklet-bundle->hash ld) key #f)
           #t)))

(define (compiled-module-expression? c)
  (compiled-module-or-amalgam-expression? c 'decl))

(define (compiled-module-amalgam-expression? c)
  (compiled-module-or-amalgam-expression? c 'amalgam))

;; ----------------------------------------

(define (compiled->linklet-directory-or-bundle c)
  (if (compiled-in-memory? c)
      (compiled-in-memory-linklet-directory c)
      c))

(define (compiled->linklet-bundle who c)
  (define ld (if (compiled-in-memory? c)
                 (compiled-in-memory-linklet-directory c)
                 c))
  (cond
    [(linklet-bundle? ld)
     ld]
    [else
     (define lh (linklet-directory->hash ld))
     (or (and (eqv? 1 (hash-count lh))
              (hash-ref lh #f #f))
         (raise-arguments-error who "compiled module for amalgam has submodules"))]))

;; Normalize a compiled module that may have no submodules and is
;; represented directy by a linklet bundle to a representation that
;; uses a linklet directory
(define (normalize-to-linklet-directory c)
  (cond
   [(linklet-directory? (compiled->linklet-directory-or-bundle c))
    ;; already in linklet-directory form:
    c]
   [(linklet-bundle? c)
    (hash->linklet-directory (hasheq #f c))]
   [else
    (struct-copy compiled-in-memory c
                 [linklet-directory (normalize-to-linklet-directory
                                     (compiled-in-memory-linklet-directory c))])]))
