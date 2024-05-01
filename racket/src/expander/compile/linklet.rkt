#lang racket/base
(require "../common/contract.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../host/linklet.rkt"
         "write-linklet.rkt"
         "correlated-linklet.rkt")

(provide linklet-directory?
         linklet-bundle?

         hash->linklet-directory
         hash->linklet-bundle

         linklet-directory->hash
         linklet-bundle->hash)

(struct linklet-directory (ht)
  #:property prop:custom-write (lambda (ld port mode)
                                 (write-linklet-directory ld
                                                          (correlated-linklet-directory? ld)
                                                          linklet-directory->hash
                                                          linklet-bundle->hash
                                                          port)))

(struct linklet-bundle (ht)
  #:property prop:custom-write (lambda (b port mode)
                                 (write-linklet-bundle b
                                                       (correlated-linklet-bundle? b)
                                                       linklet-bundle->hash
                                                       port)))

(define/who (hash->linklet-directory ht)
  (check who (lambda (ht)
               (and (not (impersonator? ht))
                    (hash? ht)
                    (immutable? ht)
                    (hash-eq? ht)))
         #:contract "(and/c hash? hash-eq? immutable? (not/c impersonator?))"
         ht)
  (for ([(k v) (in-hash ht)])
    (cond
      [(not k)
       (unless (linklet-bundle? v)
         (raise-arguments-error who
                                "value for #f key is not a linklet bundle"
                                "value" v))]
      [(symbol? k)
       (unless (linklet-directory? v)
         (raise-arguments-error who
                                "value for symbol key is not a linklet directory"
                                "value" v))]
      [else
       (raise-arguments-error who
                              "key in given hash is not #f or a symbol"
                              "key" k)]))
  (linklet-directory ht))

(define/who (hash->linklet-bundle ht [recur-amalgam? #f])
  (check who (lambda (ht)
               (and (not (impersonator? ht))
                    (hash? ht)
                    (immutable? ht)
                    (hash-eq? ht)))
         #:contract "(and/c hash? hash-eq? immutable? (not/c impersonator?))"
         ht)
  (for ([k (in-hash-keys ht)])
    (unless (or (symbol? k) (fixnum? k))
      (raise-arguments-error who
                             "key in given hash is not a symbol or fixnum"
                             "key" k)))
  (define a-ht
    (cond
      [(and recur-amalgam?
            (hash-ref ht 'amalgam #f))
       => (lambda (amalgam)
            (unless (and (list? amalgam)
                         (for/and ([p (in-list amalgam)])
                           (and (list? p)
                                ((length p) . >= . 2)
                                (module-path? (car p))
                                (for/and ([ph (in-list (cddr p))])
                                  (phase? ph)))))
              (raise-arguments-error who
                                     "value for amalgam in given hash is not a list of lists with module paths and phases"
                                     "alamgam" amalgam))
            (define amalgam-bundles
              (for/list ([p (in-list amalgam)])
                (if (cadr p)
                    (list* (car p)
                           (hash->linklet-bundle (cadr p))
                           (cddr p))
                    p)))
            (hash-set ht 'amalgam amalgam-bundles))]
      [else ht]))
  (linklet-bundle a-ht))

(define/who (linklet-directory->hash ld [recur-amalgam? #f])
  (check who linklet-directory? ld)
  (linklet-directory-ht ld))
  
(define/who (linklet-bundle->hash ld [recur-amalgam? #f])
  (check who linklet-bundle? ld)
  (define ht (linklet-bundle-ht ld))
  (cond
    [(and recur-amalgam?
          (hash-ref ht 'amalgam #f))
     => (lambda (amalgam)
          (define amalgam-hts
            (for/list ([name+bundle+phases (in-list amalgam)])
              (if (cadr name+bundle+phases)
                  (list* (car name+bundle+phases)
                         (linklet-bundle->hash (cadr name+bundle+phases))
                         (cddr name+bundle+phases))
                  name+bundle+phases)))
          (hash-set ht 'amalgam amalgam-hts))]
    [else ht]))

;; ----------------------------------------

;; If there are no values that satisfy `linklet?`, then
;; assume that we have `correlated-linklet?` values.

(define (correlated-linklet-directory? ld)
  (for/and ([(k v) (in-hash (linklet-directory->hash ld))])
    (cond
      [(not k) (correlated-linklet-bundle? v)]
      [(symbol? k) (correlated-linklet-directory? v)]
      [else #t])))
      
(define (correlated-linklet-bundle? b)
  (for/and ([(k v) (in-hash (linklet-bundle->hash b))])
    (not (linklet? v))))
