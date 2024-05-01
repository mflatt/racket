#lang racket/base
(require "../common/module-path.rkt"
         "../common/contract.rkt"
         "../common/phase.rkt"
         "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "reflect-compiled.rkt")

(provide module-compiled-amalgam)

(define/who module-compiled-amalgam
  (case-lambda
    [(c)
     (check who compiled-module-expression? c)
     (cond
       [(compiled-module-amalgam-expression? c)
        (define ld (compiled->linklet-directory-or-bundle c))
        (define-values (h amalgam)
          (if (linklet-directory? ld)
              (let ([h (linklet-bundle->hash (hash-ref (linklet-directory->hash ld) #f))])
                (values h (hash-ref h 'amalgam)))
              (let ([h (linklet-bundle->hash ld)])
                (values h (hash-ref h 'amalgam)))))
        amalgam]
       [else '()])]
    [(c amalgam)
     (check who compiled-module-expression? c)
     (unless (and (list? amalgam)
                  (for/and ([name+c+phases (in-list amalgam)])
                    (and (list? name+c+phases)
                         ((length name+c+phases) . >= . 2)
                         (module-path? (car name+c+phases))
                         (or (not (cadr name+c+phases))
                             (compiled-module-expression? (cadr name+c+phases)))
                         (for/and ([phase (in-list (cddr name+c+phases))])
                           (phase? phase)))))
       (raise-argument-error who "(listof (cons/c module-path? (cons/c (or/c compiled-module-expression? #f) (listof phase?))))"
                             amalgam))
     (define ld (compiled->linklet-directory-or-bundle c))
     (let ([amalgam (for/list ([name+c+phases (in-list amalgam)])
                      (list* (car name+c+phases)
                             (and (cadr name+c+phases)
                                  (compiled->linklet-bundle who (cadr name+c+phases)))
                             (cddr name+c+phases)))])
       (cond
         [(linklet-directory? ld)
          (let* ([dh (linklet-directory->hash ld)]
                 [h (linklet-bundle->hash (hash-ref dh #f))]
                 [b (hash->linklet-bundle (hash-set h 'amalgam amalgam))])
            (hash->linklet-directory (hash-set dh #f b)))]
         [else
          (let ([h (linklet-bundle->hash ld)])
            (hash->linklet-bundle (hash-set h 'amalgam amalgam)))]))]))
