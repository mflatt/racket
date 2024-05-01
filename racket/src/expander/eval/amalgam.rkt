#lang racket/base
(require "../common/module-path.rkt"
         "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "module.rkt"
         "reflect-compiled.rkt")

(provide eval-module-amalgam)

(define (eval-module-amalgam c #:namespace ns)
  (define ld (compiled->linklet-directory-or-bundle c))
  (define-values (h amalgam)
    (if (linklet-directory? ld)
        (let ([h (linklet-bundle->hash (hash-ref (linklet-directory->hash ld) #f))])
          (values h (hash-ref h 'amalgam)))
        (let ([h (linklet-bundle->hash ld)])
          (values h (hash-ref h 'amalgam)))))
  (define amalgam-name (substitute-module-declare-name (hash-ref h 'name 'module)))
  (define amalgam-source-path (let ([src (current-module-declare-source)])
                                (and src
                                     (let-values ([(base name dir?) (split-path src)])
                                       (and (path? base) base)))))
  (define amalgam-parts
    (for/list ([name+c+phases (in-list amalgam)])
      (define name (car name+c+phases))
      (define c (cadr name+c+phases))
      (parameterize ([current-module-declare-name (module-path-index-resolve
                                                   (module-path-index-join name amalgam-name))]
                     [current-module-declare-source #f])
        (when c
          (eval-module c
                       #:namespace ns
                       #:amalgam-name amalgam-name)))
      (list* name (and c #t) (cddr name+c+phases))))
  (eval-module c
               #:namespace ns
               #:amalgam-parts amalgam-parts))
