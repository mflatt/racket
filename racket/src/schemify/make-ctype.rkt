#lang racket/base
(require "wrap.rkt"
         "known.rkt")

(provide make-ctype?/rep)

(define (make-ctype?/rep v prim-knowns knowns mutated)
  (and (wrap-pair? v)
       (let ([u-rator (unwrap (wrap-car v))])
         (and (or (eq? u-rator 'make-ctype)
                  (eq? u-rator 'assert-ctype))
              (wrap-pair? (wrap-cdr v))
              (let ([u-arg (unwrap (wrap-car (wrap-cdr v)))])
                (and (symbol? u-arg)
                     (let ([k (or (hash-ref prim-knowns u-arg #f)
                                  (hash-ref knowns u-arg #f))])
                       (and (known-ctype? k)
                            (known-ctype-rep k)))))))))
