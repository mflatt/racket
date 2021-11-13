#lang racket/base
(require "../syntax/syntax.rkt")

(provide constant-transformer?
         prop:constant-transformer
         make-constant-transformer
         constant-transformer-target)

(define-values (prop:constant-transformer constant-transformer? constant-transformer-value)
  (make-struct-type-property 'constant-transformer
                             (lambda (v info)
                               (unless (or (exact-nonnegative-integer? v)
                                           (syntax? v))
                                 (raise-argument-error
                                  'guard-for-prop:constant-transformer
                                  (string-append "(or/c exact-nonnegative-integer?\n"
                                                 "      syntax?)")
                                  v))
                               (when (exact-nonnegative-integer? v)
                                 (unless (v . <= . (list-ref info 1))
                                   (raise-arguments-error 'guard-for-prop:constant-transformer
                                                          "field index >= initialized-field count for structure type"
                                                          "field index" v
                                                          "initialized-field count" (list-ref info 1)))
                                 (unless (member v (list-ref info 5))
                                   (raise-arguments-error 'guard-for-prop:constant-transformer
                                                          "field index not declared immutable"
                                                          "field index" v)))
                               (define ref (list-ref info 3))
                               (cond
                                [(identifier? v) (lambda (t) v)]
                                [else
                                 (lambda (t)
                                   (define val (ref t v))
                                   (if (syntax? val)
                                       val
                                       (datum->syntax #f '?)))]))))

(struct syntax-constant-transformer (id)
  ;; for direct access to constant:
  #:property prop:constant-transformer 0
  ;; for use as a generic macro transformer:
  #:property prop:procedure (lambda (t stx) (constant-transformer-target t))
  #:reflection-name 'constant-transformer)

(define (make-constant-transformer stx)
  (unless (syntax? stx)
    (raise-argument-error 'make-constant-transformer "syntax?" stx))
  (syntax-constant-transformer stx))

(define (constant-transformer-target t)
  ((constant-transformer-value t) t))
