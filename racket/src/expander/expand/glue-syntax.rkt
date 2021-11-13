#lang racket/base
(require "../syntax/syntax.rkt")

(provide (struct-out glue-syntax)
         make-glue-syntax)

(struct glue-syntax (target)
  #:reflection-name 'import-bin)

(define (make-glue-syntax stx)
  (unless (syntax? stx)
    (raise-argument-error 'make-glue-syntax "syntax?" stx))
  (glue-syntax stx))
