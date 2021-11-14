#lang racket/base
(require "../syntax/syntax.rkt")

(provide constant-transformer?
         make-constant-transformer
         constant-transformer-target)

(struct constant-transformer (stx)
  #:reflection-name 'syntax-constant)

(define (make-constant-transformer stx)
  (constant-transformer stx))

(define (constant-transformer-target t)
  (constant-transformer-stx t))
