#lang racket/base

(provide provide/contract
         or/c)

(define-syntax-rule (provide/contract [id ctc] ...)
  (provide id ...))

(define or/c void)

