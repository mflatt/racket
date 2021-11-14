#lang racket/base

(provide (struct-out binned-syntax))

(struct binned-syntax (stx)
  #:reflection-name 'import-bin)
