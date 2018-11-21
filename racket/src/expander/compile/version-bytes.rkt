#lang racket/base

(provide version-bytes)

(define version-bytes (string->bytes/utf-8 (version)))
