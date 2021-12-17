#lang racket/base

;; The `raise-argument-error`, `raise-arguments-error`, and
;; `raise-range-error` functions imported into this layer are ones
;; that call hooks for primitive error messages. Otherwise, we need to
;; call the hooks ourselves.

(provide error-message->string)

;; a string as `orig-who` means "not a primitive name"
(define (error-message->string orig-who msg)
  (define who (cond
                [(symbol? orig-who) (symbol->string orig-who)]
                [(string? orig-who) (string->symbol orig-who)]
                [else orig-who]))
  (if who
      (string-append who ": " msg)
      msg))
