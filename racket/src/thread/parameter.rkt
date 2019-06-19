#lang racket/base
(require "host.rkt")

(provide current-atomic
         current-thread/in-atomic
         current-future) ; not the one exported to Racket; see "api.rkt"

;; These definitions are specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register:
(define current-atomic (make-pthread-parameter 0))
(define current-thread/in-atomic (make-pthread-parameter #f))
(define current-future (make-pthread-parameter #f))

;; Calling `(current-thread/in-atomic)` is faster than
;; `(current-thread)`, but it's only valid in a place's main pthread
;; --- not in a future thread.
