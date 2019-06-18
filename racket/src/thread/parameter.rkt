#lang racket/base
(require "host.rkt")

(provide current-atomic
         current-thread  ; not the one exported to Racket; see "api.rkt"
         current-future) ; not the one exported to Racket; see "api.rkt"

;; These definitions are specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register:
(define current-atomic (make-pthread-parameter 0))
(define current-thread (make-pthread-parameter #f))
(define current-future (make-pthread-parameter #f))
