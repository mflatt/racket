#lang racket/base
(require "internal-error.rkt"
         "parameter.rkt"
         "atomic.rkt"
         "host.rkt")

;; This module implements a lightweight spinlock for futures and
;; fsemaphores.

;; The overall locking regime depends on this lock order (i.e.,
;; when multiple locks are held at once, they must be acquired
;; in this order):
;;    - fsemaphore [one at a time]
;;    - schedule queue
;;    - futures, lower ID before higher ID

(provide with-lock
         make-lock
         lock-acquire
         lock-release
         start-future-uninterrupted
         end-future-uninterrupted)

(define-syntax-rule (with-lock lock-expr expr ...)
  (let ([lock lock-expr])
    (lock-acquire lock)
    (begin0
      (let () expr ...)
      (lock-release lock))))

(define (make-lock) (box 0))

(define (start-future-uninterrupted)
  (if (current-future)
      (current-atomic (add1 (current-atomic))) ; see `run-future-in-worker`
      (start-atomic)))

(define (end-future-uninterrupted)
  (if (current-future)
      (current-atomic (sub1 (current-atomic))) ; see `run-future-in-worker`
      (end-atomic)))

(define (lock-acquire lock)
  (start-future-uninterrupted)
  (let loop ()
    (unless (box-cas! lock 0 1)
      (loop))))

(define (lock-release lock)
  (unless (box-cas! lock 1 0)
    (internal-error "lock release failed!"))
  (end-future-uninterrupted))
