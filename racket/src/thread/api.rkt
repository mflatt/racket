#lang racket/base
(require "check.rkt"
         (rename-in "semaphore.rkt"
                    [semaphore-peek-evt raw:semaphore-peek-evt])
         (rename-in "evt.rkt"
                    [wrap-evt raw:wrap-evt]
                    [handle-evt raw:handle-evt]
                    [guard-evt raw:guard-evt])
         (rename-in "channel.rkt"
                    [channel-put-evt raw:channel-put-evt]))

(provide wrap-evt
         handle-evt
         guard-evt
         nack-guard-evt
         channel-put-evt
         semaphore-peek-evt)

(define (wrap-evt evt proc)
  (check 'wrap-evt evt? evt)
  (check 'wrap-evt procedure? proc)
  (raw:wrap-evt evt proc))

(define (handle-evt evt proc)
  (check 'handle-evt evt? evt)
  (check 'handle-evt procedure? proc)
  (raw:handle-evt evt proc))

(define (guard-evt proc)
  (check 'guard-evt
         (lambda (v) (and (procedure? v) (procedure-arity-includes? v 0)))
         #:contract "(procedure-arity-includes?/c 0)"
         proc)
  (raw:guard-evt proc))

(define (nack-guard-evt proc)
  (check 'nack-guard-evt
         (lambda (v) (and (procedure? v) (procedure-arity-includes? v 1)))
         #:contract "(procedure-arity-includes?/c 1)"
         proc)
  (raw:guard-evt (lambda ()
                   (define s (make-semaphore))
                   ;; Return control-state-evt to register
                   ;; the nack semaphore before exposing it to
                   ;; the `proc` callback:
                   (control-state-evt
                    (raw:guard-evt
                     (lambda ()
                       (define v (proc (raw:semaphore-peek-evt s)))
                       (if (evt? v)
                           v
                           (wrap-evt always-evt (lambda () v)))))
                    void
                    (lambda () (semaphore-post s))
                    void))))

(define (channel-put-evt ch v)
  (check 'channel-put-evt channel? ch)
  (raw:channel-put-evt ch v))

(define (semaphore-peek-evt s)
  (check 'semaphore-peek-evt semaphore? s)
  (raw:semaphore-peek-evt s))