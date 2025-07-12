#lang racket/base
(require "host.rkt"
         "parameter.rkt")

(provide (struct-out future*)
         (struct-out parallel-pool)

         currently-running-future-key
         currently-running-future)

;; ----------------------------------------

;; See "future-lock.rkt" for information on locking rules
(struct future* (id
                 lock
                 custodian          ; don't run in future pthread if custodian is shut down
                 pool               ; futures scheduler that manages the future, #f implies `(current-scheduler)`
                 [thread #:mutable] ; #f, a thread for unblocking, or 'stop termination request
                 [would-be? #:mutable] ; transitions from #t to 'blocked after blocked
                 [thunk #:mutable]  ; thunk or continuation
                 [prev #:mutable]   ; queue previous
                 [next #:mutable]   ; queue next
                 [results #:mutable] ; may have (cons <mutex> <condition>) to go with a top request
                 [state #:mutable]  ; #f (could run), 'running, 'blocked, 'done, 'aborted, 'fsema or box, or future waiting on
                 [dependents #:mutable]) ; futures that are blocked on this one
  #:authentic
  #:reflection-name 'future)

(struct parallel-pool (scheduler)
  #:authentic
  #:reflection-name 'parallel-pool)

;; ----------------------------------------

(define currently-running-future-key (gensym 'future))

;; Only called in a Racket thread:
(define (currently-running-future)
  (define f (current-future))
  (cond
    [f (and (not (future*-thread f))
            f)]
    [else
     (continuation-mark-set-first
      #f
      currently-running-future-key
      #f
      (unsafe-root-continuation-prompt-tag))]))
