#lang racket/base
(require "evt.rkt"
         "sync.rkt"
         "semaphore.rkt"
         "schedule-info.rkt"
         "sandman.rkt"
         "atomic.rkt"
         "custodian.rkt"
         "thread.rkt")

;; Unsafe scheduler-cooperation functions are made available to
;; clients through a `#%thread` primitive linklet instance:

(provide #%thread-instance)

(define #%thread-instance
  (hasheq 'make-semaphore make-semaphore
          'semaphore-post semaphore-post
          'semaphore-wait semaphore-wait
          'semaphore-peek-evt semaphore-peek-evt
          'wrap-evt wrap-evt
          'choice-evt choice-evt
          'sync sync
          'sync/timeout sync/timeout
          'evt? evt?
          'sync-atomic-poll-evt? sync-atomic-poll-evt?
          'prop:evt prop:evt
          'prop:secondary-evt prop:secondary-evt
          'poller poller
          'poller-evt poller-evt
          'poll-ctx-poll? poll-ctx-poll?
          'poll-ctx-select-proc poll-ctx-select-proc
          'poll-ctx-sched-info poll-ctx-sched-info
          'control-state-evt control-state-evt
          'async-evt async-evt
          'current-sandman current-sandman
          'schedule-info-current-exts schedule-info-current-exts
          'schedule-info-did-work! schedule-info-did-work!
          'start-atomic start-atomic
          'end-atomic end-atomic
          'current-custodian current-custodian
          'custodian-shut-down? custodian-shut-down?
          'unsafe-custodian-register unsafe-custodian-register
          'unsafe-custodian-unregister unsafe-custodian-unregister
          'thread-push-kill-callback! thread-push-kill-callback!
          'thread-pop-kill-callback! thread-pop-kill-callback!))