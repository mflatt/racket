#lang racket/base
(require "config.rkt"
         "place-local.rkt"
         "check.rkt"
         "internal-error.rkt"
         "host.rkt"
         "parameter.rkt"
         "atomic.rkt"
         "custodian-object.rkt"
         "thread.rkt"
         (submod "thread.rkt" for-future)
         (submod "custodian.rkt" for-future)
         "sync.rkt"
         "evt.rkt"
         "future-lock.rkt")

(provide init-future-place!
         futures-enabled?
         future
         future?
         would-be-future
         touch
         future-block
         current-future-prompt
         currently-running-future
         reset-future-logs-for-tracing!
         mark-future-trace-end!
         set-processor-count!)

(module+ for-place
  (provide set-place-future-procs!
           kill-future-scheduler))

(module+ for-fsemaphore
  (provide future*-lock
           set-future*-state!
           future-suspend
           future-notify-dependent))

(define (init-future-place!)
  (void))

;; ---------------------------- futures ----------------------------------

(define ID (box 1))

(define get-next-id
  (lambda ()
    (let ([id (unbox ID)])
      (if (box-cas! ID id (+ 1 id))
          id
          (get-next-id)))))

(define (futures-enabled?)
  (threaded?))

(struct future* (id
                 lock
                 custodian          ; don't run in future pthread if custodian is shut down
                 would-be?
                 [thunk #:mutable]  ; thunk or continuation
                 [prev #:mutable]   ; queue previous
                 [next #:mutable]   ; queue next
                 [results #:mutable]
                 [state #:mutable]  ; #f (could run), 'running, 'blocked, 'done, 'aborted, 'fsema, or future waiting on
                 [dependents #:mutable]) ; futures that are blocked on this one
  #:authentic
  #:reflection-name 'future)

(struct future-evt (future)
  #:property prop:evt (poller (lambda (fe poll-ctx)
                                (define f (future-evt-future fe))
                                (lock-acquire (future*-lock f))
                                (define s (future*-state f))
                                (lock-release (future*-lock f))
                                (cond
                                  [(or (eq? s 'running)
                                       (eq? s 'fsema))
                                   (values #f fe)]
                                  [else (values '(#t) #f)]))))

(define (create-future thunk cust would-be?)
  (future* (get-next-id)           ; id
           (make-lock)             ; lock
           cust
           would-be?
           thunk
           #f          ; prev
           #f          ; next
           #f          ; results
           #f          ; state
           #hasheq())) ; dependents

(define (future? v)
  (future*? v))

(define future-scheduler-prompt-tag (make-continuation-prompt-tag 'future-scheduler))
(define future-start-prompt-tag (make-continuation-prompt-tag 'future-star))

(define (current-future-prompt)
  (if (current-future)
      future-scheduler-prompt-tag
      (internal-error "not running in a future")))

(define currently-running-future-key (gensym 'future))

;; Only called in a Racket thread:
(define (currently-running-future)
  (continuation-mark-set-first
   #f
   currently-running-future-key
   #f
   (unsafe-root-continuation-prompt-tag)))

;; called with lock on f held
(define (run-future f)
  (set-future*-state! f 'running)
  (define thunk (future*-thunk f))
  (set-future*-thunk! f #f)
  (lock-release (future*-lock f))
  (define (finish! results state)
    (start-future-uninterrupted)
    (lock-acquire (future*-lock f))
    (set-future*-results! f results)
    (set-future*-state! f state)
    (define deps (future*-dependents f))
    (set-future*-dependents! f #hasheq())
    (lock-release (future*-lock f))
    ;; stay in uninterrupted mode here, because we need to make sure
    ;; that dependents get rescheduled
    (future-notify-dependents deps)
    (end-future-uninterrupted))
  (cond
    [(current-future)
     ;; An attempt to escape will cause the future to block, so
     ;; we only need to handle succees
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          thunk
                          future-start-prompt-tag
                          (lambda args (void))))
                       (lambda results
                         (finish! results 'done)))]
    [else
     ;; No need for the future prompt tag
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (with-continuation-mark
         currently-running-future-key f
         (call-with-values thunk
                           (lambda results
                             (finish! results 'done)))))
      (lambda ()
        (unless (eq? (future*-state f) 'done)
          (finish! #f 'aborted))))]))

(define/who (future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (cond
    [(not (futures-enabled?))
     (would-be-future thunk)]
    [else
     (define me-f (current-future))
     (define cust (if me-f
                      (future*-custodian me-f)
                      (thread-representative-custodian (current-thread))))
     (define f (create-future thunk cust #f))
     (when cust
       (unless me-f
         (maybe-start-scheduler)
         (set-custodian-sync-futures?! cust #t))
       (schedule-future! f))
     f]))

(define/who (would-be-future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (create-future thunk #f #t))

;; When two futures interact, we may need to adjust both;
;; to keep locks ordered, take lock of future with the
;; lower ID, first; beware that the two futures make be
;; the same (in which case we're headed for a circular
;; dependency)
(define (lock-acquire-both f)
  (define me-f (current-future))
  (cond
    [(or (not me-f)
         (eq? me-f f))
     (lock-acquire (future*-lock f))]
    [((future*-id me-f) . < . (future*-id f))
     (lock-acquire (future*-lock me-f))
     (lock-acquire (future*-lock f))]
    [else
     (lock-acquire (future*-lock f))
     (lock-acquire (future*-lock me-f))]))

(define (lock-release-both f)
  (lock-release-current)
  (lock-release (future*-lock f)))

(define (lock-release-current)
  (define me-f (current-future))
  (when me-f
    (lock-release (future*-lock me-f))))

(define/who (touch f)
  (check who future*? f)
  (lock-acquire-both f)
  (define s (future*-state f))
  (cond
    [(eq? s 'done)
     (lock-release-both f)
     (apply values (future*-results f))]
    [(eq? s 'aborted)
     (lock-release-both f)
     (raise (exn:fail "touch: future previously aborted"
                      (current-continuation-marks)))]
    [(eq? s 'blocked)
     (cond
       [(current-future)
        ;; Can't run a blocked future in a future pthread
        (dependent-on-future f)]
       [else
        ;; Lock on f is held (and no current future to lock)
        (run-future f)
        (apply values (future*-results f))])]
    [(eq? s #f)
     (cond
       [(future*-would-be? f)
        (lock-release-current)
        ;; Lock on f is held
        (run-future f)
        (apply values (future*-results f))]
       [(current-future)
        ;; Need to wait on `f`, so deschedule current one;
        ;; we may pick `f` next the queue (or maybe later)
        (dependent-on-future f)]
       [else
        (cond
          [(try-deschedule-future? f)
           ;; lock on `f` is held...
           (run-future f)
           (apply values (future*-results f))]
          [else
           ;; Contention, so try again
           (lock-release (future*-lock f))
           (touch f)])])]
    [(eq? s 'running)
     (cond
       [(current-future)
        ;; Stop working on this one until `f` is done
        (dependent-on-future f)]
       [else
        ;; Have to wait until it's not running anywhere
        (set-future*-dependents! f (hash-set (future*-dependents f) 'place #t))
        (lock-release (future*-lock f))
        (sync (future-evt f))
        (touch f)])]
    [(future? s)
     (cond
       [(current-future)
        ;; Waiting on `s` on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Maybe we can start running `s` to get `f` moving...
        (lock-release-both f)
        (touch s)
        (touch f)])]
    [(box? s) ; => dependent on fsemaphore
     (cond
       [(current-future)
        ;; Lots to wait on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Wait until fsemaphore post succeeds for the future, then try again.
        (lock-release (future*-lock f))
        (sync (future-evt f))
        (touch f)])]
    [else
     (lock-release (future*-lock f))
     (internal-error "unrecognized future state")]))

;; called in a futurre pthread;
;; called with lock held for both `f` and the current future
(define (dependent-on-future f)
  ;; in a future pthread, so set up a dependency and on `f` and
  ;; bail out, so the current future pthread can do other things;
  ;; note that `me-f` might be the same as `f`, in which case we'll
  ;; create a circular dependency
  (define me-f (current-future))
  (set-future*-dependents! f (hash-set (future*-dependents f) me-f #t))
  (set-future*-state! me-f f)
  (unless (eq? me-f f)
    (lock-release (future*-lock f)))
  ;; almost the same as being blocked, but when `f` completes,
  ;; it will reschedule `me-f`
  (future-suspend)
  ;; on return from `future-suspend`, no locks are held
  (touch f))

;; called in a futurre pthread;
;; can be called from Rumble layer
(define (future-block)
  (define me-f (current-future))
  (lock-acquire (future*-lock me-f))
  (set-future*-state! me-f 'blocked)
  (future-suspend))

;; called with lock held on the current future
(define (future-suspend)
  (define me-f (current-future))
  (call-with-composable-continuation
   (lambda (k)
     (set-future*-thunk! me-f k)
     (lock-release (future*-lock me-f))
     (abort-current-continuation future-scheduler-prompt-tag (void)))
   future-start-prompt-tag))

;; ------------------------------------- future scheduler ----------------------------------------

(define pthread-count 1)

(define (set-processor-count! n)
  (set! pthread-count n))

(define-place-local the-scheduler #f)

(struct worker (id
                [die? #:mutable]
                sync-state) ; box used to sync shutdowns: 'idle, 'running, or 'pending
  #:authentic)

(define (make-worker id)
  (worker id
          #f  ; die?
          (box 'idle)))

(struct scheduler ([workers #:mutable]
                   [futures-head #:mutable]
                   [futures-tail #:mutable]
                   mutex   ; guards futures chain; see "future-lock.rkt" for discipline
                   cond)   ; signaled when chain goes from empty to non-empty
  #:authentic)

;; called in a Racket thread
(define (maybe-start-scheduler)
  (atomically
   (unless the-scheduler
     (ensure-place-wakeup-handle)
     (set! the-scheduler (scheduler '()
                                    #f  ; futures-head
                                    #f  ; futures-tail
                                    (host:make-mutex)
                                    (host:make-condition)))
     (define workers
       (for/list ([id (in-range 1 (add1 pthread-count))])
         (define w (make-worker id))
         (start-worker w)
         w))
     (set-scheduler-workers! the-scheduler workers))))

;; called in <atomic mode
(define (kill-future-scheduler)
  (when the-scheduler
    (define s the-scheduler)
    (host:mutex-acquire (scheduler-mutex s))
    (for ([w (in-list (scheduler-workers s))])
      (set-worker-die?! w #t))
    (host:condition-signal (scheduler-cond s))
    (host:mutex-release (scheduler-mutex s))
    (futures-sync-for-shutdown)))

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (schedule-future! f #:front? [front? #f])
  (define s the-scheduler)
  (with-lock (future*-lock f)
    (host:mutex-acquire (scheduler-mutex s))
    (set-future*-state! f #f)
    (define old (if front?
                    (scheduler-futures-head s)
                    (scheduler-futures-tail s)))
    (cond
      [(not old)
       (set-scheduler-futures-head! s f)
       (set-scheduler-futures-tail! s f)
       (host:condition-signal (scheduler-cond s))]
      [front?
       (set-future*-next! f old)
       (set-future*-prev! old f)
       (set-scheduler-futures-head! s f)]
      [else
       (set-future*-prev! f old)
       (set-future*-next! old f)
       (set-scheduler-futures-tail! s f)])
    (host:mutex-release (scheduler-mutex s))))

;; called with lock on f held
(define (try-deschedule-future? f)
  (define s the-scheduler)
  (host:mutex-acquire (scheduler-mutex s))
  (define ok?
    (cond
      [(future*-state f) #f]
      [(or (future*-prev f)
           (future*-next f))
       (if (future*-prev f)
           (set-future*-next! (future*-prev f) (future*-next f))
           (set-scheduler-futures-head! s (future*-next f)))
       (if (future*-next f)
           (set-future*-prev! (future*-next f) (future*-prev f))
           (set-scheduler-futures-tail! s (future*-prev f)))
       (set-future*-prev! f #f)
       (set-future*-next! f #f)
       #t]
      [(eq? f (scheduler-futures-head s))
       (set-scheduler-futures-head! s #f)
       (set-scheduler-futures-tail! s #f)
       #t]
      [else
       (internal-error "future with #f state is not in queue")]))
  (host:mutex-release (scheduler-mutex s))
  ok?)

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (future-notify-dependents deps)
  (for ([f (in-hash-keys deps)])
    (cond
      [(eq? f 'place) (wakeup-this-place)]
      [else (future-notify-dependent f)])))

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (future-notify-dependent f)
  (schedule-future! f #:front? #t))

;; ----------------------------------------

(define (start-worker w)
  (define s the-scheduler)
  (fork-pthread
   (lambda ()
     (current-future 'worker)
     (let loop ()
       (host:mutex-acquire (scheduler-mutex s))
       (or (box-cas! (worker-sync-state w) 'idle 'running)
           (box-cas! (worker-sync-state w) 'pending 'running))
       (cond
         [(worker-die? w) ;; worker was killed
          (host:mutex-release (scheduler-mutex s))
          (box-cas! (worker-sync-state w) 'running 'idle)]
         [(scheduler-futures-head s)
          => (lambda (f)
               (host:mutex-release (scheduler-mutex s))
               (lock-acquire (future*-lock f))
               (cond
                 [(try-deschedule-future? f)
                  ;; lock is held on f; run the future
                  (maybe-run-future-in-worker f w)
                  ;; look for more work
                  (loop)]
                 [else
                  ;; we didn't get `f`, so look for more work
                  (lock-release (future*-lock f))
                  (loop)]))]
         [else
          ;; wait for work
          (or (box-cas! (worker-sync-state w) 'pending 'idle)
              (box-cas! (worker-sync-state w) 'running 'idle))
          (host:condition-wait (scheduler-cond s) (scheduler-mutex s))
          (host:mutex-release (scheduler-mutex s))
          (loop)])))))

;; called with lock on f
(define (maybe-run-future-in-worker f w)
  ;; Don't start the future if the custodian is shut down,
  ;; because we may have transitioned from 'pending to
  ;; 'running without an intervening check
  (cond
    [(custodian-shut-down? (future*-custodian f))
     (lock-release (future*-lock f))]
    [else
     (run-future-in-worker f w)]))

(define (run-future-in-worker f w)
  (current-future f)
  ;; If we didn't need to check custodians, could be just
  ;;   (call-with-continuation-prompt
  ;;     (lambda () (run-future f))
  ;;     future-scheduler-prompt-tag
  ;;     void)
  ;; But use an engine so we can periodically check that he future is
  ;; still supposed to run.
  ;; We take advantage of `current-atomic`, which would otherwise
  ;; be unused, to disable interruptions.
  (define e (make-engine (lambda () (run-future f))
                         future-scheduler-prompt-tag
                         void
                         break-enabled-default-cell
                         #t))
  (let loop ([e e])
    (e TICKS
       (lambda ()
         ;; Check that the future should still run
         (when (and (custodian-shut-down? (future*-custodian f))
                    (zero? (current-atomic)))
           (lock-acquire (future*-lock f))
           (set-future*-state! f #f)
           (future-suspend)))
       (lambda (leftover-ticks result)
         ;; Done --- completed or suspend (e.g., blocked)
         (void))
       (lambda (e timeout?)
         (loop e))))
  (current-future 'worker))

;; in atomic mode
(define (futures-sync-for-shutdown)
  ;; Make sure any futures that are running in a future pthread
  ;; have had a chance to notice a custodian shutdown or a
  ;; future-scheduler shutdown.
  ;;
  ;; Move each 'running worker into the 'pending state:
  (for ([w (in-list (scheduler-workers the-scheduler))])
    (box-cas! (worker-sync-state w) 'running 'pending))
  ;; A worker that transitions from 'pending to 'running or 'idle
  ;; is guaranteed to not run a future chose custodian is
  ;; shutdown or run any future if the worker is terminated
  (for ([w (in-list (scheduler-workers the-scheduler))])
    (define bx (worker-sync-state w))
    (let loop ()
      (when (box-cas! bx 'pending 'pending)
        (host:sleep 0.001) ; not much alternative to spinning
        (loop)))))

;; ----------------------------------------

(define (reset-future-logs-for-tracing!)
  (void))

(define (mark-future-trace-end!)
  (void))

;; ----------------------------------------

(define wakeup-this-place (lambda () (void)))
(define ensure-place-wakeup-handle (lambda () (void)))

(define (set-place-future-procs! wakeup ensure)
  (set! wakeup-this-place wakeup)
  (set! ensure-place-wakeup-handle ensure))

;; tell "atomic.rkt" layer how to block:
(void (set-future-block! future-block))

(void (set-custodian-futures-sync! futures-sync-for-shutdown))
