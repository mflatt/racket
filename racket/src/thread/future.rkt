#lang racket/base
(require racket/fixnum
         "config.rkt"
         "place-local.rkt"
         "place-object.rkt"
         "check.rkt"
         "internal-error.rkt"
         "host.rkt"
         "parameter.rkt"
         "atomic.rkt"
         "custodian-object.rkt"
         "thread.rkt"
         (submod "thread.rkt" for-future)
         (submod "custodian.rkt" for-future)
         (submod "semaphore.rkt" for-future)
         "sync.rkt"
         "evt.rkt"
         "future-object.rkt"
         "future-id.rkt"
         "future-lock.rkt"
         "future-logging.rkt"
         "error.rkt"
         (only-in '#%paramz
                  parameterization-key)
         (only-in '#%unsafe
                  unsafe-call-with-composable-continuation/no-wind
                  unsafe-abort-current-continuation/no-wind))

;; See "README.txt" for some general information about this
;; implementation of futures.

(provide init-future-place!
         futures-enabled?
         future
         future?
         would-be-future
         touch
         thread/parallel
         make-parallel-thread-pool
         parallel-thread-pool?
         future-block
         future-sync
         current-future-prompt
         currently-running-future
         reset-future-logs-for-tracing!
         mark-future-trace-end!
         set-processor-count!)

(module+ for-place
  (provide set-place-future-procs!
           kill-future-schedulers))

(module+ for-fsemaphore
  (provide future*-lock
           set-future*-state!
           future-maybe-notify-stop
           future-suspend
           future-notify-dependent
           wakeup-this-place))

(define (init-future-place!)
  (init-future-logging-place!))

(define (futures-enabled?)
  (threaded?))

;; ----------------------------------------

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
  (define id (get-next-id))
  (log-future 'create #:data id)
  (future* id
           (make-lock) ; lock
           cust
           #f          ; parallel
           would-be?
           thunk
           #f          ; prev
           #f          ; next
           #f          ; results
           #f          ; state
           #hasheq())) ; dependents

(define (future? v)
  (future*? v))

(define (current-future-in-future-thread) ; includes would-be futures
  (define f (current-future))
  (and f
       (or (in-future-thread?)
           (future*-would-be? f))
       f))

(define (current-future-in-unblock-thread)
  (define f (current-future))
  (and f
       (let ([t (current-thread/in-atomic)])
         (and t (future*-parallel f)))
       f))

(define future-scheduler-prompt-tag (make-continuation-prompt-tag 'future-scheduler))
(define future-start-prompt-tag (make-continuation-prompt-tag 'future-start))

(define (current-future-prompt)
  (define f (current-future))
  (if (future*-parallel f)
      ;; in a parallel thread, the future sees the full continuation,
      ;; whether it's running in a future pthread or as a Racket thread
      #f
      future-scheduler-prompt-tag))

;; called with lock on f held;
;; in a non-main pthread, caller is responsible for logging 'end-work;
;; in a non-mail thread, decrements `(current-atomic)` just before starting thunk
(define (run-future f
                    #:was-blocked? [was-blocked? #f]
                    #:as-unblock? [as-unblock? #f])
  (set-future*-state! f (if as-unblock?
                            #f ; like an unscheduled future
                            'running))
  (define thunk (future*-thunk f))
  (set-future*-thunk! f #f)
  (lock-release (future*-lock f))
  (when was-blocked?
    (when (logging-futures?)
      (log-future (if as-unblock? 'sync 'block) (future*-id f)
                  #:prim-name (continuation-current-primitive
                               thunk
                               '(unsafe-start-atomic)))
      (log-future (if as-unblock? 'sync 'result) (future*-id f))))
  (unless (eq? (future*-would-be? f) 'blocked)
    (log-future 'start-work (future*-id f)))
  (define (finish! results state)
    (start-future-uninterrupted)
    (lock-acquire (future*-lock f))
    (future-maybe-notify-stop f)
    (set-future*-results! f results)
    (set-future*-state! f state)
    (define deps (future*-dependents f))
    (set-future*-dependents! f #hasheq())
    (lock-release (future*-lock f))
    ;; stay in uninterrupted mode here, because we need to make sure
    ;; that dependents get rescheduled
    (future-notify-dependents deps)
    (unblock-thread f)
    (end-future-uninterrupted)
    (log-future 'complete (future*-id f)))
  (cond
    [(current-future-in-future-thread)
     (when (future*-parallel f)
       (set-engine-thread-cell-state! (thread-cells (parallel*-thread (future*-parallel f)))))
     ;; An attempt to escape will cause the future to block, so
     ;; we only need to handle success
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (end-future-uninterrupted)
                            (thunk))
                          future-start-prompt-tag
                          (lambda args (void))))
                       (lambda results
                         (finish! results 'done)))]
    [as-unblock?
     ;; result is ignored, and will not block, but might suspend
     ;; to be rescheduled to run in a future pthread
     (current-future f)
     (set-engine-thread-cell-state! (thread-cells (parallel*-thread (future*-parallel f))))
     ;; unblock thread's start has `future-start-prompt-tag` prompt:
     (thunk)]
    [(eq? (future*-would-be? f) #t)
     ;; Similar to `(current-future-in-future-thread)` case, but retries
     ;; excplitily if the future blocks
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (current-future f)
                            (begin0
                              (thunk)
                              (current-future #f)))
                          future-start-prompt-tag
                          (lambda args
                            ;; Blocked as a would-be future; `(current-future)` has been
                            ;; reset to #f, and we can retry immediately
                            (set-future*-would-be?! f 'blocked)
                            (touch f))))
                       (lambda results
                         (when (eq? (future*-state f) 'running)
                           (finish! results 'done)
                           (log-future 'end-work (future*-id f)))))]
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
          (finish! #f 'aborted))
        (log-future 'end-work (future*-id f))))]))

(define/who (future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (cond
    [(not (futures-enabled?))
     (would-be-future thunk)]
    [else
     (define me-f (current-future))
     (define cust (future-custodian me-f))
     (when (and cust (not me-f))
       (maybe-start-scheduler)
       (set-custodian-sync-futures?! cust #t))
     (define f (create-future thunk cust #f))
     (when cust
       (schedule-future! f))
     f]))

(define/who (would-be-future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (ensure-place-wakeup-handle)
  (create-future thunk (future-custodian (current-future)) #t))

(define (future-custodian me-f)
  (if me-f
      (future*-custodian me-f)
      (thread-representative-custodian (current-thread/in-atomic))))

(define/who (make-parallel-thread-pool [n pthread-count])
  (check who exact-positive-integer? n)
  (make-phantom-bytes (* n 1024)) ; intended to make sure that `n` is reasonable 
  (atomically
   (define s (start-scheduler n #t))
   (set-place-schedulers! current-place (hash-set (place-schedulers current-place) s #t))
   (define pool (parallel-thread-pool s))
   (host:will-register custodian-will-executor pool
                       (lambda (pool)
                         (define s (parallel-thread-pool-scheduler pool))
                         (kill-future-scheduler s)
                         (set-place-schedulers! current-place (hash-remove (place-schedulers current-place) s))))
   pool))

(define/who (thread/parallel thunk [pool (make-parallel-thread-pool)])
  (check who (procedure-arity-includes/c 0) thunk)
  (check who parallel-thread-pool? pool)
  (cond
    [(not (futures-enabled?))
     (thread thunk)]
    [else
     (define cust (current-custodian))
     (define paramz (current-parameterization))
     (define break-enabled (current-break-enabled-cell))
     (define thunk-in-prompt
       (lambda ()
         ;; Use the default prompt tag inside a prompt with
         ;; `future-start-prompt-tag` so that capturing the
         ;; continuation (as far as clients can tell) does
         ;; not capture its futureness, and also so that
         ;; continuation-mark actions generally complete local
         ;; to the future
         (call-with-continuation-prompt
          (lambda ()
            (with-continuation-mark
                parameterization-key paramz
                (with-continuation-mark
                    break-enabled-key
                  break-enabled
                  (|#%app| thunk))))
          (default-continuation-prompt-tag))))
     (define me-f (create-future thunk-in-prompt cust #f))
     (define th
       (do-make-thread who
                       #:break-enabled-cell parallel-break-disabled-cell
                       #:custodian cust
                       #:schedule? #f
                       (lambda ()
                         (let loop ()
                           (call-with-continuation-prompt
                            (lambda () (touch-blocked me-f))
                            future-start-prompt-tag
                            (lambda args
                              (loop)))))))
     (set-future*-parallel! me-f (parallel* pool th #f))
     (thread-push-kill-callback! (lambda () (future-stop me-f)) th)
     ;; this is the step (internally atomic) that commits the thread to running:
     (schedule-future! me-f)
     th]))

;; When two futures interact, we may need to adjust both;
;; to keep locks ordered, take lock of future with the
;; lower ID, first; beware that the two futures may be
;; the same (in which case we're headed for a circular
;; dependency)
(define (lock-acquire-both f)
  (define me-f (current-future-in-future-thread))
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
  (define me-f (current-future-in-future-thread))
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
     (raise (exn:fail (error-message->string
                       'touch
                       "future previously aborted")
                      (current-continuation-marks)))]
    [(let ([cf (current-future)])
       (and cf (future*-parallel cf)))
     ;; We're in a `thread/parallel` future pthread, but `f` must be
     ;; in the futures different scheduler;
     ;; block so that the future is handled in a Racket thread
     (future-barrier)
     (call-with-values (lambda () (touch f))
                       (lambda results
                         (future-exit-barrier)
                         (apply values results)))]
    [(eq? s 'blocked)
     (cond
       [(current-future-in-future-thread)
        ;; Can't run a blocked future in a future pthread
        (dependent-on-future f)]
       [else
        ;; Lock on f is held (and no current future to lock)
        (run-future f #:was-blocked? #t)
        (apply values (future*-results f))])]
    [(eq? s #f)
     (cond
       [(current-future-in-future-thread)
        ;; Need to wait on `f`, so deschedule current one;
        ;; we may pick `f` next the queue (or maybe later)
        (dependent-on-future f)]
       [(future*-would-be? f) ; => not scheduled
        (lock-release-current)
        ;; Lock on f is held
        (run-future f)
        (apply values (future*-results f))]
       [else
        ;; Give up locks in hope of geting `f` off the
        ;; schedule queue
        (lock-release (future*-lock f))
        (cond
          [(try-deschedule-future? f)
           ;; lock on `f` is held...
           (run-future f)
           (apply values (future*-results f))]
          [else
           ;; Contention, so try again
           (touch f)])])]
    [(eq? s 'running)
     (cond
       [(current-future-in-future-thread)
        ;; Stop working on this one until `f` is done
        (dependent-on-future f)]
       [else
        ;; Have to wait until it's not running anywhere
        (set-future*-dependents! f (hash-set (future*-dependents f) 'place #t))
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [(future? s)
     (cond
       [(current-future-in-future-thread)
        ;; Waiting on `s` on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Maybe we can start running `s` to get `f` moving...
        (lock-release (future*-lock f))
        (touch s)
        (touch f)])]
    [(or (box? s) (eq? s 'fsema)) ; => dependent on fsemaphore
     (cond
       [(current-future-in-future-thread)
        ;; Lots to wait on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Wait until fsemaphore post succeeds for the future, then try again.
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [else
     (lock-release (future*-lock f))
     (internal-error "unrecognized future state")]))

(define/who (touch-blocked f)
  (lock-acquire (future*-lock f))
  (define s (future*-state f))
  (cond
    [(eq? s 'blocked)
     (run-future f #:was-blocked? #t #:as-unblock? #t)]
    [(or (eq? s 'done)
         (eq? s 'aborted))
     (lock-release (future*-lock f))]
    [else
     ;; the future is not blocked; suspend and get resumed if/when
     ;; needed again
     (lock-release (future*-lock f))
     ((thread-deschedule! (current-thread/in-atomic) #f 'future))
     ;; only reason we should get rescheduled is `unblock-thread`
     (touch-blocked f)]))

;; called in a future pthread;
;; called with lock held for both `f` and the current future
(define (dependent-on-future f)
  ;; in a future pthread, so set up a dependency and on `f` and
  ;; bail out, so the current future pthread can do other things;
  ;; note that `me-f` might be the same as `f`, in which case we'll
  ;; create a circular dependency
  (define me-f (current-future))
  (set-future*-dependents! f (hash-set (future*-dependents f) me-f #t))
  (set-future*-state! me-f f)
  (on-transition-to-unfinished)
  (unless (eq? me-f f)
    (lock-release (future*-lock f)))
  ;; almost the same as being blocked, but when `f` completes,
  ;; it will reschedule `me-f`
  (future-suspend f)
  ;; on return from `future-suspend`, no locks are held
  (touch f))

;; called in a future pthread, in a Racket thread running a would-be future,
;; or in a Racket thread is that is a future's unblock thread;
;; can be called from Rumble layer
(define (future-block)
  (define me-f (current-future-in-future-thread))
  (when me-f
    (unless (future*-would-be? me-f)
      (log-future 'block (future*-id me-f)))
    (lock-acquire (future*-lock me-f))
    (future-maybe-notify-stop me-f)
    (set-future*-state! me-f 'blocked)
    (on-transition-to-unfinished)
    (future-suspend)))

;; called in a Racket thread running a would-be future or as an unblock thread;
;; only does something if the thread matches the future's unblock thread
(define (future-unblock)
  (when (eqv? (current-atomic) 0)
    (define me-f (current-future-in-unblock-thread))
    (when me-f
      (cond
        [(continuation-prompt-available? future-start-prompt-tag)
         (lock-acquire (future*-lock me-f))
         ;; Assert: (eq? (future*-state me-f) #f)
         (with-continuation-mark
             break-enabled-key parallel-break-disabled-cell
             (future-suspend #:reschedule (lambda ()
                                            (schedule-future! me-f)
                                            (current-future #f)
                                            ;; back to start, which will suspend and then
                                            ;; loop to potentially (if resumed) unblock again
                                            (unsafe-abort-current-continuation/no-wind future-start-prompt-tag (void)))))]
        [else
         ;; thread has jumped outside of the future prompt; switch to being
         ;; a plain thread running the future's continuation
         (current-future #f)]))))

;; called with lock held on the current future, which implies
;; that `(current-atomic)` has been incremented, too
(define (future-suspend [touching-f #f]
                        #:reschedule [reschedule #f])
  (define me-f (current-future))
  (unsafe-call-with-composable-continuation/no-wind
   (lambda (k)
     (cond
       [(eqv? (current-atomic) 1)
        (set-future*-thunk! me-f (if (and (future*-parallel me-f)
                                          (in-future-thread?))
                                     (lambda ()
                                       ;; check for break on apply in Racket thread,
                                       ;; since `no-wind` won't check automatically
                                       (call-in-continuation k check-for-break))
                                     k))]
       [else
        ;; extra atomicity is from `start-uninterrupted`s
        (define n (fx- (current-atomic) 1))
        (current-atomic 1)
        (set-future*-thunk! me-f (lambda ()
                                   (current-atomic (+ n (current-atomic)))
                                   (k)))])
     ;; no future-scheduler swap out from here on:
     (unless (in-racket-thread?)
       (define p (future*-parallel me-f))
       (when p
         (set-scheduler-round-robin! (parallel-thread-pool-scheduler (parallel*-pool p)) 'pause)))
     ;; Release lock and go out of atomic mode:
     (lock-release (future*-lock me-f))
     (when touching-f
       (log-future 'touch (future*-id me-f) #:data (future*-id touching-f)))
     (unless (future*-would-be? me-f)
       (log-future 'suspend (future*-id me-f)))
     (cond
       [reschedule
        (reschedule)]
       [(future*-would-be? me-f)
        (current-future #f)
        (unsafe-abort-current-continuation/no-wind future-start-prompt-tag (void))]
       [else
        (unblock-thread me-f)
        (unsafe-abort-current-continuation/no-wind future-scheduler-prompt-tag (void))]))
   future-start-prompt-tag))

(define (future-swapping-out? f)
  (eq? (scheduler-round-robin (parallel-thread-pool-scheduler (parallel*-pool (future*-parallel f)))) 'pause))

;; in any pthread and potentially in atomic mode
(define (unblock-thread me-f)
  (define p (future*-parallel me-f))
  (when p
    (unless (parallel*-stop? p)
      (define th (parallel*-thread p))
      ;; Assert: (in-future-thread?)
      (set-engine-thread-cell-state! #f)
      (host:post-as-asynchronous-callback
       (lambda ()
         ;; in atomic mode and in arbitrary Racket thread selected by scheduler
         (cond
           [(thread-descheduled? th)
            ;; If the threads wasn't descheduled most recently by its
            ;; future, then Racket thread could still have noticed the waiting
            ;; future thread early, ran it, and then get descheduled for some
            ;; other good reason
            (when (eq? 'future (thread-interrupt-callback th))
              (set-thread-interrupt-callback! th #f)
              (unless (or (thread-dead? th)
                          (thread-suspended? th))
                (thread-reschedule! th)))]
           [else
            ;; Racket thread should be on its way back to `touch-blocked` or
            ;; alerady noticed the reader future; in the former case, it will
            ;; check on the future without needing to be rescheduled
            (void)])))
      (wakeup-this-place))))

;; in atomic mode in Racket thread when an unblocking thread is killed
(define (future-stop f)
  (cond
    [(try-deschedule-future? f)
     ;; lock on `f` is held...
     (set-parallel*-stop?! (future*-parallel f) #t)
     (lock-release (future*-lock f))]
    [else
     (lock-acquire (future*-lock f))
     (set-parallel*-stop?! (future*-parallel f) #t)
     (define mutex+cond (and (eq? (future*-state f) 'running)
                             (list (host:make-mutex) (host:make-condition))))
     (when mutex+cond
       (set-future*-results! f mutex+cond)
       (host:mutex-acquire (car mutex+cond)))
     (lock-release (future*-lock f))
     (when mutex+cond
       (let loop ()
         (host:condition-wait (cadr mutex+cond) (car mutex+cond))
         (lock-acquire (future*-lock f))
         (define done? (not (eq? (future*-state f) 'running)))
         (lock-release (future*-lock f))
         (unless done? (loop)))
       (host:mutex-release (car mutex+cond)))]))

;; lock on f is held
(define (future-maybe-notify-stop f)
  (define p (future*-parallel f))
  (when (and p
             (parallel*-stop? p)
             (eq? (future*-state f) 'running))
    (define mutex+cond (future*-results f))
    (host:mutex-acquire (car mutex+cond))
    (host:condition-broadcast (cadr mutex+cond))
    (host:mutex-release (car mutex+cond))))

;; ----------------------------------------

;; Can be in a future thread
;; Call `thunk` in the place's main thread:
(define (future-sync who thunk)
  (define me-f (current-future))
  (cond
    [(future*-would-be? me-f)
     (current-future #f)
     (log-future 'sync (future*-id me-f) #:prim-name who)
     (let ([v (thunk)])
       (log-future 'result (future*-id me-f))
       (current-future me-f)
       v)]
    [(in-racket-thread?)
     (thunk)]
    [else
     ;; In case the main thread is trying to shut down futures, check in:
     (engine-block)
     ;; Host's `call-as-asynchronous-callback` will post `thunk`
     ;; so that it's returned by `host:poll-async-callbacks` to
     ;; the scheduler in the place's main thread; it will also
     ;; tell the scheduler to be in atomic mode so that we don't
     ;; get terminated or swapped out while blocking on the main thread
     (host:call-as-asynchronous-callback
      (lambda ()
        (log-future 'sync (future*-id me-f) #:prim-name who)
        (let ([v (thunk)])
          (log-future 'result (future*-id me-f))
          v)))]))

;; ----------------------------------------

(define pthread-count 1)

;; Called by io layer
(define (set-processor-count! n)
  (set! pthread-count n))

(struct scheduler ([workers #:mutable]
                   [futures-head #:mutable]
                   [futures-tail #:mutable]
                   mutex   ; guards futures chain; see "future-lock.rkt" for discipline
                   cond    ; signaled when chain goes from empty to non-empty
                   ping-cond
                   [round-robin #:mutable] ; #f, 'round, 'pause
                   [capacity #:mutable])
  #:authentic)

(struct worker (id
                [pthread #:mutable]
                current-future-box ; reports current future (for access external to pthread)
                [die? #:mutable]
                [ping #:mutable]) ; box set to #t when the thread should check in with scheduler
  #:authentic)

(define current-scheduler
  (case-lambda
    [() (place-future-scheduler current-place)]
    [(s) (set-place-future-scheduler! current-place s)]))

(define (future-scheduler f)
  (define p (future*-parallel f))
  (if p
      (parallel-thread-pool-scheduler (parallel*-pool p))
      (current-scheduler)))

(define (make-worker id)
  (worker id
          #f         ; pthread
          (box #f)   ; current-future-box
          #f         ; die?
          (box #f)))

;; called in a Racket thread
(define (maybe-start-scheduler)
  (atomically
   (unless (current-scheduler)
     (current-scheduler (start-scheduler pthread-count #f)))))

;; called in atomic mode in a Racket thread
(define (start-scheduler pthread-count round-robin?)
  (ensure-place-wakeup-handle)
  (define s (scheduler '()
                       #f  ; futures-head
                       #f  ; futures-tail
                       (host:make-mutex)
                       (host:make-condition)
                       (host:make-condition)
                       (and round-robin? 'round)
                       pthread-count))
  (define workers
    (for/list ([id (in-range 1 (add1 pthread-count))])
      (define w (make-worker id))
      (start-worker w s)
      w))
  (set-scheduler-workers! s workers)
  s)

;; called in atomic mode
(define (kill-future-schedulers)
  (define s (current-scheduler))
  (when s
    (kill-future-scheduler s)
    (current-scheduler #f))
  (for ([s (in-hash-keys (place-schedulers current-place))])
    (kill-future-scheduler s))
  (set-place-schedulers! current-place (hasheq)))

;; called in atomic mode
(define (kill-future-scheduler s)
  (host:mutex-acquire (scheduler-mutex s))
  (for ([w (in-list (scheduler-workers s))])
    (set-worker-die?! w #t))
  (host:mutex-release (scheduler-mutex s))
  (futures-sync-for-shutdown))

;; called in any pthread
;; called maybe holding an fsemaphore lock or scheduler lock, and
;; maybe atomically, but no other locks held
;; (see "future-lock.rkt" for more on lock discipline)
(define (schedule-future! f #:front? [front? #f])
  (start-future-uninterrupted)
  (define s (future-scheduler f))
  (host:mutex-acquire (scheduler-mutex s))
  (define old (if front?
                  (scheduler-futures-head s)
                  (scheduler-futures-tail s)))
  (cond
    [(not old)
     (set-scheduler-futures-head! s f)
     (set-scheduler-futures-tail! s f)]
    [front?
     (set-future*-next! f old)
     (set-future*-prev! old f)
     (set-scheduler-futures-head! s f)]
    [else
     (set-future*-prev! f old)
     (set-future*-next! old f)
     (set-scheduler-futures-tail! s f)])
  (host:condition-signal (scheduler-cond s))
  (host:mutex-release (scheduler-mutex s))
  (increment-place-parallel-count! 1)
  (end-future-uninterrupted))

;; called with queue lock held
(define (deschedule-future f)
  (define s (future-scheduler f))
  (cond
    [(or (future*-prev f)
         (future*-next f))
     (if (future*-prev f)
         (set-future*-next! (future*-prev f) (future*-next f))
         (set-scheduler-futures-head! s (future*-next f)))
     (if (future*-next f)
         (set-future*-prev! (future*-next f) (future*-prev f))
         (set-scheduler-futures-tail! s (future*-prev f)))
     (set-future*-prev! f #f)
     (set-future*-next! f #f)]
    [(eq? f (scheduler-futures-head s))
     (set-scheduler-futures-head! s #f)
     (set-scheduler-futures-tail! s #f)]
    [else
     (internal-error "future is not in queue")]))

;; called with no locks held; if successful,
;; returns with lock held on f
(define (try-deschedule-future? f)
  (start-future-uninterrupted)
  (define s (future-scheduler f))
  (host:mutex-acquire (scheduler-mutex s))
  (define ok?
    (cond
      [(and (not (future*-prev f))
            (not (future*-next f))
            (not (eq? f (scheduler-futures-head s))))
       ;; Was descheduled by someone else already, or maybe
       ;; hasn't yet made it back into the schedule after a
       ;; dependency triggered `future-notify-dependent`
       #f]
      [else
       (deschedule-future f)
       (lock-acquire (future*-lock f))
       #t]))
  (host:mutex-release (scheduler-mutex s))
  (when ok? (increment-place-parallel-count! -1))
  (end-future-uninterrupted)
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
  (lock-acquire (future*-lock f))
  (define p (future*-parallel f))
  (cond
    [(and p (parallel*-stop? p))
     (lock-release (future*-lock f))
     #f]
    [else
     (set-future*-state! f #f)
     (lock-release (future*-lock f))
     (on-transition-to-unfinished)
     (if (future*-would-be? f)
         (wakeup-this-place)
         (schedule-future! f #:front? #t))
     #t]))

;; ----------------------------------------

(define-syntax-rule (keep-trying e)
  (let loop () (unless e (loop))))

(define (start-worker w s)
  (define th
    (fork-pthread
     (lambda ()
       (current-thread/in-atomic #f)
       (current-future 'worker)
       (host:mutex-acquire (scheduler-mutex s))
       (let loop ()
         (check-in w s)
         (cond
           [(worker-die? w) ; worker was killed
            (host:mutex-release (scheduler-mutex s))]
           [(scheduler-futures-head s)
            => (lambda (f)
                 (deschedule-future f)
                 (set-scheduler-capacity! s (- (scheduler-capacity s) 1))
                 (host:mutex-release (scheduler-mutex s))
                 (lock-acquire (future*-lock f))
                 ;; lock is held on f; run the future
                 (maybe-run-future-in-worker f w s)
                 ;; look for more work
                 (host:mutex-acquire (scheduler-mutex s))
                 (set-scheduler-capacity! s (+ (scheduler-capacity s) 1))
                 (loop))]
           [else
            ;; wait for work
            (host:condition-wait (scheduler-cond s) (scheduler-mutex s))
            (loop)])))))
  (set-worker-pthread! w th))

;; called with lock on f
(define (maybe-run-future-in-worker f w s)
  ;; Don't start the future if the custodian is shut down,
  ;; because we may have transitioned from 'pending to
  ;; 'running without an intervening check
  (cond
    [(or (custodian-shut-down?/other-pthread (future*-custodian f))
         (future-stop? f))
     (future-maybe-notify-stop f)
     (set-future*-state! f 'blocked)
     (on-transition-to-unfinished)
     (increment-place-parallel-count! -1)
     (lock-release (future*-lock f))]
    [else
     (run-future-in-worker f w s)]))

(define (run-future-in-worker f w s)
  (current-future f)
  (set-box! (worker-current-future-box w) f)
  ;; If we didn't need to check custodians, could be just
  ;;   (call-with-continuation-prompt
  ;;     (lambda () (run-future f))
  ;;     future-scheduler-prompt-tag
  ;;     void)
  ;; But use an engine so we can periodically check that the future is
  ;; still supposed to run.
  ;; We take advantage of `current-atomic` to disable interruptions,
  ;; both directly here and in the implementation of
  ;; `unsafe-{start, end}-uninterruptable`.
  (define e (make-engine (lambda ()
                           ;; decrements `(current-atomic)`
                           (run-future f))
                         future-scheduler-prompt-tag
                         void
                         (make-engine-thread-cell-state break-enabled-default-cell
                                                        #t)
                         #t))
  (start-future-uninterrupted)
  (call-with-engine-completion
   (lambda (done)
     (let loop ([e e])
       (e TICKS
          (lambda ()
            ;; Check whether the main pthread wants to know we're here
            (when (and (zero? (current-atomic))
                       (worker-pinged? w))
              (host:mutex-acquire (scheduler-mutex s))
              (check-in w s)
              (host:mutex-release (scheduler-mutex s)))
            ;; Check that the future should still run
            (when (and (or (custodian-shut-down?/other-pthread (future*-custodian f))
                           (worker-die? w)
                           (future-stop? f))
                       (zero? (current-atomic)))
              (lock-acquire (future*-lock f))
              (future-maybe-notify-stop f)
              (set-future*-state! f #f)
              (on-transition-to-unfinished)
              (future-suspend))
            (when (and (eq? (scheduler-round-robin s) 'round)
                       (zero? (current-atomic)))
              (check-for-break)
              (host:mutex-acquire (scheduler-mutex s))
              (define others? (and (scheduler-futures-head s)
                                   (zero? (scheduler-capacity s))))
              (host:mutex-release (scheduler-mutex s))
              (when others?
                (lock-acquire (future*-lock f))
                (future-maybe-notify-stop f)
                (set-future*-state! f #f)
                (define stop? (future-stop? f))
                (future-suspend
                 #:reschedule (lambda ()
                                (set-engine-thread-cell-state! #f)
                                (unless stop? (schedule-future! f))
                                (unsafe-abort-current-continuation/no-wind future-scheduler-prompt-tag (void))))
                (void))))
          (lambda (e results leftover-ticks)
            (cond
              [e (loop e)]
              [else
               ;; Done --- completed or suspended (e.g., blocked)
               (increment-place-parallel-count! -1)
               (done (void))]))))))
  (log-future 'end-work (future*-id f))
  (current-future 'worker)
  (set-box! (worker-current-future-box w) #f)
  (when (scheduler-round-robin s)
    (set-scheduler-round-robin! s 'round)))

;; in atomic mode
(define (futures-sync-for-shutdown)
  ;; Make sure any futures that are running in a future pthread
  ;; have had a chance to notice a custodian shutdown or a
  ;; future-scheduler shutdown.
  ;;
  ;; Assert: all workers have `ping` as #f.
  (define (sync-one s)
    (host:mutex-acquire (scheduler-mutex s))
    (for ([w (in-list (scheduler-workers s))])
      (let retry ()
        (unless (box-cas! (worker-ping w) #f #t)
          (retry))))
    ;; Assert: all workers have `ping` as #t.
    ;; Wake up idle threads so they check in:
    (host:condition-broadcast (scheduler-cond s))
    (drain-async-callbacks (scheduler-mutex s)) ; releases and re-acquires mutex
    ;; When a worker sets `ping` to #f, they must broadcast
    ;; a wakeup for the following loop's benefit
    (let loop ()
      (when (for/or ([w (in-list (scheduler-workers s))])
              (unbox (worker-ping w)))
        (host:condition-wait (scheduler-ping-cond s) (scheduler-mutex s))
        (loop)))
    ;; Assert: all workers have `ping` as #f.
    (host:mutex-release (scheduler-mutex s)))
  (when (current-scheduler)
    (sync-one (current-scheduler)))
  (for ([s (in-hash-keys (place-schedulers current-place))])
    (sync-one s)))

;; lock-free synchronization to check whether the box content is #f
(define (worker-pinged? w)
  (cond
    [(box-cas! (worker-ping w) #t #t) #t]
    [(box-cas! (worker-ping w) #f #f) #f]
    [else (worker-pinged? w)]))

;; called with scheduler lock
(define (check-in w s)
  (when (unbox (worker-ping w))
    (set-box! (worker-ping w) #f)
    (host:condition-broadcast (scheduler-ping-cond s))))

;; in atomic mode
;; While we're trying to finish up futures, some of them
;; may be blocked waiting for async callbacks. No new ones
;; will get posted since we've set the ping flag, so we
;; only have to drain once.
(define (drain-async-callbacks m)
  (host:mutex-release m)
  (define callbacks (host:poll-async-callbacks))
  (for ([callback (in-list callbacks)])
    (callback))
  (host:mutex-acquire m))

;; ----------------------------------------

;; called in a GCing pthread with all other pthreads stopped
(define (scheduler-add-thread-custodian-mapping! s ht)
  (when s
    (for ([w (in-list (scheduler-workers s))])
      (define f (unbox (worker-current-future-box w)))
      (when f
        (define c (future*-custodian f))
        (when c
          (hash-set! ht c (cons (worker-pthread w)
                                (hash-ref ht c null))))))))

;; ----------------------------------------

(define (reset-future-logs-for-tracing!)
  (atomically
   (flush-future-log)))

(define (mark-future-trace-end!)
  (log-future 'stop-trace #f))

;; ----------------------------------------

;; When a future changes from a state where the main thread may be
;; waiting for it, then make sure there's a wakeup signal
(define (on-transition-to-unfinished)
  (define me-f (current-future))
  (when (and me-f
             (or (not (future*-would-be? me-f))
                 (and (future*-parallel me-f)
                      (in-future-thread?))))
    (wakeup-this-place)))

(define wakeup-this-place (lambda () (void)))
(define ensure-place-wakeup-handle (lambda () (void)))

(define (set-place-future-procs! wakeup ensure)
  (set! wakeup-this-place wakeup)
  (set! ensure-place-wakeup-handle ensure))

;; tell "atomic.rkt" layer how to block:
(void (set-future-block! future-block future-unblock))

;; tell "custodian.rkt" how to sync and map pthreads to custodians:
(void (set-custodian-future-callbacks! futures-sync-for-shutdown
                                       scheduler-add-thread-custodian-mapping!))

;; tell "thread.rkt" layer how to maybe extract a thread from `(current-future)`:
(void (set-future->thread! (lambda (f)
                             (define p (future*-parallel f))
                             (and p (parallel*-thread p)))
                           future-swapping-out?))

(void (set-future-can-take-lock?! future*-parallel))
