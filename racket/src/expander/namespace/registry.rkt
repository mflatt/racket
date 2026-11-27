#lang racket/base

(require racket/private/place-local)

(provide (struct-out module-registry)
         make-module-registry
         registry-call-with-lock

         cross-phase-instance-ref
         cross-phase-instance-set!
         cross-phase-instances-place-init!)

(struct module-registry (declarations  ; resolved-module-path -> module
                         lock-box))    ; reentrant lock to guard registry for use by on-demand visits

(define (make-module-registry)
  (module-registry (make-hasheq) (box #f)))

;; A cross-phase persistent module has a single instance per place,
;; independent of namespaces and module registries: resolved-module-path ->
;; module-instance; it's an ephemeron table so that an instance for a
;; submodule of a module being expanded can be reclaimed
(define-place-local cross-phase-instances (make-ephemeron-hasheq))

(define (cross-phase-instances-place-init!)
  (set! cross-phase-instances (make-ephemeron-hasheq)))

(define (cross-phase-instance-ref name)
  (hash-ref cross-phase-instances name #f))

(define (cross-phase-instance-set! name mi)
  (hash-set! cross-phase-instances name mi))

(define (registry-call-with-lock r proc)
  (define lock-box (module-registry-lock-box r))
  (let loop ()
    (define v (unbox lock-box))
    (cond
     [(or (not v)
          (sync/timeout 0 (car v) (let ([t (weak-box-value (cdr v))])
                                    (if (eq? t (current-thread))
                                        never-evt
                                        (or t always-evt)))))
      ;; Lock holder is released its semaphore or terminated
      (define sema (make-semaphore))
      (define lock (cons (semaphore-peek-evt sema) (make-weak-box (current-thread))))
      ((dynamic-wind
        void
        (lambda ()
          (cond
            [(box-cas! lock-box v lock)
             ;; This thread became the lock holder
             (call-with-values
              proc
              (lambda results
                (lambda () (apply values results))))]
            [else
             ;; CAS failed; take it from the top
             (lambda () (loop))]))
        (lambda ()
          (semaphore-post sema))))]
     [(eq? (current-thread) (weak-box-value (cdr v)))
      ;; This thread already holds the lock
      (proc)]
     [else
      ; Wait and try again:
      (sync (car v) (or (weak-box-value (cdr v)) always-evt))
      (loop)])))
