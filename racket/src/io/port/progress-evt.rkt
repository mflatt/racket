#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "input-port.rkt"
         "count.rkt")

(provide (rename-out [progress-evt?* progress-evt?])
         port-provides-progress-evts?
         port-progress-evt
         port-commit-peeked

         check-progress-evt)

(struct progress-evt (port evt)
  #:property prop:evt (lambda (pe)
                        (wrap-evt (progress-evt-evt pe)
                                  (lambda args pe))))

(define progress-evt?*
  (let ([progress-evt?
         (case-lambda
           [(v) (progress-evt? v)]
           [(v port)
            (and (progress-evt? v)
                 (eq? port (progress-evt-port v)))])])
    progress-evt?))

;; ----------------------------------------

(define/who (port-provides-progress-evts? in)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (and (core-input-port-get-progress-evt in) #t)))

(define/who (port-progress-evt orig-in)
  (check who input-port? orig-in)
  (let ([in (->core-input-port orig-in)])
    (define get-progress-evt (core-input-port-get-progress-evt in))
    (if get-progress-evt
        (progress-evt orig-in (get-progress-evt))
        (raise-arguments-error 'port-progress-evt
                               "port does not provide progress evts"
                               "port" orig-in))))

(define/who (port-commit-peeked amt progress-evt evt [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who progress-evt? progress-evt)
  (check who sync-atomic-poll-evt?
         #:contract "(or/c channel-put-evt? channel? semaphore? semaphore-peek-evt? always-evt never-evt)"
         evt)
  (check who input-port? in)
  (check-progress-evt who progress-evt in)
  (let ([in (->core-input-port in)])
    (define commit (core-input-port-commit in))
    (atomically
     (define bstr (commit amt (progress-evt-evt progress-evt) evt))
     (cond
       [bstr
        (port-count! in (bytes-length bstr) bstr 0)
        #t]
       [else #f]))))

(define (check-progress-evt who progress-evt in)
  (unless (progress-evt?* progress-evt in)
    (raise-arguments-error who "evt is not a progress evt for the given port"
                           "evt" progress-evt
                           "port" in)))