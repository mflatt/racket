#lang racket/base
(require "../common/check.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide file-stream-buffer-mode)

(define/who file-stream-buffer-mode
  (case-lambda
    [(p)
     (let ([p (cond
                [(input-port? p) (->core-input-port p)]
                [(output-port? p) (->core-output-port p)]
                [else
                 (raise-argument-error 'file-stream-buffer-mode "port?" p)])])
       (define buffer-mode (core-port-buffer-mode p))
       (and buffer-mode
            (buffer-mode)))]
    [(p mode)
     (unless (or (input-port? p) (output-port? p))
       (raise-argument-error who "port?" p))
     (unless (or (eq? mode 'none) (eq? mode 'line) (eq? mode 'block))
       (raise-argument-error who "(or/c 'none 'line 'block)" mode))
     (when (and (eq? mode 'line) (not (output-port? p)))
       (raise-arguments-error who
                              "'line buffering not supported for an input port"
                              "port" p))
     (define (set-buffer-mode p)
       (define buffer-mode (core-port-buffer-mode p))
       (cond
         [buffer-mode
          (buffer-mode mode)
          #t]
         [else #f]))
     (cond
       [(input-port? p)
        (or (set-buffer-mode (->core-input-port p))
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for input port"
                                   "mode" mode
                                   "input port" p))]
       [else
        (or (set-buffer-mode (->core-output-port p))
            (raise-arguments-error 'file-stream-buffer-mode
                                   "buffering not supported for output port"
                                   "mode" mode
                                   "output port" p))])
     (void)]))