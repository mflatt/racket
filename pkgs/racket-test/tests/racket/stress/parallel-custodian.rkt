#lang racket/base
(require racket/parallel)

(for ([i (in-range 1000)])
  (define ts
    (for/list ([j (in-range 8)])
      (parameterize ([current-custodian (make-custodian)])
        (thread/parallel
         (lambda ()
           (custodian-shutdown-all (make-custodian)))))))
  (map thread-wait ts))
