#lang racket/base
(require '#%futures)

(provide thread/parallel
         make-parallel-thread-pool
         parallel-thread-pool?
         parallel-thread-pool-close)
