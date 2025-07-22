#lang racket/base

(define shared-ht (make-hasheq))

(for-each
 thread-wait
 (for/list ([j (in-range 8)])
   (thread #:pool 'own
           (lambda ()
             (for ([i (in-range 1000)])
               (define items
                 (for/list ([i (in-range 10)])
                   (cons 1 2)))
               (for ([item (in-list items)])
                 (hash-set! shared-ht item item))
               (for ([item (in-list items)])
                 (unless (eq? item (hash-ref shared-ht item #f))
                   (error "missing")))
               (for ([item (in-list items)])
                 (hash-remove! shared-ht item))
               (for ([item (in-list items)])
                 (when (hash-ref shared-ht item #f)
                   (error "still there"))))))))

