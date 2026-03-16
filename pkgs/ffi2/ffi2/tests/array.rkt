#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type triple_t (array int32_t 3))

(check-equal? (ffi2-sizeof triple_t) 12)

(ffi2-malloc triple_t)
