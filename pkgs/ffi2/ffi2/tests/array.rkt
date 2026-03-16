#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type triple_t (array int32_t 3))
(define-ffi2-type int32_t* void_t* #:tag int32_t*)

(check-equal? (ffi2-sizeof triple_t) 12)

(define p (ffi2-malloc triple_t))
(check-true (triple_t? p))
(check-true (int32_t*? p))

(check-equal? (triple_t-set! p 0 101) (void))
(check-equal? (triple_t-set! p 1 102) (void))
(check-equal? (triple_t-set! p 2 103) (void))

(check-equal? (triple_t-ref p 0) 101)
(check-equal? (triple_t-ref p 1) 102)
(check-equal? (triple_t-ref p 2) 103)

(check-exn exn:fail:contract? (lambda () (triple_t-ref p -1)))
(check-exn exn:fail:contract? (lambda () (triple_t-ref p "x")))
(check-exn exn:fail:contract? (lambda () (triple_t-ref p 3)))

(check-exn exn:fail:contract? (lambda () (triple_t-set! p -1 0)))
(check-exn exn:fail:contract? (lambda () (triple_t-set! p "x" 0)))
(check-exn exn:fail:contract? (lambda () (triple_t-set! p 3 0)))

(check-exn exn:fail:contract? (lambda () (triple_t-set! p 0 0.0)))
