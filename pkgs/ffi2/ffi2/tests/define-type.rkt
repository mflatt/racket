#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type percentage_t double_t
  #:predicate (lambda (v) (and (real? v) (<= 0.0 v 100.0)))
  #:racket->c (lambda (v) (/ v 100.0))
  #:c->racket (lambda (v) (* v 100.0)))

(check-true (percentage_t? 100.0))
(check-true (percentage_t? 0.0))
(check-true (percentage_t? 25))
(check-false (percentage_t? 101.0))
(check-false (percentage_t? -1.0))

(let ()
  (define p (ffi2-malloc double_t))
  (ffi2-ptr-set! p double_t 0.5)
  (check-equal? (ffi2-ptr-ref p double_t) 0.5)
  (check-equal? (ffi2-ptr-ref p percentage_t) 50.0)
  (ffi2-ptr-set! p percentage_t 25.5)
  (check-equal? (ffi2-ptr-ref p double_t) 0.255)
  (ffi2-ptr-set! p percentage_t #e5.25)
  (check-equal? (ffi2-ptr-ref p double_t) 0.0525))

(define-ffi2-type percentage_box_t void_t*
  #:predicate (lambda (bx) (percentage_t? (unbox bx)))
  #:racket->c (lambda (bx)
                (define ptr (ffi2-malloc percentage_t))
                (ffi2-ptr-set! ptr percentage_t (unbox bx))
                ptr)
  #:c->racket (lambda (ptr)
                (box (ffi2-ptr-ref ptr percentage_t))))

(let ()
  (define p (ffi2-malloc #:gcable-traced void_t*))
  (ffi2-ptr-set! p percentage_box_t (box 50.5))
  (check-equal? (ffi2-ptr-ref (ffi2-ptr-ref p void_t*/gcable) double_t) 0.505))
