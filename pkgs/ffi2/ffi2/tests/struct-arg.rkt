#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (ffi2.so clean-ffi2-lib)
  (build-ffi2-lib))

(define test-lib (ffi2-lib ffi2.so))

(define-ffi2-type intfloat_t (struct
                               [i int_t]
                               [f float_t]))

(define-ffi2-type to_double_t (int_t float_t . -> . double_t))

(define-ffi2-procedure intfloat_sum test-lib (intfloat_t . -> . double_t))
(define-ffi2-procedure intfloat_sum_content test-lib (intfloat_t* . -> . double_t))
(define-ffi2-procedure intfloat_build test-lib (int_t float_t . -> . intfloat_t))
(define-ffi2-procedure double_built test-lib (to_double_t . -> . double_t))
(define-ffi2-procedure multiply_built test-lib (to_double_t int_t . -> . double_t))
(define-ffi2-procedure multiply_built2 test-lib (to_double_t
                                                 to_double_t
                                                 int_t
                                                 . -> . double_t))
(define-ffi2-procedure intfloat_sum_built test-lib ((int_t float_t . -> . intfloat_t)
                                                    . -> . double_t))

(check-equal? (intfloat_sum (intfloat_t 2 3.5)) 5.5)
(check-equal? (intfloat_sum_content (intfloat_t 2 3.5)) 5.5)

(let ()
  (define n2 (intfloat_build 5 6.5))
  (check-equal? (intfloat_t-i n2) 5)
  (check-equal? (intfloat_t-f n2) 6.5))

(check-equal? (double_built (lambda (i f) (+ i f)))
              220.0)
(check-equal? (intfloat_sum_built (lambda (i f) (intfloat_t i f)))
              110.0)

(check-equal? (multiply_built (lambda (i f) (+ i f (multiply_built (lambda (i f) (+ i f)) 3)))
                              10)
              4400.0)
(check-equal? (multiply_built2 (lambda (i f) (+ i f))
                               (lambda (i f) (collect-garbage) (- i f))
                               10)
              200.0)

(clean-ffi2-lib)
