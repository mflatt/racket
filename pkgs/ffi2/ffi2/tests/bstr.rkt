#lang racket/base
(require ffi2
         rackunit)

(define bstr (make-bytes 32 65))
(define bstr-p (cpointer->ffi2-ptr bstr))
(check-equal? bstr #"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

(ffi2-ptr-set! bstr-p int_t 3 0)
(check-equal? bstr #"AAAAAAAAAAAA\0\0\0\0AAAAAAAAAAAAAAAA")

(ffi2-ptr-set! (ffi2-bytes-cast bstr void_t* 1 #:bytes) int16_t #x101)

(check-equal? bstr #"A\1\1AAAAAAAAA\0\0\0\0AAAAAAAAAAAAAAAA")

(ffi2-ptr-set! (ffi2-ptr-add (ffi2-bytes-cast bstr void_t*) 3 #:bytes) int16_t #x202)
(ffi2-ptr-set! (ffi2-ptr-add (ffi2-bytes-cast bstr void_t*) int16_t 6) int16_t #x303)
(check-equal? bstr #"A\1\1\2\2AAAAAAA\3\3\0\0AAAAAAAAAAAAAAAA")
