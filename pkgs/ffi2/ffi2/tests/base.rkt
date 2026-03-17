#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         ffi2
         rackunit)

(define-syntax (check-base stx)
  (syntax-parse stx
    [(_ [type_t #:good (good-val ...)
                #:bad (bad-val ...)
                (~optional malloc-kind
                           #:defaults ([malloc-kind #'#:gcable]))]
        ...)
     #'(begin
         (let ([p (ffi2-malloc malloc-kind type_t)])
           (define (set v) (ffi2-ptr-set! p type_t v))
           (define (ref) (ffi2-ptr-ref p type_t))
           (begin
             (set good-val)
             (check-equal? (ref) good-val))
           ...
           (check-exn exn:fail:contract? (lambda () (set bad-val)))
           ...)
         ...)]))

(check-base
 [int8_t #:good (-128 0 127) #:bad (-129 255 1.0 "oops")]
 [uint8_t #:good (0 127 255) #:bad (-1 256 1.0 "oops")]
 [int16_t #:good (#x-8000 0 #x7FFF) #:bad (#x-8001 #x8000 1.0 "oops")]
 [uint16_t #:good (0 #x7FFF #xFFFF) #:bad (-1 #x10000 1.0 "oops")]
 [int32_t #:good (#x-80000000 0 #x7FFFFFFF) #:bad (#x-80000001 #x80000000 1.0 "oops")]
 [uint32_t #:good (0 #x7FFFFFFF #xFFFFFFFF) #:bad (-1 #x100000000 1.0 "oops")]
 [int64_t #:good (#x-8000000000000000 0 #x7FFFFFFFFFFFFFFF) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [uint64_t #:good (0 #x7FFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [byte_t #:good (0 127 255) #:bad (-1 256 1.0 "oops")]
 [int_t #:good (#x-80000000 0 #x7FFFFFFF) #:bad (#x-80000001 #x80000000 1.0 "oops")]
 [uint_t #:good (0 #x7FFFFFFF #xFFFFFFFF) #:bad (-1 #x100000000 1.0 "oops")]
 [long_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [ulong_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [intptr_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [uintptr_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [size_t #:good (0 256) #:bad (-1 #x10000000000000000 1.0 "oops")]
 [ssize_t #:good (-256 0 256) #:bad (#x-8000000000000001 #x8000000000000000 1.0 "oops")]
 [float_t #:good (-1.0 1.0 +inf.0) #:bad (1 1/2 "oops")]
 [double_t #:good (-1.0 1.0 +inf.0) #:bad (1 1/2 "oops")]
 [intwchar_t #:good (0 255) #:bad (-1 1.0 "oops")]
 [wchar_t #:good (#\a #\!) #:bad (65 1.0 "oops")]
 [bool_t #:good (#t #f) #:bad ()]
 [intbool_t #:good (#t #f) #:bad ()]
 [string_t #:good ("apple" #f) #:bad (1 'apple #"apple") #:gcable-traced]
 [bytes_t #:good (#"apple" #f) #:bad (1 'apple "apple") #:gcable-traced]
 [bytes_ptr_t #:good (#f) #:bad (1 'apple "apple") #:gcable-traced]
 [path_t #:good (#f) #:bad (1 'apple "apple") #:gcable-traced])

(check-equal? (ffi2-sizeof int_t) 4)
(check-equal? (ffi2-sizeof float_t) 4)
(check-equal? (ffi2-sizeof double_t) 8)
(check-equal? (ffi2-sizeof intptr_t) (ffi2-sizeof void_t*))
(check-equal? (ffi2-sizeof uintptr_t) (ffi2-sizeof void_t*))

(let ()
  (define p (ffi2-malloc 16))
  (ffi2-ptr-set! p bool_t 'ok)
  (check-equal? (ffi2-ptr-ref p bool_t) #t)
  (ffi2-ptr-set! p intbool_t 'ok)
  (check-equal? (ffi2-ptr-ref p intbool_t) #t))

(let ()
  (define p (ffi2-malloc #:gcable-traced 16))
  (define path (bytes->path #"apple"))
  (ffi2-ptr-set! p path_t path)
  (check-equal? (ffi2-ptr-ref p path_t) path))

(let ()
  (define p (ffi2-malloc #:gcable-traced 16))
  (define bstr #"apple\0pie")
  (ffi2-ptr-set! p bytes_ptr_t bstr)
  (check-equal? (ffi2-ptr-ref p bytes_t) #"apple")
  (define ptr (ffi2-ptr-cast (ffi2-ptr-ref p void_t*) void_t* 6 #:bytes))
  (check-equal? (integer->char (ffi2-ptr-ref ptr byte_t 0)) #\p)
  (check-equal? (integer->char (ffi2-ptr-ref ptr byte_t 1)) #\i)
  (check-equal? (integer->char (ffi2-ptr-ref ptr byte_t 2)) #\e))
