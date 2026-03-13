#lang racket/base
(require ffi2
         rackunit)

(ffi2-struct point_t ([x int_t]
                      [y int_t]))

(ffi2-struct dimen_t ([width double_t]
                      [height double_t]))

(ffi2-struct rect_t ([topleft point_t]
                     [size dimen_t]))

;; `ffi2-sizeof` and `ffi2-offsetof` generally depend on the
;; platform, but `int_t` and `double_t` size and alignment are
;; the same everywhere (currently)

(check-equal? (ffi2-sizeof point_t) 8)
(check-equal? (ffi2-sizeof dimen_t) 16)
(check-true ((+ (ffi2-sizeof point_t) (ffi2-sizeof dimen_t))
             . <= .
             (ffi2-sizeof rect_t)))

(check-equal? (ffi2-offsetof point_t x) 0)
(check-equal? (ffi2-offsetof point_t y) 4)
(check-equal? (ffi2-offsetof dimen_t width) 0)
(check-equal? (ffi2-offsetof dimen_t height) 8)
(check-true ((ffi2-offsetof rect_t size) . >= . (ffi2-sizeof point_t)))

(let ()
  (define pt (ffi2-malloc #:manual point_t))
  (check-true (ffi2-ptr? pt))
  (check-false (ffi2-ptr/gcable? pt))
  (check-true (point_t*? pt))
  (ffi2-free pt))

(define pt (ffi2-malloc point_t))
(check-true (ffi2-ptr? pt))
(check-true (ffi2-ptr/gcable? pt))
(check-true (point_t*? pt))
(check-equal? (set-point_t-x! pt 10) (void))
(check-equal? (set-point_t-y! pt 11) (void))
(check-equal? (point_t-x pt) 10)
(check-equal? (point_t-y pt) 11)
(check-exn exn:fail:contract? (lambda () (set-point_t-x! pt 0.0)))

(let ()
  (define p (ffi2-malloc 1))
  (check-exn exn:fail:contract? (lambda () (point_t-x p)))
  (check-exn exn:fail:contract? (lambda () (set-point_t-x! p 0))))

(let ()
  (define pt1 (point_t 99 100))
  (check-equal? (point_t-x pt1) 99)
  (check-equal? (point_t-y pt1) 100))

(let ()
  (define r (ffi2-malloc rect_t))
  (ffi2-memset r 255 (ffi2-sizeof rect_t))
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) -1)

  (ffi2-ptr-set! r int_t 1 22)
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) 22)

  (check-equal? (ffi2-ptr-ref r int_t 0) -1)
  (check-equal? (ffi2-ptr-ref r int_t 1) 22)
  (check-equal? (ffi2-ptr-ref r int_t 4 #:bytes) 22)

  (set-point_t-y! (rect_t-topleft r) 77)
  (check-equal? (point_t-x (rect_t-topleft r)) -1)
  (check-equal? (point_t-y (rect_t-topleft r)) 77)

  (set-rect_t-topleft! r pt)
  (check-equal? (point_t-x (rect_t-topleft r)) 10)
  (check-equal? (point_t-y (rect_t-topleft r)) 11)

  (set-rect_t-size! r (dimen_t 101.0 102.0))
  (check-equal? (point_t-x (rect_t-topleft r)) 10)
  (check-equal? (point_t-y (rect_t-topleft r)) 11)
  (check-equal? (dimen_t-width (rect_t-size r)) 101.0)
  (check-equal? (dimen_t-height (rect_t-size r)) 102.0)

  (void))

(let ()
  (define r (rect_t (point_t 0 1) (dimen_t 3.0 4.0)))
  (check-equal? (point_t-x (rect_t-topleft r)) 0)
  (check-equal? (point_t-y (rect_t-topleft r)) 1)
  (check-equal? (dimen_t-width (rect_t-size r)) 3.0)
  (check-equal? (dimen_t-height (rect_t-size r)) 4.0)
  (void))
