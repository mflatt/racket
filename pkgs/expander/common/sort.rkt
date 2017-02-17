#lang racket/base

(provide sort)
(#%require (rename '#%unsafe i+ unsafe-fx+)
           (rename '#%unsafe i- unsafe-fx-)
           (rename '#%unsafe i= unsafe-fx=)
           (rename '#%unsafe i< unsafe-fx<)
           (rename '#%unsafe i<= unsafe-fx<=)
           (rename '#%unsafe i> unsafe-fx>)
           (rename '#%unsafe i>= unsafe-fx>=)
           (rename '#%unsafe i>> unsafe-fxrshift)
           (rename '#%unsafe i<< unsafe-fxlshift)
           (rename '#%unsafe vref unsafe-vector-ref)
           (rename '#%unsafe vset! unsafe-vector-set!)
           (rename '#%unsafe ucar unsafe-car)
           (rename '#%unsafe ucdr unsafe-cdr)
           (rename '#%unsafe unsafe-fl< unsafe-fl<)
           (rename '#%unsafe unsafe-fl<= unsafe-fl<=)
           (rename '#%unsafe unsafe-fl> unsafe-fl>)
           (rename '#%unsafe unsafe-fl>= unsafe-fl>=))


(define-syntax-rule (i/2 x) (i>> x 1))
(define-syntax-rule (i*2 x) (i<< x 1))

(define (sort lst less-than? #:key [key #f])
  (unless (list? lst)
    (raise-argument-error 'sort "list?" lst))
  (let ([n (length lst)])
    (define-syntax-rule (<? x y)
      (if key
          (less-than? (key x) (key y))
          (less-than? x y)))
    (cond
      ;; trivial cases (where we know there is no caching to be done)
      [(= n 0) lst]
      ;; below we can assume an unsorted list
      ;; inlined case, for optimization of short lists
      [(= n 1) lst]
      [(= n 2)
       (if (<? (car lst) (cadr lst))
           lst
           (list (cadr lst) (car lst)))]
      [(= n 3)
       (let ([a (car lst)] [b (cadr lst)] [c (caddr lst)])
         ;; General note: we need a stable sort, so we should always compare
         ;; (<? later-item earlier-item) since it gives more information.
         ;; Good code should have each permutation appears exactly once.
         ;; This means that n=4 will have 23 cases, so don't bother.                 
         (if (<? b a)
             ;; b<a
             (if (<? c b)
                 (list c b a)
                 ;; b<a, b<=c
                 (if (<? c a)
                     (list b c a)
                     (list b a c)))
             ;; a<=b, so c<b (b<=c is impossible due to above test)
             (if (<? c b)
                 (if (<? c a)
                     (list c a b)
                     (list a c b))
                 lst)))]
    ;; check if the list is already sorted (which can be common, eg,
    ;; directory lists)
    [(let loop ([last (car lst)] [next (cdr lst)])
       (or (null? next)
           (and (not (<? (car next) last))
                (loop (car next) (cdr next)))))
     lst]
    [else (let ([vec (make-vector (+ n (ceiling (/ n 2))))])
            ;; list -> vector
            (let loop ([i 0] [lst lst])
              (when (pair? lst)
                (vector-set! vec i (car lst))
                (loop (add1 i) (cdr lst))))
            ;; sort
            (sort-internal-body vec less-than? n key)
            ;; vector -> list
            (let loop ([i n] [r '()])
              (let ([i (sub1 i)])
                (if (< i 0) r (loop i (cons (vector-ref vec i) r))))))])))

(define-syntax-rule (sort-internal-body A less-than? n key)
  (let ()
    ;; comparison & vector access macros
    (define-syntax-rule (<? x y)
      (if key
          (less-than? (key x) (key y))
          (less-than? x y)))
    (define-syntax-rule (ref index) (vref A index))
    (define-syntax-rule (set! index val) (vset! A index val))
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ;; Stable Sort (Mergesort)
    ;; (used by `sort', `vector-sort', and `vector-sort!')
    ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ;; Based on "Fast mergesort implementation based on half-copying merge algorithm",
    ;; Cezary Juszczak, http://kicia.ift.uni.wroc.pl/algorytmy/mergesortpaper.pdf
    ;; Written in Racket by Eli Barzilay.  (Note: the reason for the seemingly
    ;; redundant pointer arithmetic in that paper is dealing with cases of uneven
    ;; number of elements.)
    (let* ([n/2- (i/2 n)]
           [n/2+ (i- n n/2-)])
      ;; - - - - - - - - - - - - - - - - - - -
      ;; Merge
      ;; - - - - - - - - - - - - - - - - - - -
      (define-syntax-rule (merge lo? A1 A2 B1 B2 C1)
        (let ([b2 B2])
          (let loop ([a1 A1] [b1 B1] [c1 C1])
            (let ([x (ref a1)] [y (ref b1)])
              (if (if lo? (not (<? y x)) (<? x y))
                  (begin (set! c1 x)
                         (let ([a1 (i+ a1 1)] [c1 (i+ c1 1)])
                           (when (i< c1 b1) (loop a1 b1 c1))))
                  (begin (set! c1 y)
                         (let ([b1 (i+ b1 1)] [c1 (i+ c1 1)])
                           (if (i<= b2 b1)
                               (let loop ([a1 a1] [c1 c1])
                                 (when (i< c1 b1)
                                   (set! c1 (ref a1))
                                   (loop (i+ a1 1) (i+ c1 1))))
                               (loop a1 b1 c1)))))))))
      
      ;; - - - - - - - - - - - - - - - - - - -
      ;; copying-insertionsort
      ;; - - - - - - - - - - - - - - - - - - -
      (define-syntax-rule (copying-insertionsort Alo Blo n)
        ;; n is never 0
        (begin (set! Blo (ref Alo))
               (let iloop ([i 1])
                 (when (i< i n)
                   (let ([ref-i (ref (i+ Alo i))])
                     (let jloop ([j (i+ Blo i)])
                       (let ([ref-j-1 (ref (i- j 1))])
                         (if (and (i< Blo j) (<? ref-i ref-j-1))
                             (begin (set! j ref-j-1) (jloop (i- j 1)))
                             (begin (set! j ref-i) (iloop (i+ i 1)))))))))))
      
      ;; - - - - - - - - - - - - - - - - - - -
      ;; Mergesort
      ;; - - - - - - - - - - - - - - - - - - -
      (define (copying-mergesort Alo Blo n)
        (cond
          ;; n is never 0, smaller values are more frequent
          [(i= n 1) (set! Blo (ref Alo))]
          [(i= n 2) (let ([x (ref Alo)] [y (ref (i+ Alo 1))])
                      (if (<? y x)
                          (begin (set! Blo y) (set! (i+ Blo 1) x))
                          (begin (set! Blo x) (set! (i+ Blo 1) y))))]
          ;; insertion sort for small chunks (not much difference up to ~30)
          [(i< n 16) (copying-insertionsort Alo Blo n)]
          [else (let* ([n/2- (i/2 n)]
                       [n/2+ (i- n n/2-)])
                  (let ([Amid1 (i+ Alo n/2-)]
                        [Amid2 (i+ Alo n/2+)]
                        [Bmid1 (i+ Blo n/2-)])
                    (copying-mergesort Amid1 Bmid1 n/2+)
                    (copying-mergesort Alo Amid2 n/2-)
                    (merge #t Amid2 (i+ Alo n) Bmid1 (i+ Blo n) Blo)))]))
      ;; start the sorting!
      (let ([Alo 0] [Amid1 n/2-] [Amid2 n/2+] [Ahi n] [B1lo n])
        (copying-mergesort Amid1 B1lo n/2+)
        (unless (zero? n/2-) (copying-mergesort Alo Amid2 n/2-))
        (merge #f B1lo (i+ B1lo n/2+) Amid2 Ahi Alo)))))
