;;; Parts from "newhash.ss" in Chez Scheme's implementation

;;; newhash.ss
;;; Copyright 1984-2016 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define codes (make-weak-eq-hashtable))
(define counter 12345)

(define (eq-hash-code x)
  (cond
   [(symbol? x) (symbol-fast-hash x)]
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else
    (or (eq-hashtable-ref codes x #f)
        (let ([c (fx1+ counter)])
          (set! counter c)
          (eq-hashtable-set! codes x counter)
          c))]))

(define (symbol-fast-hash sym)
  ;; Avoid forcing the universal name of a gensym when hashing
  (if (gensym? sym)
      (or (getprop sym 'racket-gensym-hash-code)
          (let ([c (fx1+ counter)])
            (set! counter c)
            (putprop sym 'racket-gensym-hash-code c)
            c))
      (symbol-hash sym)))

;; Mostly copied from Chez Scheme's "newhash.ss":
(define number-hash
  (lambda (z)
    (cond
     [(fixnum? z) (if (fx< z 0) (fxnot z) z)]
     [(flonum? z) (#3%$flhash z)]
     [(bignum? z) (modulo z (most-positive-fixnum))]
     [(ratnum? z) (number-hash (+ (* (numerator z) 5) (denominator z)))]
     [else (logand (logxor (lognot (number-hash (real-part z))) (number-hash (imag-part z)))
                   (most-positive-fixnum))])))

(define (eqv-hash-code x)
  (cond
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else (eq-hash-code x)]))

;; We don't use `equal-hash` because we need impersonators to be able
;; to generate the same hash code as the unwrapped value.
(define (equal-hash-code x)
  (call-with-values (lambda () (equal-hash-loop x 0 0))
    (lambda (hc burn) hc)))

(define MAX-HASH-BURN 128)

(define (equal-hash-loop x burn hc)
  (define (+/fx hc k)
    (#3%fx+ hc k))
  (define (sll/fs hc i)
    (#3%fxsll hc i))
  (define (->fx v)
    (if (fixnum? v)
        v
        (modulo v (greatest-fixnum))))
  (define (mix1 hc)
    (+/fx hc (sll/fs hc 3)))
  (define (mix2 hc)
    (+/fx hc (sll/fs hc 5)))
  (cond
   [(fx> burn MAX-HASH-BURN) (values hc burn)]
   [(boolean? x) (values (+/fx hc (if x #x0ace0120 #x0cafe121)) burn)]
   [(null? x) (values (+/fx hc #x0cabd122) burn)]
   [(number? x) (values (+/fx hc (number-hash x)) burn)]
   [(char? x) (values (+/fx hc (char->integer x)) burn)]
   [(symbol? x) (values (+/fx hc (symbol-hash x)) burn)]
   [(string? x) (values (+/fx hc (string-hash x)) burn)]
   [(bytevector? x) (values (+/fx hc (equal-hash x)) burn)]
   [(box? x) (equal-hash-loop (unbox x) (fx+ burn 1) (+/fx hc 1))]
   [(pair? x)
    (let-values ([(hc0 burn) (equal-hash-loop (car x) (fx+ burn 2) 0)])
      (let ([hc (+/fx (mix1 hc) hc0)])
        (if (list? x)
            ;; If it's a list, don't count cdr direction as burn:
            (equal-hash-loop (cdr x) (fx- burn 2) hc)
            (equal-hash-loop (cdr x) burn hc))))]
   [(vector? x)
    (let ([len (vector-length x)])
      (cond
       [(fx= len 0) (values (+/fx hc 1) burn)]
       [else
        (let vec-loop ([i 0] [burn burn] [hc hc])
          (cond
           [(fx= i len) (values hc burn)]
           [else
            (let-values ([(hc0 burn) (equal-hash-loop (vector-ref x i) burn 0)])
              (vec-loop (fx+ i 1)
                        burn
                        (+/fx (mix2 hc) hc0)))]))]))]
   [(and (#%$record? x) (#%$record-hash-procedure x))
    => (lambda (rec-hash)
         (let ([burn (fx+ burn 2)])
           (let ([hc (+/fx hc (->fx (rec-hash x (lambda (x)
                                                  (let-values ([(hc0 burn0) (equal-hash-loop x burn 0)])
                                                    (set! burn burn0)
                                                    hc0)))))])
             (values hc burn))))]
   [(impersonator? x)
    ;; If an impersonator wraps a value where `equal?` hashing is
    ;; `eq?` hashing, such as for a procedure, then make sure
    ;; we discard the impersonator wrapper.
    (equal-hash-loop (impersonator-val x) burn hc)]
   [else (values (+/fx hc (eq-hash-code x)) burn)]))

(define (hash-code-combine hc v)
  (bitwise-and (+ (bitwise-arithmetic-shift-left hc 2)
                  v)
               (greatest-fixnum)))

(define (hash-code-combine-unordered hc v)
  (bitwise-and (+ hc v)
               (greatest-fixnum)))