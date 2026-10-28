#lang racket/base
(require "../common/range.rkt"
         "config.rkt")

;; Add case-insensitive mappings as specified by `config`; a mapping
;; beyond `limit-c` is dropped, since a byte regexp's range must stay
;; within 0 to 255 (e.g., the upcase of byte 255 is 376)
(provide range-add*
         range-add-span*)

(define (range-add* range c config limit-c)
  (define (add range c2)
    (if (c2 . > . limit-c)
        range
        (range-add range c2)))
  (cond
   [(not c) range]
   [else
    (define range2 (range-add range c))
    (cond
     [(parse-config-case-sensitive? config) range2]
     [else
      (define range3 (add range2 (char->integer (char-upcase (integer->char c)))))
      (define range4 (add range3 (char->integer (char-foldcase (integer->char c)))))
      (add range4 (char->integer (char-downcase (integer->char c))))])]))

(define (range-add-span* range from-c to-c config limit-c)
  (cond
   [(parse-config-case-sensitive? config)
    (range-add-span range from-c to-c)]
   [else
    (for/fold ([range range]) ([c (in-range from-c (add1 to-c))])
      (range-add* range c config limit-c))]))
