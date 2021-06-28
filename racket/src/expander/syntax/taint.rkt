#lang racket/base
(require "syntax.rkt"
         "tamper.rkt"
         "../common/set.rkt")

(provide taint-content
         
         syntax-tainted?
         syntax-clean?
         syntax-taint

         struct-copy/t)

(define-syntax struct-copy/t
  (syntax-rules (syntax tamper)
    [(struct-copy/t syntax s [tamper v])
     (let* ([stx s]
            [t v]
            [content* (syntax-content* stx)]
            [content (if (modified-content? content*)
                         (modified-content-content content*)
                         content*)]
            [p (and (modified-content? content*)
                    (modified-content-scope-propagations+tamper content*))])
       (struct-copy syntax stx
                    [content*
                     (let ([new-p (if (tamper? p)
                                      t
                                      ((propagation-set-tamper-ref p) p t))])
                       (if new-p
                           (modified-content content new-p)
                           content))]))]))

(define (taint-content d)
  (non-syntax-map d
                  (lambda (tail? x) x)
                  (lambda (sub-s)
                    (cond
                     [(tamper-tainted? (syntax-tamper sub-s)) sub-s]
                     [else (struct-copy/t syntax sub-s
                                          [tamper
                                           (tamper-tainted-for-content (syntax-content sub-s))])]))))

(define (syntax-tainted? s)
  (tamper-tainted? (syntax-tamper s)))

(define (syntax-clean? s)
  (tamper-clean? (syntax-tamper s)))

(define (syntax-taint s)
  (if (tamper-tainted? (syntax-tamper s))
      s
      (struct-copy/t syntax s
                     [tamper (tamper-tainted-for-content (syntax-content s))])))
