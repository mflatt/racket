#lang racket/base
(require "../common/set.rkt"
         "datum-map.rkt")

(provide tamper?
         tamper-tainted?
         tamper-clean?
         tamper-tainted-for-content
         tamper-needs-propagate?
         tamper-propagated

         serialize-tamper
         deserialize-tamper)

;; A tamper status is either
;;   * #f - clean
;;   * 'tainted - tainted
;;   * 'tainted/need-propagate - tainted, and taint needs to be propagated to children

(define (tamper? v)
  (or (not v) (symbol? v) (set? v)))

(define (tamper-tainted? v)
  (symbol? v))

(define (tamper-clean? v)
  (not v))

(define (tamper-tainted-for-content v)
  (if (datum-has-elements? v)
      'tainted/need-propagate
      'tainted))

(define (tamper-needs-propagate? t)
  (eq? t 'tainted/need-propagate))

(define (tamper-propagated t)
  (if (eq? t 'tainted/need-propagate)
      'tainted
      t))

;; ----------------------------------------

(define (serialize-tamper t) t)
(define (deserialize-tamper t) t)
