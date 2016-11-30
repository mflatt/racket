#lang racket/base
(require "../common/set.rkt"
         "built-in-symbol.rkt"
         "self-quoting.rkt"
         "../host/correlate.rkt")

;; To support extraction of a bootstrapped version of the expander, we
;; need to be able to prune unused module content. Pruning is usefully
;; improved by a simple analysis of whether a module body has any
;; side-effects.

(provide any-side-effects?)

(define (any-side-effects? e ; compiled expression
                           expected-results ; number of expected reuslts, or #f if any number is ok
                           required-reference?
                           #:locals [locals #hasheq()]) ; allowed local variabes
  (define actual-results
    (let loop ([e e] [locals locals])
      (case (and (pair? (correlated-e e))
                 (correlated-e (car (correlated-e e))))
        [(quote lambda case-lambda #%variable-reference) 1]
        [(letrec-values let-values)
         (define-correlated-match m e '(_ ([ids rhs] ...) body))
         (and (not (for/or ([ids (in-list (m 'ids))]
                            [rhs (in-list (m 'rhs))])
                     (any-side-effects? rhs (correlated-length ids) required-reference?
                                        #:locals locals)))
              (loop (m 'body) (add-binding-info locals (m 'ids) (m 'rhs))))]
        [(values)
         (define-correlated-match m e '(_ e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (any-side-effects? e 1 required-reference? #:locals locals)))
              (length (m 'e)))]
        [(void)
         (define-correlated-match m e '(_ e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (any-side-effects? e 1 required-reference? #:locals locals)))
              1)]
        [(begin)
         (define-correlated-match m e '(_ e ...))
         (let bloop ([es (m 'e)])
           (cond
            [(null? es) #f]
            [(null? (cdr es)) (loop (car es) locals)]
            [else (and (not (any-side-effects? (car es) #f required-reference? #:locals locals))
                       (bloop (cdr es)))]))]
        [(begin0)
         (define-correlated-match m e '(_ e0 e ...))
         (and (for/and ([e (in-list (m 'e))])
                (not (any-side-effects? e #f required-reference? #:locals locals)))
              (loop (m 'e0) locals))]
        [(make-struct-type)
         (and (ok-make-struct-type? e required-reference?)
              5)]
        [(make-struct-field-accessor)
         (and (ok-make-struct-field-accessor/mutator? e locals 'accessor)
              1)]
        [(make-struct-field-mutator)
         (and (ok-make-struct-field-accessor/mutator? e locals 'mutator)
              1)]
        [(make-struct-type-property)
         (and (ok-make-struct-type-property? e)
              3)]
        [else
         (define v (correlated-e e))
         (and
          (or (self-quoting-in-linklet? v)
              (and (symbol? v)
                   (or (hash-ref locals v #f)
                       (built-in-symbol? v)
                       (required-reference? v))))
          1)])))
  (not (and actual-results
            (or (not expected-results)
                (= actual-results expected-results)))))

;; ----------------------------------------

(define-struct struct-op (type field-count) #:prefab)

(define (add-binding-info locals idss rhss)
  (for/fold ([locals locals]) ([ids (in-list idss)]
                               [rhs (in-list rhss)])
    (let loop ([rhs rhs])
      (case (and (pair? (correlated-e rhs))
                 (correlated-e (car (correlated-e rhs))))
        [(make-struct-type)
         ;; Record result "types"
         (define field-count (extract-struct-field-count-lower-bound rhs))
         (for/fold ([locals locals]) ([id (in-list (correlated->list ids))]
                                      [type (in-list '(struct-type
                                                       constructor
                                                       predicate
                                                       accessor
                                                       mutator))])
           (hash-set locals (correlated-e id) (struct-op type field-count)))]
        [(let-values)
         (if (null? (correlated-e (correlated-cadr rhs)))
             (loop (caddr (correlated->list rhs)))
             (loop #f))]
        [else
         (for/fold ([locals locals]) ([id (in-list (correlated->list ids))])
           (hash-set locals (correlated-e id) #t))]))))

;; ----------------------------------------

(define (ok-make-struct-type-property? e)
  (define l (correlated->list e))
  (and (or ((length l) . = . 3)
           ((length l) . = . 2))
       (for/and ([arg (cdr l)]
                 [pred (list
                        (lambda (v) (quoted? symbol? v))
                        (lambda (v) (is-lambda? v 2)))])
         (pred arg))))

;; ----------------------------------------

(define (ok-make-struct-type? e required-reference?)
  (define l (correlated->list e))
  (define init-field-count-expr (and ((length l) . > . 3)
                                     (list-ref l 3)))
  (define immutables-expr (or (and ((length l) . > . 9)
                                   (list-ref l 9))
                              'null))  
  (and ((length l) . >= . 5)
       ((length l) . <= . 12)
       (for/and ([arg (cdr l)]
                 [pred (list
                        (lambda (v) (quoted? symbol? v))
                        (lambda (v) (quoted? false? v))
                        (lambda (v) (field-count-expr-to-field-count v))
                        (lambda (v) (field-count-expr-to-field-count v))
                        (lambda (v) (not (any-side-effects? v 1 required-reference?)))
                        (lambda (v) (known-good-struct-properties? v immutables-expr))
                        (lambda (v) (inspector-or-false? v))
                        (lambda (v) (procedure-spec? v immutables-expr))
                        (lambda (v) (immutables-ok? v init-field-count-expr)))])
         (pred arg))))

(define (extract-struct-field-count-lower-bound e)
  ;; e is already checked by `ok-make-struct-type?`
  (define l (correlated->list e))
  (+ (field-count-expr-to-field-count (list-ref l 3))
     (field-count-expr-to-field-count (list-ref l 4))))

(define (quoted? val? v)
  (or (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'quote)
           (val? (correlated-e (correlated-cadr v))))
      (val? (correlated-e v))))

(define (quoted-value v)
  (if (pair? (correlated-e v))
      (correlated-e (correlated-cadr v))
      (correlated-e v)))

(define (false? v)
  (eq? (correlated-e v) #f))

(define (field-count-expr-to-field-count v)
  (and (quoted? exact-nonnegative-integer? v)
       (quoted-value v)))

(define (inspector-or-false? v)
  (or (quoted? false? v)
      (and (= 1 (correlated-length v))
           (eq? 'current-inspector (correlated-e (car (correlated-e v)))))))

(define (known-good-struct-properties? v immutables-expr)
  (or (quoted? null? v)
      (eq? 'null (correlated-e v))
      (and (pair? (correlated-e v))
           (eq? (correlated-e (car (correlated-e v))) 'list)
           (for/and ([prop+val (in-list (cdr (correlated->list v)))])
             (and (= (correlated-length prop+val) 3)
                  (let ([prop+val (correlated->list prop+val)])
                    (and (eq? 'cons (correlated-e (car prop+val)))
                         (known-good-struct-property+value? (list-ref prop+val 1)
                                                            (list-ref prop+val 2)
                                                            immutables-expr)))))
           ;; All properties must be distinct
           (= (sub1 (correlated-length v))
              (set-count (for/set ([prop+val (in-list (cdr  (correlated->list v)))])
                           (correlated-e (list-ref (correlated->list prop+val) 1))))))))

(define (known-good-struct-property+value? prop-expr val-expr immutables-expr)
  (define prop-name (correlated-e prop-expr))
  (case prop-name
    [(prop:evt) (or (is-lambda? val-expr 1)
                    (immutable-field? val-expr immutables-expr))]
    [(prop:procedure) (or (is-lambda? val-expr)
                          (immutable-field? val-expr immutables-expr))]
    [(prop:custom-write) (is-lambda? val-expr 3)]
    [(prop:method-arity-error) #t]
    [else #f]))

;; is expr a procedure of specified arity? (arity irrelevant if not specified)
(define (is-lambda? expr [arity #f])
  (and (pair? (correlated-e expr))
       (eq? 'lambda (car (correlated-e expr)))
       (or (not arity)
           (= arity (length (correlated->list (cadr (correlated->list expr))))))))
       
    

(define (immutable-field? val-expr immutables-expr)
  (and (quoted? exact-nonnegative-integer? val-expr)
       (memv (quoted-value val-expr)
             (immutables-expr-to-immutables immutables-expr null))))

(define (immutables-expr-to-immutables e fail-v)
  (case (and (pair? (correlated-e e))
             (correlated-e (car (correlated-e e))))
    [(quote)
     (define v (correlated-cadr e))
     (or (and (correlated-length v)
              (let ([l (map correlated-e (correlated->list v))])
                (and (andmap exact-nonnegative-integer? l)
                     (= (length l) (set-count (list->set l)))
                     l)))
         fail-v)]
    [else fail-v]))

(define (procedure-spec? e immutables-expr)
  (or (quoted? false? e)
      (and (quoted? exact-nonnegative-integer? e)
           (memv (quoted-value e)
                 (immutables-expr-to-immutables immutables-expr null)))))

(define (immutables-ok? e init-field-count-expr)
  (define l (immutables-expr-to-immutables e #f))
  (define c (field-count-expr-to-field-count init-field-count-expr))
  (and l
       (for/and ([n (in-list l)])
         (n . < . c))))

;; ----------------------------------------

(define (ok-make-struct-field-accessor/mutator? e locals type)
  (define l (correlated->list e))
  (define a (and (= (length l) 4)
                 (hash-ref locals (correlated-e (list-ref l 1)) #f)))
  (and (struct-op? a)
       (eq? (struct-op-type a) type)
       ((field-count-expr-to-field-count (list-ref l 2)) . < . (struct-op-field-count a))
       (quoted? symbol? (list-ref l 3))))

;; ----------------------------------------

(module+ test
  (define-syntax-rule (check expr result)
    (unless (equal? expr result)
      (error 'failed "~s" #'expr)))
  
  (define (any-side-effects?* e n)
    (define v1 (any-side-effects? e n (lambda (s) #f)))
    (define v2 (any-side-effects? (datum->correlated e) n (lambda (s) #f)))
    (unless (equal? v1 v2)
      (error "problem with correlated:" e))
    v1)
  
  (check (any-side-effects?* ''1 1)
         #f)

  (check (any-side-effects?* ''1 #f)
         #f)

  (check (any-side-effects?* '(lambda (x) x) 1)
         #f)
  
  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               '()
                               (current-inspector)
                               '0
                               '(0))
                             5)
         #f)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list
                                (cons prop:evt '0)
                                (cons prop:evt '0)) ; duplicate
                               (current-inspector)
                               '#f
                               '(0))
                             5)
         #t)

  (check (any-side-effects?* '(make-struct-type 'evt '#f '1 '0 '#f
                               (list (cons prop:evt '0))
                               (current-inspector)
                               '#f
                               '(1)) ; <- too big
                             5)
         #t))


