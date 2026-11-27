#lang racket/base
(require (only-in racket/extflonum extflonum?)
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../common/prefab.rkt"
         "../boot/runtime-primitive.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt"
         "require+provide.rkt")

;; Check whether a module fits the restricted grammar of a cross-phase
;; persistent module

(provide check-cross-phase-persistent-form
         make-module-phaseless!)

(define (check-cross-phase-persistent-form bodys self-mpi)

  (define (check-body bodys)
    (for ([body (in-list bodys)])
      (define p (if (expanded+parsed? body)
                    (expanded+parsed-parsed body)
                    body))
      (cond
        [(parsed-define-values? p)
         (check-expr (parsed-define-values-rhs p) (length (parsed-define-values-syms p)) p)]
        [(or (parsed-#%declare? p)
             (parsed-module? p)
             (syntax? p)) ;; remaining unparsed forms, such as `#%require` and `#%provide`, are ok
         (void)]
        [else
         (disallow p)])))

  (define (check-expr e num-results enclosing)
    (cond
      [(parsed-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-case-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-quote? e)
       (check-datum (parsed-quote-datum e) e)
       (check-count 1 num-results enclosing)]
      [(parsed-app? e)
       (define rands (parsed-app-rands e))
       (for ([rand (in-list rands)])
         (check-expr rand 1 e))
       (case (cross-phase-primitive-name (parsed-app-rator e))
         [(cons list hasheq)
          (check-count 1 num-results enclosing)]
         [(make-struct-type)
          (check-count 5 num-results enclosing)]
         [(make-struct-type-property)
          (check-count 3 num-results enclosing)]
         [(make-parameter)
          (check-count 1 num-results enclosing)]
         [(gensym)
          (unless (or (= 0 (length rands))
                      (and (= 1 (length rands))
                           (quoted-string? (car rands))))
            (disallow e))]
         [(string->uninterned-symbol)
          (unless (and (= 1 (length rands))
                       (quoted-string? (car rands)))
            (disallow e))]
         [else (disallow e)])]
      [else (check-no-disallowed-expr e)]))

  (define (check-no-disallowed-expr e)
    (cond
      [(parsed-lambda? e)
       (check-body-no-disallowed-expr (parsed-lambda-body e))]
      [(parsed-case-lambda? e)
       (for ([clause (in-list (parsed-case-lambda-clauses e))])
         (check-body-no-disallowed-expr (cadr clause)))]
      ;; explicitly allow (variable-reference-from-unsafe? (#%variable-reference))
      [(and (parsed-app? e)
            (eq? 'variable-reference-from-unsafe? (cross-phase-primitive-name (parsed-app-rator e)))
            (andmap parsed-#%variable-reference? (parsed-app-rands e)))
       (void)]
      [(parsed-app? e)
       (check-no-disallowed-expr (parsed-app-rator e))
       (for ([e (in-list (parsed-app-rands e))])
         (check-no-disallowed-expr e))]
      [(parsed-if? e)
       (check-no-disallowed-expr (parsed-if-tst e))
       (check-no-disallowed-expr (parsed-if-thn e))
       (check-no-disallowed-expr (parsed-if-els e))]
      [(parsed-set!? e)
       (define id (parsed-set!-id e))
       (define normal-b (parsed-id-binding id))
       (when (or (not normal-b)
                 (parsed-top-id? id)
                 (and (not (symbol? normal-b))
                      (eq? (module-binding-module normal-b) self-mpi)))
         (disallow e))
       (check-no-disallowed-expr (parsed-set!-rhs e))]
      [(parsed-with-continuation-mark? e)
       (check-no-disallowed-expr (parsed-with-continuation-mark-key e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-val e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-body e))]
      [(parsed-begin? e)
       (check-body-no-disallowed-expr (parsed-begin-body e))]
      [(parsed-begin0? e)
       (check-body-no-disallowed-expr (parsed-begin0-body e))]
      [(parsed-let_-values? e)
       (for ([clause (in-list (parsed-let_-values-clauses e))])
         (check-no-disallowed-expr (cadr clause)))
       (check-body-no-disallowed-expr (parsed-let_-values-body e))]
      [(or (parsed-quote-syntax? e)
           (parsed-#%variable-reference? e))
       (disallow e)]
      ;; Other forms have no subexpressions
      [else (void)]))

  (define (check-body-no-disallowed-expr l)
    (for ([e (in-list l)])
      (check-no-disallowed-expr e)))

  (check-body bodys))

(define (check-count is-num expected-num enclosing)
  (unless (= is-num expected-num)
    (disallow enclosing)))

; (walk-for-quotable d recur) -> (or/c (not/c #f) #f)
;   d : any/c
;   recur : (any/c . -> . (or/c (not/c #f) #f))
; If `d`'s outermost structure is quotable, then returns `(and (recur u) ...)`
; where `u ...` are the pieces of immediate substructure of d. (If `recur`
; encounters an unquotable expression, it can either return #f or throw; the
; walk-for-quotable invocation will do the same.)
; If `d`'s outermost structure is not quotable, returns #f.
(define (walk-for-quotable d recur)
  (or (number? d)
      (boolean? d)
      (symbol? d)
      (and (string? d) (immutable? d))
      (and (bytes? d) (immutable? d))
      (char? d)
      (keyword? d)
      (null? d)
      (regexp? d)
      (extflonum? d)
      (and (pair? d)
           (recur (car d))
           (recur (cdr d)))
      (and (vector? d)
           (immutable? d)
           (for/and ([v (in-vector d)]) (recur v)))
      (and (hash? d)
           (hash-eq? d)
           (immutable? d)
           (for/and ([(k v) (in-hash d)])
             (and (recur k) (recur v))))
      (and (box? d)
           (immutable? d)
           (recur (unbox d)))
      (and (immutable-prefab-struct-key d)
           (for/and ([v (in-vector (struct->vector d) 1)])
             (recur v)))))

; (check-datum datum expr) -> (or/c (not/c #f) #f)
;   datum : any/c
;   expr : (or/c parsed? syntax?)
; If datum is quotable, returns an unspecified not-#f value. Else, raises an
; error with `e` as the context.
(define (check-datum d e)
  (or (walk-for-quotable d
                         (lambda (d2)
                           (check-datum d2 e)))
      (disallow e)))

(define (quoted-string? e)
  (and (parsed-quote? e)
       (string? (parsed-quote-datum e))))

(define (cross-phase-primitive-name id)
  (cond
    [(parsed-id? id)
     (define b (parsed-id-binding id))
     (and (module-binding? b)
          (eq? runtime-module-name (module-path-index-resolve (module-binding-module b)))
          (module-binding-sym b))]
    [else #f]))

(define (disallow body)
  (raise-syntax-error 'module
                      "not allowed in a cross-phase persistent module"
                      (if (parsed? body)
                          (datum->syntax #f body (parsed-s body))
                          body)))

;; ----------------------------------------

;; Convert a module that is declared `#:unsafe-cross-phase-persistent`
;; so that its inside-edge scope is phaseless. The module can have
;; definitions only at phases 0 and 1, and no transformer definitions
;; at phase 1 (which would need phase-2 code). Imports are allowed at
;; any phase except the label phase, since an import does not imply
;; code at the import's phase. Converting to phaseless merges bindings
;; at all phases, so check that every module-level binding is the same
;; after conversion as before; that is, no symbol can be imported or
;; defined differently at different phases.
(define (make-module-phaseless! inside-scope r+p defined-syms s)
  (define (check-phase phase id-or-sym what)
    (unless (or (eqv? phase 0) (eqv? phase 1))
      (raise-syntax-error #f
                          (format (string-append "~a at ~a not allowed in an unsafe cross-phase persistent module\n"
                                                 "  name: ~a")
                                  what
                                  (cond
                                    [(not phase) "the label phase"]
                                    [else (format "phase ~a" phase)])
                                  (if (syntax? id-or-sym) (syntax-e id-or-sym) id-or-sym))
                          s
                          (and (syntax? id-or-sym) id-or-sym))))

  ;; Collect each binding site: sym -> list of (vector get-id phase source)
  (define sym-sites (make-hasheq))
  (define (add-site! sym get-id phase source)
    (hash-set! sym-sites sym (cons (vector get-id phase source) (hash-ref sym-sites sym null))))
  (requires+provides-for-each-required-id
   r+p
   (lambda (sym phase source get-id)
     ;; Imports at any phase are allowed, since they don't imply code
     ;; at that phase, but they are merged with other phases
     (when (label-phase? phase)
       (check-phase phase (get-id) "import"))
     (add-site! sym get-id phase source)))
  (requires+provides-for-each-defined-sym
   r+p
   (lambda (sym phase kind)
     (check-phase phase sym "definition")
     (when (and (eqv? phase 1) (eq? kind 'transformer))
       (raise-syntax-error #f
                           (format (string-append "macro definition at phase 1 not allowed in an unsafe"
                                                  " cross-phase persistent module\n"
                                                  "  name: ~a")
                                   sym)
                           s))))
  (for* ([(phase ht) (in-hash defined-syms)]
         [(sym id) (in-hash ht)])
    (add-site! (syntax-e id) (lambda () id) phase #f))

  ;; Merging phases can only change the binding of a site if the same
  ;; symbol has a binding site at some other phase, and only if the
  ;; sites can have different bindings; all sites for a symbol that
  ;; are bulk imports from the same provides table have the same
  ;; binding. So, record bindings before conversion only for other
  ;; sites, which avoids resolving (twice) every name imported from,
  ;; say, '#%kernel at both phase 0 and phase 1.
  (define sites
    (for*/list ([(sym l) (in-hash sym-sites)]
                #:unless (let ([site0 (car l)])
                           (or (for/and ([site (in-list (cdr l))])
                                 (eqv? (vector-ref site0 1) (vector-ref site 1)))
                               (let ([source0 (vector-ref site0 2)])
                                 (and source0
                                      (for/and ([site (in-list (cdr l))])
                                        (define source (vector-ref site 2))
                                        (and source
                                             (eq? (car source0) (car source))
                                             (eqv? (cdr source0) (cdr source))))))))
                [site (in-list l)])
      (define id ((vector-ref site 0)))
      (vector id (vector-ref site 1) (resolve+shift id (vector-ref site 1)))))

  (make-shifted-multi-scope-phaseless! inside-scope)

  ;; Check that bindings are unchanged
  (define bad-sites
    (for/list ([site (in-list sites)]
               #:unless (let ([before-b (vector-ref site 2)]
                              [after-b (resolve+shift (vector-ref site 0) (vector-ref site 1))])
                          (or (eq? before-b after-b)
                              (and before-b
                                   after-b
                                   (or (module-binding? before-b) (local-binding? before-b))
                                   (same-binding? before-b after-b)))))
      site))
  (unless (null? bad-sites)
    (define site (car bad-sites))
    (define id (vector-ref site 0))
    (define phase (vector-ref site 1))
    (define before-b (vector-ref site 2))
    (define after-b (resolve+shift id phase))
    (define all-names
      (let loop ([l bad-sites] [seen #hasheq()])
        (cond
          [(null? l) null]
          [else
           (define sym (syntax-e (vector-ref (car l) 0)))
           (if (hash-ref seen sym #f)
               (loop (cdr l) seen)
               (cons sym (loop (cdr l) (hash-set seen sym #t))))])))
    (raise-syntax-error #f
                        (string-append "binding differs across phases in an unsafe cross-phase persistent module;\n"
                                       " a phase-0 binding and a phase-1 binding with the same name must be the same")
                        s
                        id
                        null
                        (format "\n  phase of binding: ~a\n  binding at that phase: ~a\n  merged binding: ~a\n  all conflicting names:~a"
                                phase
                                (describe-binding before-b)
                                (describe-binding after-b)
                                (apply string-append
                                       (for/list ([sym (in-list all-names)])
                                         (format " ~a" sym)))))))

(define (describe-binding b)
  (cond
    [(not b) "none"]
    [(module-binding? b)
     (format "~a from ~a at phase level ~a"
             (module-binding-sym b)
             (let ([mpi (module-binding-module b)])
               (if (module-path-index? mpi)
                   (resolved-module-path-name (module-path-index-resolve mpi))
                   mpi))
             (module-binding-phase b))]
    [(local-binding? b) "local binding"]
    [else "ambiguous"]))
