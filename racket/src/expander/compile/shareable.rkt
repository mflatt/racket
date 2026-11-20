#lang racket/base
(require "reserved-symbol.rkt"
         "built-in-symbol.rkt"
         "shareable-primitive.rkt"
         "../common/sharing-log.rkt"
         "../host/correlate.rkt")

;; A module's instance at a given phase level can be shared among
;; multiple instantiations (e.g., for different module expansions
;; that each need the module at phase 1) when running the phase
;; level's body creates no state that can be observed or changed by
;; one use and then seen by another. This module implements a
;; conservative analysis of a phase level's body to detect that case.
;;
;; At the module level (i.e., outside of any `lambda`), an expression
;; must not have side effects other than raising an exception. An
;; exception is ok, because it aborts the instantiation, and so no
;; partial instance will be shared. In particular, a module-level
;; expression can only call functions that are known to be "pure",
;; where a pure function is one whose body satisfies the same
;; constraints as module-level code (treating its arguments as opaque
;; values). Pure functions include constructors of structure types
;; that have no mutable fields, a few known primitives, and pure
;; functions exported from other modules (as recorded by this
;; analysis for those other modules).
;;
;; Anywhere, including inside `lambda`, `set!` is allowed only on a
;; variable that is bound within an enclosing `lambda`, so that the
;; mutable variable is fresh for each call of a module-level
;; function. Also, `#%variable-reference` is disallowed anywhere
;; (except as the argument to an operation like
;; `variable-reference-from-unsafe?`), since it could expose the
;; namespace of a specific instantiation.
;;
;; A programmer can assert that state created by an expression is ok
;; to share by wrapping the expression with `unsafe-assume-shareable`.
;;
;; Allocation of mutable values is allowed in the arguments of an
;; exception-raising function like `raise-argument-error`, since the
;; allocated values can only end up in an exception, which aborts the
;; instantiation.
;;
;; Structure types count as immutable when all fields are immutable,
;; or when the type's general mutator is never used (where "used"
;; includes being exported or mentioned in a syntax literal, so that
;; a macro cannot introduce a reference).
;;
;; The result of the analysis is a set of shareable phase levels and
;; information about exported variables that can be used to analyze
;; importing modules.

(provide analyze-shareable
         (struct-out shareable-record)
         unsafe-assume-shareable/fallback)

;; `unsafe-assume-shareable` is an identity function, exported from
;; '#%unsafe, that is recognized by the analysis: a module-level
;; `(unsafe-assume-shareable e)` is treated as shareable, which means
;; that the programmer promises that any state created by `e` (such as
;; a logger or a thread cell that is never mutated) is ok to share
;; across instantiations. It's normally a primitive, but this fallback
;; is used when bootstrapping with an older Racket that lacks it; the
;; name is different from the primitive's to avoid a collision in the
;; flattened expander.
(define (unsafe-assume-shareable/fallback v) v)

;; Logs the reason that a phase level is not shareable, where `e` is
;; the first expression that is not shareable
(define (log-unshareable log-name phase e)
  (when (log-sharing-enabled?)
    (define d (correlated->datum e))
    (define s (let ([o (open-output-string)])
                (write d o)
                (get-output-string o)))
    (log-sharing (vector log-name phase d)
                 "phase ~a of ~a is not shareable due to: ~a"
                 phase
                 (or log-name "module")
                 (if (> (string-length s) 400)
                     (string-append (substring s 0 400) "...")
                     s))))

;; Information collected for each compiled definition or expression:
(struct shareable-record (phase
                          expr              ; compiled expression
                          expected-results  ; #f or number
                          def-syms          ; #f or list of symbols (not correlated), for a variable definition
                          binding-syms      ; #f or list of symbols, for a variable definition
                          id-syms           ; #f or list of symbols, for a variable definition
                          import-lookup))   ; symbol -> #f or encoded known

;; ----------------------------------------
;; Abstract values

;; A function; `pure-promise` is a boolean or a thunk, and `clauses`
;; is #f or a list of `(vector min-args rest? fn)` for purity of each
;; `case-lambda` clause
(struct fn (pure-promise [clauses #:auto]) #:authentic #:auto-value #f #:mutable)
;; A structure type; `guards-pure` is a boolean or a thunk that reports
;; whether the guards (of the type and its supertypes) are pure
(struct st ([immutable? #:mutable] [guards-pure #:mutable]) #:authentic)
(struct prop (guard-pure?) #:authentic)      ; structure type property
(struct gmut (st) #:authentic)               ; general mutator for `st`

(define (st-guards-pure? s)
  (define p (st-guards-pure s))
  (cond
    [(procedure? p)
     ;; Guard to avoid infinite recursion; a guard cannot depend on the
     ;; structure type itself, anyway
     (set-st-guards-pure! s #f)
     (define v (and (p) #t))
     (set-st-guards-pure! s v)
     v]
    [else p]))

;; Other abstract values: 'value for a value that should not be called

;; Purity of a function is computed lazily, and a recursive reference
;; is optimistically assumed to be pure. A #t result that is computed
;; under the assumption that some in-progress function is pure is
;; tentative: if that assumption turns out to be wrong, then the
;; tentative results are reset. A #f result is always final, since
;; purity is monotonic in assumptions.
(struct frame (fn [assumed? #:mutable] [tentatives #:mutable])) ; tentatives: list of (cons fn thunk)
(define (make-fn-pure?)
  (define pure-stack '())
  (define (fn-pure? f)
    (define p (fn-pure-promise f))
    (cond
      [(eq? p 'in-progress)
       (for ([fr (in-list pure-stack)])
         (when (eq? f (frame-fn fr))
           (set-frame-assumed?! fr #t)))
       #t]
      [(procedure? p)
       (set-fn-pure-promise! f 'in-progress)
       (define fr (frame f #f null))
       (set! pure-stack (cons fr pure-stack))
       (define v (and (p) #t))
       (set! pure-stack (cdr pure-stack))
       (set-fn-pure-promise! f v)
       (define (propagate! l)
         (when (pair? pure-stack)
           (define parent (car pure-stack))
           (set-frame-tentatives! parent (append l (frame-tentatives parent)))))
       (cond
         [(and (not v) (frame-assumed? fr))
          ;; Results that depended on `f` being pure must be recomputed
          (for ([g+p (in-list (frame-tentatives fr))])
            (set-fn-pure-promise! (car g+p) (cdr g+p)))]
         [v (propagate! (cons (cons f p) (frame-tentatives fr)))]
         [else (propagate! (frame-tentatives fr))])
       v]
      [else p]))

  (define (fn-pure-for? f n)
    (define clauses (fn-clauses f))
    (cond
      [clauses
       ;; Use the first matching clause, like `case-lambda`
       (let loop ([clauses clauses])
         (cond
           [(null? clauses) #f]
           [else
            (define c (car clauses))
            (if (if (vector-ref c 1)
                    (n . >= . (vector-ref c 0))
                    (= n (vector-ref c 0)))
                (fn-pure? (vector-ref c 2))
                (loop (cdr clauses)))]))]
      [else (fn-pure? f)]))
  (values fn-pure? fn-pure-for?))

;; ----------------------------------------

;; Primitives that are pure for the purposes of sharing: no side
;; effect other than raising an exception, no calls to procedure
;; arguments or other user code (e.g., no `equal?`-based hashing),
;; and no new mutable values in the result. Information about
;; primitives that are implemented by the core (as opposed to the
;; expander) is generated from annotations in "../cs/primitive/*.ss";
;; see "../gen-shareable-primitive.rkt".

;; Pure primitives that are implemented by the expander:
(define expander-pure-primitives
  '(syntax? identifier? path-string?
    rename-transformer? set!-transformer? set!-transformer-procedure
    module-path-index? resolved-module-path?
    datum->syntax syntax-e syntax->datum syntax->list syntax-property syntax-property-remove
    syntax-source syntax-line syntax-column syntax-position syntax-span
    make-rename-transformer make-set!-transformer make-interned-syntax-introducer
    raise-syntax-error))

;; Structure-type properties implemented by the expander, all of which
;; have pure guards:
(define expander-properties
  '(prop:rename-transformer prop:set!-transformer prop:liberal-define-context
    prop:expansion-contexts prop:exn:missing-module))

;; Functions implemented by the expander that never return:
(define expander-raising-primitives
  '(raise-syntax-error))

(define pure-primitives
  (append shareable-primitives expander-pure-primitives))

;; Pure functions that never return:
(define raising-primitives
  (append shareable-raising-primitives expander-raising-primitives))

;; Primitives that allocate mutable values, but are otherwise pure;
;; allowed only in arguments to `raising-primitives`:
(define allocating-primitives
  shareable-for-error-primitives)

(define built-in-knowns
  (let* ([ht (for/hasheq ([sym (in-list pure-primitives)])
               (values sym (fn #t)))]
         ;; Pure only for specific argument counts, such as a parameter
         ;; that is pure to read but not to set:
         [ht (for/fold ([ht ht]) ([sym+ns (in-list shareable-arity-primitives)])
               (define f (fn #f))
               (set-fn-clauses! f (for/list ([n (in-list (cdr sym+ns))])
                                    (vector n #f (fn #t))))
               (hash-set ht (car sym+ns) f))]
         [ht (for/fold ([ht ht]) ([sym (in-list (append shareable-properties
                                                         expander-properties))])
               (hash-set ht sym (prop #t)))])
    ht))

;; ----------------------------------------

;; Returns two values:
;;  * a list of phase levels that are shareable
;;  * a sorted list of `(vector phase-level sym kind flag)` describing
;;    exported variables, where `kind` is 'fn, 'st, or 'prop, and
;;    `flag` is a boolean for pure, immutable, or pure-guard
(define (analyze-shareable records
                           #:log-name [log-name #f]
                           #:phases phases
                           #:provided-sym? provided-sym?
                           #:literal-sym? literal-sym?)
  (define phase-to-records
    (for/fold ([ht #hasheqv()]) ([r (in-list (reverse records))])
      (hash-update ht (shareable-record-phase r) (lambda (l) (cons r l)) null)))
  (define all-exports null)
  (define shareable-phases
    (for/list ([phase (in-list phases)]
               #:when (let ([rs (reverse (hash-ref phase-to-records phase null))])
                        (define-values (ok? exports) (analyze-phase phase rs provided-sym? literal-sym? log-name))
                        (when ok?
                          (set! all-exports (append exports all-exports)))
                        ok?))
      phase))
  (values shareable-phases
          (sort all-exports
                (lambda (a b)
                  (or (< (vector-ref a 0) (vector-ref b 0))
                      (and (= (vector-ref a 0) (vector-ref b 0))
                           (symbol<? (vector-ref a 1) (vector-ref b 1))))))))

(define (analyze-phase phase records provided-sym? literal-sym? log-name)
  (define-values (fn-pure? fn-pure-for?) (make-fn-pure?))
  (define defns (make-hasheq)) ; def-sym or import-sym -> abstract value
  (define pure-memo (make-hasheq)) ; lambda form -> fn

  ;; Lazily computed set of symbols referenced anywhere in the phase
  (define referenced-syms #f)
  (define (referenced-sym? sym)
    (unless referenced-syms
      (set! referenced-syms (make-hasheq))
      (for ([r (in-list records)])
        (collect-symbols! (shareable-record-expr r) referenced-syms)))
    (hash-ref referenced-syms sym #f))

  (define import-lookup (if (pair? records)
                            (shareable-record-import-lookup (car records))
                            (lambda (sym) #f)))

  (define (lookup sym env)
    (or (hash-ref env sym #f)
        (hash-ref defns sym #f)
        (let ([k (decode-known (import-lookup sym))])
          (and k
               (begin
                 (hash-set! defns sym k)
                 k)))
        (and (built-in-symbol? sym)
             (primitive-known sym))
        'value))

  ;; Maps a primitive reference to its name, so it can be recognized
  ;; by `case`; leaves any other symbol as-is
  (define (normalize-head sym env)
    (cond
      [(not (symbol? sym)) sym]
      [(hash-ref env sym #f) sym]
      [(hash-ref defns sym #f) sym]
      [(built-in-symbol? sym) sym]
      [else
       ;; An import from a primitive module may have been renamed
       ;; locally (see `select-fresh`), so use its external name
       (define enc (import-lookup sym))
       (if (and (vector? enc) (eq? 'primitive (vector-ref enc 0)))
           (vector-ref enc 1)
           sym)]))

  ;; Whether `check` is within the arguments of a `raising-primitives`
  ;; call, where allocation is allowed:
  (define allow-allocation? #f)

  ;; Checks an expression in module-level mode; returns a list of abstract
  ;; values for the results, or #f if the expression is not ok
  (define (check e env)
    (define v (correlated-e e))
    (cond
      [(pair? v)
       (define head (normalize-head (correlated-e (car v)) env))
       (define (args) (cdr (correlated->list e)))
       (case head
         [(quote) (list 'value)]
         [(lambda case-lambda)
          (and (closure-ok? e (lambda (sym) (normalize-head sym #hasheq())))
               (list (lambda->fn e env)))]
         [(let-values letrec-values)
          (define l (correlated->list e))
          (and (= 3 (length l))
               (let* ([clauses (for/list ([clause (in-list (correlated->list (cadr l)))])
                                 (define cl (correlated->list clause))
                                 (cons (map correlated-e (correlated->list (car cl)))
                                       (cadr cl)))]
                      [rec? (eq? head 'letrec-values)]
                      [env-box (box #f)]
                      [pre-env (if rec?
                                   (for*/fold ([env env]) ([clause (in-list clauses)]
                                                           [id (in-list (car clause))])
                                     (hash-set env id 'value))
                                   env)]
                      [pre-env (if rec?
                                   ;; Allow recursive functions; purity is computed
                                   ;; lazily, so it can use `env-box` that is
                                   ;; filled in below
                                   (for/fold ([env pre-env]) ([clause (in-list clauses)])
                                     (define rhs (cdr clause))
                                     (if (and (= 1 (length (car clause)))
                                              (lambda-form? rhs))
                                         (hash-set env (car (car clause)) (lambda->fn rhs env-box))
                                         env))
                                   pre-env)])
                 (set-box! env-box pre-env)
                 (define new-env
                   (for/fold ([new-env pre-env]) ([clause (in-list clauses)])
                     (and new-env
                          (let ([vals (check (cdr clause) (if rec? pre-env env))])
                            (and vals
                                 (bind-all new-env (car clause) vals
                                           (lambda (sym)
                                             ;; a local variable is "used" if it appears anywhere
                                             ;; in the `let` form other than the binding position,
                                             ;; except that a direct result of the body is checked
                                             ;; at the binding of the result
                                             (local-referenced? sym e))))))))
                 (when new-env
                   (set-box! env-box new-env))
                 (and new-env
                      (check (caddr l) new-env))))]
         [(begin begin-unsafe)
          (let loop ([es (args)])
            (cond
              [(null? es) #f]
              [(null? (cdr es)) (check (car es) env)]
              [else (and (check (car es) env)
                         (loop (cdr es)))]))]
         [(begin0)
          (define es (args))
          (and (pair? es)
               (let ([vals (check (car es) env)])
                 (and vals
                      (for/and ([e (in-list (cdr es))])
                        (check e env))
                      vals)))]
         [(if)
          (and (for/and ([e (in-list (args))])
                 (check-1 e env))
               (list 'value))]
         [(with-continuation-mark)
          (define es (args))
          (and (= 3 (length es))
               (check-1 (car es) env)
               (check-1 (cadr es) env)
               (check (caddr es) env))]
         [(values)
          (let loop ([es (args)])
            (cond
              [(null? es) null]
              [else
               (define v (check-1 (car es) env))
               (and v
                    (let ([r (loop (cdr es))])
                      (and r (cons v r))))]))]
         [(set! #%variable-reference #%foreign-inline) #f]
         [(unsafe-assume-shareable)
          ;; trust the programmer's assertion
          (and (= 1 (length (args)))
               (list 'value))]
         [(make-struct-type) (check-make-struct-type (args) env)]
         [(make-struct-type-property unsafe-make-struct-type-property/guard-calls-no-arguments)
          (check-make-struct-type-property (args) env)]
         [(apply)
          ;; Pure if the applied function is pure for any number of arguments
          (define as (args))
          (and (pair? as)
               (let ([f (check-1 (car as) env)])
                 (and (fn? f)
                      (fn-pure? f)))
               (all-ok? (cdr as) env)
               (list 'value))]
         [(vector)
          ;; An empty vector cannot be mutated
          (and (null? (args))
               (list 'value))]
         [(make-struct-field-accessor)
          (and (all-ok? (args) env)
               (list (fn #t)))]
         [(make-struct-field-mutator)
          (and (all-ok? (args) env)
               (list (fn #f)))]
         [else
          (cond
            [(and (eq? head get-syntax-literal!-id)
                  (= 1 (length (args))))
             (list 'value)]
            [(and (memq head allowed-variable-reference-consumers)
                  (= 1 (length (args)))
                  (allowed-variable-reference? (car (args))))
             (list 'value)]
            [else
             (define rator (if (symbol? (correlated-e (car v)))
                               (if (and allow-allocation?
                                        (memq head allocating-primitives))
                                   (fn #t)
                                   (lookup (correlated-e (car v)) env))
                               (check-1 (car v) env)))
             (and (fn? rator)
                  (fn-pure-for? rator (length (args)))
                  (if (memq head raising-primitives)
                      (let ([old allow-allocation?])
                        (set! allow-allocation? #t)
                        (begin0
                          (all-ok? (args) env)
                          (set! allow-allocation? old)))
                      (all-ok? (args) env))
                  (list 'value))])])]
      [(symbol? v) (list (lookup v env))]
      [else (list 'value)]))

  (define (check-1 e env)
    (define vals (check e env))
    (and vals
         (if (= 1 (length vals))
             (car vals)
             'value)))

  (define (all-ok? es env)
    (for/and ([e (in-list es)])
      (check-1 e env)))

  (define (lambda->fn e env)
    (or (hash-ref pure-memo e #f)
        (let* ([clauses
                (for/list ([clause (in-list (lambda-clauses e))])
                  (define-values (ids rest?) (formals->list+rest (car clause)))
                  (vector (length ids)
                          rest?
                          (fn (lambda ()
                                (define the-env (if (box? env) (unbox env) env))
                                (define body-env
                                  (for/fold ([env the-env]) ([id (in-list (formals->list (car clause)))])
                                    (hash-set env id 'value)))
                                ;; purity is cached, so it must not depend on context
                                (define old allow-allocation?)
                                (set! allow-allocation? #f)
                                (begin0
                                  (check (cdr clause) body-env)
                                  (set! allow-allocation? old))))))]
               [f (fn (lambda ()
                        (for/and ([c (in-list clauses)])
                          (fn-pure? (vector-ref c 2)))))])
          (set-fn-clauses! f clauses)
          (hash-set! pure-memo e f)
          f)))

  (define (check-make-struct-type args env)
    (define n (length args))
    (and (<= 4 n 11)
         (all-ok? args env)
         (let ()
           (define (arg i) (and (< i n) (list-ref args i)))
           (define super-v (check-1 (arg 1) env))
           (define super-ok? (or (quoted-false? (arg 1))
                                 (and (st? super-v)
                                      (st-immutable? super-v))))
           (define init-count (quoted-value (arg 2) exact-nonnegative-integer?))
           (define auto-count (quoted-value (arg 3) exact-nonnegative-integer?))
           (define immutables (if (arg 8)
                                  (quoted-value (arg 8) list?)
                                  null))
           (define props-good? (or (not (arg 5))
                                   (props-ok? (arg 5) env)))
           ;; A guard is called by the constructor, and so is a supertype's
           ;; guard, so they all must be pure for the constructor to be pure
           (define guard-v (and (arg 9)
                                (not (quoted-false? (arg 9)))
                                (check-1 (arg 9) env)))
           (define guards-pure
             (cond
               [(and (arg 1) (not (quoted-false? (arg 1)))
                     (not (and (st? super-v) (st-guards-pure? super-v))))
                #f]
               [(not guard-v) #t]
               [(fn? guard-v) (lambda () (fn-pure? guard-v))]
               [else #f]))
           (and props-good?
                (let* ([candidate? (and super-ok?
                                        (eqv? auto-count 0)
                                        init-count
                                        #t)]
                       [all-immutable? (and candidate?
                                            immutables
                                            (for/and ([i (in-range init-count)])
                                              (memv i immutables))
                                            #t)]
                       ;; If `candidate?` but not `all-immutable?`, the structure
                       ;; type becomes immutable if the general mutator is not used
                       [s (st all-immutable? guards-pure)])
                  (list s
                        (fn (lambda () (and (st-immutable? s) (st-guards-pure? s))))
                        (fn #t)
                        (fn #t)
                        (if candidate?
                            (gmut s)
                            (fn #f))))))))

  (define (props-ok? e env)
    ;; Each property's guard can be called when the structure type is
    ;; created, so the guard must be pure
    (define v (correlated-e e))
    (cond
      [(quoted-value e null?) #t]
      [(eq? v 'null) #t]
      [(and (pair? v) (eq? 'list (correlated-e (car v))))
       (for/and ([p+v (in-list (cdr (correlated->list e)))])
         (define l (and (pair? (correlated-e p+v)) (correlated->list p+v)))
         (and l
              (= 3 (length l))
              (eq? 'cons (correlated-e (car l)))
              (let ([p (check-1 (cadr l) env)])
                (and (prop? p)
                     (prop-guard-pure? p)))))]
      [else #f]))

  (define (check-make-struct-type-property args env)
    (define n (length args))
    (and (<= 1 n 7)
         (all-ok? args env)
         (let ([guard-pure?
                (and (or (< n 2)
                         (quoted-false? (list-ref args 1))
                         (let ([g (check-1 (list-ref args 1) env)])
                           (and (fn? g) (fn-pure? g))))
                     (or (< n 3)
                         (supers-ok? (list-ref args 2) env)))])
           (list (prop guard-pure?) (fn #t) (fn #t)))))

  (define (supers-ok? e env)
    (define v (correlated-e e))
    (cond
      [(quoted-value e null?) #t]
      [(eq? v 'null) #t]
      [(and (pair? v) (eq? 'list (correlated-e (car v))))
       (for/and ([p+v (in-list (cdr (correlated->list e)))])
         (define l (and (pair? (correlated-e p+v)) (correlated->list p+v)))
         (and l
              (= 3 (length l))
              (eq? 'cons (correlated-e (car l)))
              (let ([p (check-1 (cadr l) env)]
                    [f (check-1 (caddr l) env)])
                (and (prop? p)
                     (prop-guard-pure? p)
                     (fn? f)
                     (fn-pure? f)))))]
      [else #f]))

  ;; Process records in order
  (define ok?
    (for/and ([r (in-list records)])
      (define e (shareable-record-expr r))
      (define vals (check e #hasheq()))
      (unless vals (log-unshareable log-name phase e))
      (and vals
           (let ([def-syms (shareable-record-def-syms r)])
             (when def-syms
               (define env
                 (bind-all #hasheq() def-syms vals
                           (lambda (sym)
                             (define i (index-of sym def-syms))
                             (or (referenced-sym? sym)
                                 (provided-sym? (list-ref (shareable-record-binding-syms r) i))
                                 (literal-sym? (list-ref (shareable-record-id-syms r) i))))))
               (for ([(k v) (in-hash env)])
                 (hash-set! defns k v)))
             #t))))

  (values ok?
          (if ok?
              (for*/list ([r (in-list records)]
                          #:when (shareable-record-def-syms r)
                          [(def-sym binding-sym) (in-parallel
                                                  (in-list (shareable-record-def-syms r))
                                                  (in-list (shareable-record-binding-syms r)))]
                          [enc (in-value (encode-known (hash-ref defns def-sym #f) fn-pure?))]
                          #:when enc)
                (vector phase binding-sym (vector-ref enc 0) (vector-ref enc 1)))
              null)))

;; ----------------------------------------

;; Checks that closures do not contain `set!` on variables that are
;; bound outside of all `lambda`s, and no `#%variable-reference`
;; Binds `ids` to the abstract values `vals` in `env`, where `used?`
;; reports whether an id is used, which determines whether a general
;; mutator makes its structure type mutable
(define (bind-all env ids vals used?)
  (cond
    [(= (length ids) (length vals))
     (for/fold ([env env]) ([id (in-list ids)]
                            [val (in-list vals)])
       (when (and (gmut? val)
                  (not (used? id)))
         ;; The general mutator is unused, so the structure type is
         ;; effectively immutable
         (set-st-immutable?! (gmut-st val) #t))
       (hash-set env id val))]
    [else
     ;; will fail at run time
     (for/fold ([env env]) ([id (in-list ids)])
       (hash-set env id 'value))]))

(define (closure-ok? e [normalize (lambda (sym) sym)])
  ;; `in-lambda` is #f when outside of any `lambda`, otherwise it's a
  ;; hash table of variables bound within an enclosing `lambda`
  (let loop ([e e] [in-lambda #f])
    (define (loop-body e) (loop e in-lambda))
    (define v (correlated-e e))
    (cond
      [(pair? v)
       (define head (correlated-e (car v)))
       (case head
         [(quote) #t]
         [(#%variable-reference) #f]
         [(lambda case-lambda)
          (for/and ([clause (in-list (lambda-clauses e))])
            (loop (cdr clause) (for/fold ([ht (or in-lambda #hasheq())]) ([id (in-list (formals->list (car clause)))])
                                 (hash-set ht id #t))))]
         [(let-values letrec-values)
          (define l (correlated->list e))
          (and (= 3 (length l))
               (let* ([clauses (correlated->list (cadr l))]
                      [new-in-lambda (and in-lambda
                                          (for*/fold ([ht in-lambda]) ([clause (in-list clauses)]
                                                                       [id (in-list (correlated->list (car (correlated->list clause))))])
                                            (hash-set ht (correlated-e id) #t)))])
                 (and (for/and ([clause (in-list clauses)])
                        (loop (cadr (correlated->list clause)) new-in-lambda))
                      (loop (caddr l) new-in-lambda))))]
         [(set!)
          (define l (correlated->list e))
          (and (= 3 (length l))
               in-lambda
               (hash-ref in-lambda (correlated-e (cadr l)) #f)
               (loop-body (caddr l)))]
         [else
          (define l (correlated->list e))
          (cond
            [(and (memq (normalize head) allowed-variable-reference-consumers)
                  (= 2 (length l))
                  (allowed-variable-reference? (cadr l)))
             #t]
            [else
             (for/and ([e (in-list l)])
               (loop-body e))])])]
      [else #t])))

;; Operations on `(#%variable-reference)` that do not depend on a
;; specific instantiation; since an instance is shared only among
;; instantiations of the same module name, the module's name and
;; source are the same in each instantiation
(define allowed-variable-reference-consumers
  '(variable-reference-from-unsafe?
    variable-reference-constant?
    variable-reference->module-declaration-inspector
    variable-reference->module-source
    variable-reference->module-name
    variable-reference->resolved-module-path))

(define (allowed-variable-reference? e)
  (define v (correlated-e e))
  (and (pair? v)
       (eq? '#%variable-reference (correlated-e (car v)))))

;; ----------------------------------------

(define (lambda-form? e)
  (define v (correlated-e e))
  (and (pair? v)
       (memq (correlated-e (car v)) '(lambda case-lambda))
       #t))

;; Returns a list of (cons formals body)
(define (lambda-clauses e)
  (define l (correlated->list e))
  (case (correlated-e (car l))
    [(lambda) (list (cons (cadr l) (caddr l)))]
    [else (for/list ([clause (in-list (cdr l))])
            (define cl (correlated->list clause))
            (cons (car cl) (cadr cl)))]))

(define (formals->list+rest formals)
  (let loop ([formals (correlated-e formals)] [accum null])
    (cond
      [(pair? formals) (loop (correlated-e (cdr formals))
                             (cons (correlated-e (car formals)) accum))]
      [(null? formals) (values (reverse accum) #f)]
      [else (values (reverse accum) #t)])))

(define (formals->list formals)
  (let loop ([formals (correlated-e formals)])
    (cond
      [(pair? formals) (cons (correlated-e (car formals))
                             (loop (correlated-e (cdr formals))))]
      [(null? formals) null]
      [else (list (correlated-e formals))])))

(define (quoted-value e ok?)
  (define v (and e (correlated-e e)))
  (cond
    [(and (pair? v)
          (eq? 'quote (correlated-e (car v))))
     (define d (correlated->datum* (cadr (correlated->list e))))
     (and (ok? d) d)]
    [(and (not (symbol? v)) (not (pair? v)) (ok? v)) v]
    [else #f]))

(define (quoted-false? e)
  (define v (correlated-e e))
  (or (eq? v #f)
      (and (pair? v)
           (eq? 'quote (correlated-e (car v)))
           (eq? #f (correlated-e (cadr (correlated->list e)))))))

(define (collect-symbols! e ht)
  (let loop ([e e])
    (define v (correlated-e e))
    (cond
      [(pair? v)
       (unless (eq? 'quote (correlated-e (car v)))
         (loop (car v))
         (loop (cdr v)))]
      [(symbol? v) (hash-set! ht v #t)]
      [else (void)])))

;; Is `sym` referenced in the `let-values` or `letrec-values` form
;; `e`, other than in a binding position? Since binding symbols are
;; distinct, any reference counts --- except for a direct argument to
;; `values` in tail position, since that result is checked where it is
;; bound.
(define (local-referenced? sym e)
  (let loop ([e e] [tail? #t])
    (define v (correlated-e e))
    (cond
      [(pair? v)
       (define head (correlated-e (car v)))
       (case head
         [(quote) #f]
         [(let-values letrec-values)
          (define l (correlated->list e))
          (or (for/or ([clause (in-list (correlated->list (cadr l)))])
                (loop (cadr (correlated->list clause)) #f))
              (loop (caddr l) tail?))]
         [(lambda case-lambda)
          (for/or ([clause (in-list (lambda-clauses e))])
            (loop (cdr clause) #f))]
         [(values)
          (for/or ([e (in-list (cdr (correlated->list e)))])
            (if (and tail? (symbol? (correlated-e e)))
                #f
                (loop e #f)))]
         [else
          (let lloop ([v v])
            (cond
              [(pair? v) (or (loop (car v) #f) (lloop (correlated-e (cdr v))))]
              [(null? v) #f]
              [else (loop v #f)]))])]
      [(symbol? v) (eq? v sym)]
      [else #f])))

(define (index-of sym l)
  (let loop ([l l] [i 0])
    (if (eq? sym (car l))
        i
        (loop (cdr l) (add1 i)))))

(define (correlated->datum* e)
  (let loop ([e e])
    (cond
      [(pair? e) (cons (loop (car e)) (loop (cdr e)))]
      [(correlated? e) (loop (correlated-e e))]
      [else e])))


;; ----------------------------------------

(define (encode-known v fn-pure?)
  (cond
    [(fn? v) (vector 'fn (or (fn-pure? v)
                             (let ([clauses (fn-clauses v)])
                               (and clauses
                                    (for/or ([c (in-list clauses)])
                                      (fn-pure? (vector-ref c 2)))
                                    (for/list ([c (in-list clauses)])
                                      (vector (vector-ref c 0)
                                              (vector-ref c 1)
                                              (fn-pure? (vector-ref c 2))))))))]
    [(st? v) (vector 'st (and (st-immutable? v)
                              (if (st-guards-pure? v) #t 'impure-guard)))]
    [(prop? v) (vector 'prop (prop-guard-pure? v))]
    [(gmut? v) (vector 'fn #f)]
    [else #f]))

(define (decode-known enc)
  (and (vector? enc)
       (= 2 (vector-length enc))
       (let ([flag (vector-ref enc 1)])
         (case (vector-ref enc 0)
           [(fn) (cond
                   [(pair? flag)
                    (define f (fn #f))
                    (set-fn-clauses! f (for/list ([c (in-list flag)])
                                         (vector (vector-ref c 0) (vector-ref c 1)
                                                 (fn (and (vector-ref c 2) #t)))))
                    f]
                   [else (fn (and flag #t))])]
           [(st) (st (and flag #t) (eq? flag #t))]
           [(prop) (prop (and flag #t))]
           [(primitive) (primitive-known flag)]
           [else #f]))))

(define (primitive-known sym)
  (hash-ref built-in-knowns sym #f))

;; ----------------------------------------

(module+ test
  (for ([sym (in-list '(cons list make-hash box vector make-parameter current-namespace
                             make-struct-type make-struct-type-property make-struct-field-accessor
                             make-struct-field-mutator prop:procedure variable-reference-from-unsafe?
                             variable-reference->namespace raise-argument-error car
                             current-inspector current-logger make-logger unsafe-assume-shareable
                             string-append apply regexp make-impersonator-property
                             variable-reference->module-source))])
    (register-built-in-symbol! sym))

  ;; Each body is a list of `(define-values (id ...) rhs)` or expressions;
  ;; `imports` maps import symbols to encoded info
  (define (analyze body #:imports [imports #hasheq()] #:provided [provided '()])
    (define records
      (for/list ([form (in-list body)])
        (define defn? (and (pair? form) (eq? 'define-values (car form))))
        (shareable-record 0
                          (if defn? (caddr form) form)
                          (and defn? (length (cadr form)))
                          (and defn? (cadr form))
                          (and defn? (cadr form))
                          (and defn? (cadr form))
                          (lambda (sym) (hash-ref imports sym #f)))))
    (define-values (phases knowns)
      (analyze-shareable (reverse records)
                         #:phases '(0)
                         #:provided-sym? (lambda (sym) (memq sym provided))
                         #:literal-sym? (lambda (sym) #f)))
    (values (equal? phases '(0))
            (for/hasheq ([k (in-list knowns)])
              (values (vector-ref k 1) (vector (vector-ref k 2) (vector-ref k 3))))))
  (define (shareable? body #:imports [imports #hasheq()] #:provided [provided '()])
    (define-values (ok? knowns) (analyze body #:imports imports #:provided provided))
    ok?)
  (define (known-of id body #:imports [imports #hasheq()])
    (define-values (ok? knowns) (analyze body #:imports imports))
    (hash-ref knowns id #f))

  (define-syntax-rule (check expr result)
    (unless (equal? expr result)
      (error 'failed "~s" 'expr)))

  ;; Functions, literals, and syntax literals are fine
  (check (shareable? `((define-values (f) (lambda (x) (cons x x)))
                       (define-values (s) (,get-syntax-literal!-id 0))
                       (define-values (q) '#(1 2))))
         #t)
  ;; Creating mutable state is not
  (check (shareable? '((define-values (h) (make-hash)))) #f)
  (check (shareable? '((define-values (p) (make-parameter #f)))) #f)
  (check (shareable? '((define-values (v) (vector 1 2)))) #f)
  ;; Hidden state via `set!` on a variable outside a `lambda`
  (check (shareable? '((define-values (n) '0)
                       (define-values (inc!) (lambda () (set! n (car (cons n n)))))))
         #f)
  (check (shareable? '((define-values (inc!) (let-values ([(n) '0]) (lambda () (set! n n))))))
         #f)
  ;; ... but `set!` on a `lambda`-local variable is ok
  (check (shareable? '((define-values (f) (lambda (x) (begin (set! x (cons x x)) x)))))
         #t)
  ;; A function that creates a closure with state is not pure
  (check (shareable? '((define-values (make-counter) (lambda (n) (lambda () (set! n n) n)))
                       (define-values (c) (make-counter '0))))
         #f)
  ;; `#%variable-reference` could expose the namespace
  (check (shareable? '((define-values (f) (lambda () (variable-reference->namespace (#%variable-reference))))))
         #f)
  (check (shareable? '((define-values (f) (lambda () (variable-reference-from-unsafe? (#%variable-reference))))))
         #t)
  ;; Calling a pure function at the module level is ok
  (check (shareable? '((define-values (f) (lambda (x) (list x x)))
                       (define-values (y) (f '1))))
         #t)
  ;; Calling an impure function is not
  (check (shareable? '((define-values (f) (lambda (x) (make-hash)))
                       (define-values (y) (f '1))))
         #f)
  ;; Calling an argument makes a function impure
  (check (shareable? '((define-values (f) (lambda (g) (g)))
                       (define-values (y) (f make-hash))))
         #f)
  ;; Recursion is ok, but not if impure somewhere in a cycle
  (check (shareable? '((define-values (y) (letrec-values ([(loop) (lambda (n) (loop (car n)))])
                                            (loop '1)))))
         #t)
  (check (shareable? '((define-values (y) (letrec-values ([(a) (lambda (n) (b n))]
                                                          [(b) (lambda (n) (begin (make-hash) (a n)))])
                                            (a '1)))))
         #f)
  ;; `case-lambda` purity is per clause, using the first matching clause
  (check (shareable? '((define-values (f) (case-lambda [(x) (make-hash)] [(x y) (cons x y)]))
                       (define-values (y) (f '1 '2))))
         #t)
  (check (shareable? '((define-values (f) (case-lambda [(x) (make-hash)] [(x y) (cons x y)]))
                       (define-values (y) (f '1))))
         #f)
  (check (shareable? '((define-values (f) (case-lambda [(x . r) (make-hash)] [(x y) (cons x y)]))
                       (define-values (y) (f '1 '2))))
         #f)
  ;; Structure types
  (define immutable-struct
    '(define-values (struct:p make-p p? p-ref p-set!)
       (make-struct-type 'p #f 2 0 #f null (current-inspector) #f '(0 1))))
  (define mutable-struct
    '(define-values (struct:p make-p p? p-ref p-set!)
       (make-struct-type 'p #f 2 0 #f null (current-inspector) #f '(0))))
  (check (shareable? (list immutable-struct '(define-values (x) (make-p '1 '2)))) #t)
  (check (known-of 'make-p (list immutable-struct)) (vector 'fn #t))
  (check (known-of 'struct:p (list immutable-struct)) (vector 'st #t))
  ;; Mutable field, but the general mutator is not used:
  (check (shareable? (list mutable-struct '(define-values (x) (make-p '1 '2)))) #t)
  ;; Mutable field with a used mutator:
  (check (shareable? (list mutable-struct
                           '(define-values (x) (make-p '1 '2))
                           '(define-values (f) (lambda (v) (p-set! v 1 '3)))))
         #f)
  (check (shareable? (list mutable-struct '(define-values (x) (make-p '1 '2))) #:provided '(p-set!))
         #f)
  ;; Creating a mutable structure type is fine, as long as no instance is created
  (check (shareable? (list mutable-struct '(define-values (f) (lambda (v) (p-set! v 1 '3)))))
         #t)
  ;; Reading a parameter is ok, but setting is not
  (check (shareable? '((define-values (i) (current-inspector)))) #t)
  (check (shareable? '((define-values (i) (current-inspector (current-inspector))))) #f)
  ;; Loggers and other state are allowed only with `unsafe-assume-shareable`
  (check (shareable? '((define-values (l) (make-logger 'x (current-logger))))) #f)
  (check (shareable? '((define-values (l) (unsafe-assume-shareable (make-logger 'x (current-logger)))))) #t)
  ;; Allocation is ok in arguments to a raising function
  (check (shareable? '((define-values (f) (lambda (x) (if (car x) x (raise-argument-error 'f (string-append "a" "b") x))))
                       (define-values (y) (f '(1)))))
         #t)
  (check (shareable? '((define-values (f) (lambda (x) (string-append "a" "b")))
                       (define-values (y) (f '(1)))))
         #f)
  ;; `apply` of a pure function
  (check (shareable? '((define-values (f) (lambda (x . r) (apply list x r)))
                       (define-values (y) (f '1 '2))))
         #t)
  (check (shareable? '((define-values (f) (lambda (g . r) (apply g r)))
                       (define-values (y) (f make-hash))))
         #f)
  ;; Information from imports
  (check (shareable? '((define-values (y) (imp-f '1)))
                     #:imports (hasheq 'imp-f (vector 'fn #t)))
         #t)
  (check (shareable? '((define-values (y) (imp-f '1)))
                     #:imports (hasheq 'imp-f (vector 'fn #f)))
         #f)
  (check (shareable? '((define-values (y) (imp-f '1)))
                     #:imports (hasheq 'imp-f (vector 'fn (list (vector 1 #f #t) (vector 2 #f #f)))))
         #t)
  (check (shareable? '((define-values (y) (imp-f '1 '2)))
                     #:imports (hasheq 'imp-f (vector 'fn (list (vector 1 #f #t) (vector 2 #f #f)))))
         #f)
  (check (shareable? '((define-values (y) (imp-car '(1))))
                     #:imports (hasheq 'imp-car (vector 'primitive 'car)))
         #t)
  ;; A structure type with an imported immutable super type
  (check (known-of 'make-q
                   '((define-values (struct:q make-q q? q-ref q-set!)
                       (make-struct-type 'q imp-st 1 0 #f null (current-inspector) #f '(0))))
                   #:imports (hasheq 'imp-st (vector 'st #t)))
         (vector 'fn #t))
  (check (known-of 'make-q
                   '((define-values (struct:q make-q q? q-ref q-set!)
                       (make-struct-type 'q imp-st 1 0 #f null (current-inspector) #f '(0))))
                   #:imports (hasheq 'imp-st (vector 'st #f)))
         (vector 'fn #f))
  ;; A property guard must be pure, since it's called when a structure type is created
  (check (shareable? '((define-values (prop:x x? x-ref)
                         (make-struct-type-property 'x (lambda (v info) (make-hash))))
                       (define-values (struct:p make-p p? p-ref p-set!)
                         (make-struct-type 'p #f 1 0 #f (list (cons prop:x '1)) (current-inspector) #f '(0)))))
         #f)
  (check (shareable? '((define-values (prop:x x? x-ref)
                         (make-struct-type-property 'x (lambda (v info) (car (cons v v)))))
                       (define-values (struct:p make-p p? p-ref p-set!)
                         (make-struct-type 'p #f 1 0 #f (list (cons prop:x '1)) (current-inspector) #f '(0)))))
         #t)

  ;; Guards
  (define (guarded guard)
    `((define-values (struct:p make-p p? p-ref p-set!)
        (make-struct-type 'p #f 1 0 #f null (current-inspector) #f '(0) ,guard))
      (define-values (x) (make-p 1))))
  (check (shareable? (guarded #f)) #t)
  (check (shareable? (guarded '(lambda (v name) v))) #t)
  (check (shareable? (guarded '(lambda (v name) (begin (box v) v)))) #f)
  ;; A supertype's guard is also called
  (define (sub-guarded guard)
    `((define-values (struct:p make-p p? p-ref p-set!)
        (make-struct-type 'p #f 1 0 #f null (current-inspector) #f '(0) ,guard))
      (define-values (struct:q make-q q? q-ref q-set!)
        (make-struct-type 'q struct:p 0 0 #f null (current-inspector) #f '()))
      (define-values (x) (make-q 1))))
  (check (shareable? (sub-guarded '(lambda (v name) v))) #t)
  (check (shareable? (sub-guarded '(lambda (v name) (begin (box v) v)))) #f)
  ;; Imported structure type with an impure guard
  (check (shareable? '((define-values (struct:q make-q q? q-ref q-set!)
                         (make-struct-type 'q struct:p 0 0 #f null (current-inspector) #f '()))
                       (define-values (x) (make-q 1)))
                     #:imports (hasheq 'struct:p (vector 'st 'impure-guard)))
         #f)
  (check (shareable? '((define-values (struct:q make-q q? q-ref q-set!)
                         (make-struct-type 'q struct:p 0 0 #f null (current-inspector) #f '()))
                       (define-values (x) (make-q 1)))
                     #:imports (hasheq 'struct:p (vector 'st #t)))
         #t)

  ;; Argument-count-specific purity
  (check (shareable? '((define-values (rx) (regexp "a")))) #t)
  (check (shareable? '((define-values (rx) (regexp "a" (lambda (s) (box s)))))) #f)

  ;; Multiple results from a pure primitive
  (check (shareable? '((define-values (p p? p-ref) (make-impersonator-property 'p)))) #t)

  ;; Instantiation-independent uses of `#%variable-reference`
  (check (shareable? '((define-values (src) (variable-reference->module-source (#%variable-reference))))) #t)
  (check (shareable? '((define-values (ns) (variable-reference->namespace (#%variable-reference))))) #f)

  ;; An empty vector is immutable
  (check (shareable? '((define-values (v) (vector)))) #t)
  (check (shareable? '((define-values (v) (vector 1)))) #f))
