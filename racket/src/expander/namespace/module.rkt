#lang racket/base
(require "../common/phase.rkt"
         "../common/phase+space.rkt"
         "../common/small-hash.rkt"
         "../common/performance.rkt"
         "../syntax/bulk-binding.rkt"
         "../syntax/module-binding.rkt"
         "../common/module-path.rkt"
         "../compile/module-use.rkt"
         "../expand/root-expand-context.rkt"
         "../expand/portal-syntax.rkt"
         "../host/linklet.rkt"
         "../common/sharing-log.rkt"
         "namespace.rkt"
         "provided.rkt"
         "registry.rkt"
         (submod "namespace.rkt" for-module))

(provide make-module-namespace
         raise-unknown-module-error

         namespace->module-instance
         namespace->module-namespace
         namespace-copy-shifted-requires!
         namespace-install-module-namespace!
         namespace-record-module-instance-attached!
         module-force-bulk-binding!
         module-instantiates-at-level?
         
         build-module-instance-shifted-requires
         namespace-save-shifted-requires!
         
         namespace->module-linklet-info
         (struct-out module-linklet-info)
         
         make-module
         declare-module!
         module-self
         module-requires
         module-recur-requires
         module-flattened-requires
         module-provides
         module-primitive?
         module-is-predefined?
         module-cross-phase-persistent?
         module-no-protected?
         module-inspector
         module-submodule-names
         module-supermodule-name
         module-get-all-variables
         module-access
         module-compute-access!
         module-realm
         module-shareable-known
         
         module-instance-namespace
         module-instance-module
         module-instance-shifted-requires

         namespace-module-instantiate!
         namespace-module-visit!
         namespace-module-make-available!
         namespace-primitive-module-visit!
         namespace-visit-available-modules!
         namespace-run-available-modules!

         namespace-module-get-portal-syntax-lookup

         namespace-module-use->module+linklet-instances

         shared-instance-install!
         shared-instance-record!
         log-shared-instance-miss)

(module+ for-module-reflect
  (provide (struct-out module)))

;; ----------------------------------------

(struct module (source-name     ; #f, symbol, or complete path
                self            ; module path index used for a self reference
                requires        ; list of (cons phase list-of-module-path-index)
                recur-requires  ; list of (list boolean ...) in parallel to `requires`
                flattened-requires ; #f or list of (vector mpi (list phase ...)), which is flattened version of `requires`
                provides        ; phase-level -> sym -> binding or (provided binding bool bool); see [*] below
                [access #:mutable] ; phase-level -> sym -> 'provided or 'protected; computed on demand from `provides`
                language-info   ; #f or vector
                realm           ; symbol
                min-phase-level ; phase-level
                max-phase-level ; phase-level
                phase-level-linklet-info-callback ; phase-level namespace -> module-linklet-info-or-#f
                force-bulk-binding ; bulk-binding-registry -> any
                prepare-instance  ; box namespace phase-shift bulk-binding-registry inspector -> any
                instantiate-phase ; box namespace phase-shift phase-level bulk-binding-registry inspector -> any
                primitive?      ; inline variable values in compiled code?
                is-predefined?  ; always defined on startup?
                cross-phase-persistent?
                no-protected?   ; short cut for checking protected access
                inspector       ; declaration-time inspector
                submodule-names ; associated submodules (i.e, when declared together)
                supermodule-name ; associated supermodule (i.e, when declared together)
                get-all-variables ; for `module->indirect-exports`
                get-portal-syntax-callback ; for `identifier-binding-portal-syntax`
                shareable-phase-levels ; list of phase levels whose instances can be shared
                shareable-knowns ; phase-level -> sym -> encoded info for "../compile/shareable.rkt"
                instance-cache) ; #f or ephemeron module-registry -> phase-shift -> list of shared-instance; see [**] below
  #:authentic)

;; [*] Beware that tables in `provides` may map non-interned symbols
;;     to provided bindings, in case something like a lifted
;;     identifier was provided. Since lifting generates a locally
;;     deterministic unreadable symbol that is intended to be specific
;;     to a particular module, `require`ing unreadable symbols can
;;     create collisions. Still, the provided binding is supposed to
;;     be accessible via `dynamic-require`.

;; [**] When a module's body has no state (as determined by the
;;      compiler), then instantiations of the module with the same
;;      phase shift can share the same underlying instances of each
;;      phase level, as long as the instances of imported variables
;;      are also the same --- which typically means that the imported
;;      modules are similarly shared. This sharing is useful, for
;;      example, to avoid re-instantiating the same phase-1 modules for
;;      each separate module expansion. Sharing is limited to
;;      instances whose module path index is not relative to the
;;      "self" of some module (such as a module being expanded), so
;;      that syntax literals in a shared instance refer to modules in
;;      a way that is independent of the instantiation context.
;;      Furthermore, sharing is limited to instances whose module path
;;      index has the same shape (i.e., the same chain of module
;;      paths), so that syntax literals serialize the same as for an
;;      unshared instance; that way, compiled code is independent of
;;      whether and how instances have been shared. Each
;;      instance-specific `module-instance` record refers to a
;;      `shared-instance` record for shared information.
(struct shared-instance (name                  ; resolved module path
                         mpi-shape             ; list of module paths in `mpi` chain
                         mpi                   ; module path index for all sharing instances
                         data-box              ; shared `module-instance-data-box`
                         phase-levels          ; phase levels that can be shared
                         levels                ; mutable phase-level -> (cons definitions import-instances)
                         by-phase              ; cache table that holds this record
                         phase-shift           ; key in `by-phase`
                         [last-use #:mutable]) ; tick from the module registry, for eviction
  #:authentic)

;; Retaining shared instances keeps them alive for as long as the
;; module registry is reachable, and instances accumulate across
;; module expansions (at various phase shifts), so the number cached
;; per module registry is limited; when the limit is exceeded, the
;; least-recently used half of the cached instances are dropped from
;; the cache (but instances that still refer to them continue to use
;; them)
(define shared-instance-limit 4000)

(define (registry-tick! r)
  (define t (add1 (module-registry-shared-tick r)))
  (set-module-registry-shared-tick! r t)
  t)

(define (register-shared-instance! r shared)
  (define all (module-registry-shared-instances r))
  (hash-set! all shared #t)
  (when ((hash-count all) . > . shared-instance-limit)
    (define sorted (sort (hash-keys all) (lambda (a b)
                                          (< (shared-instance-last-use a)
                                             (shared-instance-last-use b)))))
    (for ([shared (in-list sorted)]
          [i (in-range (- (hash-count all) (quotient shared-instance-limit 2)))])
      (hash-remove! all shared)
      (define by-phase (shared-instance-by-phase shared))
      (define phase-shift (shared-instance-phase-shift shared))
      (define l (remq shared (hash-ref by-phase phase-shift null)))
      (if (null? l)
          (hash-remove! by-phase phase-shift)
          (hash-set! by-phase phase-shift l)))))

(struct module-linklet-info (linklet-or-instance ; #f, linklet, or instance supplied for cross-linking optimization
                             module-uses         ; #f or vector for linklet's imports
                             self                ; self modidx
                             inspector           ; declaration-time inspector
                             extra-inspector     ; optional extra inspector
                             extra-inspectorsss) ; optional extra inspector sets per variable per import
  #:authentic
  #:transparent)

(define (make-module #:source-name [source-name #f]
                     #:self self
                     #:requires [requires null]
                     #:recur-requires [recur-requires (for/list ([phase+mps (in-list requires)])
                                                        (for/list ([mp (in-list (cdr phase+mps))])
                                                          #t))]
                     #:flattened-requires [flattened-requires #f]
                     #:provides provides
                     #:min-phase-level [min-phase-level 0]
                     #:max-phase-level [max-phase-level 0]
                     #:instantiate-phase-callback instantiate-phase
                     #:force-bulk-binding-callback [force-bulk-binding void]
                     #:prepare-instance-callback [prepare-instance void]
                     #:phase-level-linklet-info-callback [phase-level-linklet-info-callback
                                                          (lambda (phase-level ns insp) #f)]
                     #:language-info [language-info #f]
                     #:realm [realm 'racket]
                     #:primitive? [primitive? #f]
                     #:predefined? [predefined? #f]
                     #:cross-phase-persistent? [cross-phase-persistent? primitive?]
                     #:no-protected? [no-protected? #f]
                     #:submodule-names [submodule-names null]
                     #:supermodule-name [supermodule-name #f]
                     #:get-all-variables [get-all-variables (lambda () null)] ; ok to omit exported
                     #:get-portal-syntax-callback [get-portal-syntax-callback (lambda (data-box phase sym) #f)]
                     #:shareable-phase-levels [shareable-phase-levels null]
                     #:shareable-knowns [shareable-knowns #hasheqv()])
  (module source-name
          self
          requires
          recur-requires
          flattened-requires
          provides
          #f ; access
          language-info
          realm
          min-phase-level max-phase-level
          phase-level-linklet-info-callback
          force-bulk-binding
          prepare-instance
          instantiate-phase
          primitive?
          predefined?
          cross-phase-persistent?
          no-protected?
          (current-code-inspector)
          submodule-names
          supermodule-name
          get-all-variables
          get-portal-syntax-callback
          shareable-phase-levels
          shareable-knowns
          (and (pair? shareable-phase-levels)
               (not cross-phase-persistent?)
               ;; Keyed by module registry (which corresponds to a bulk-binding
               ;; registry), and ephemeron-based so that cached instances
               ;; can be reclaimed when the module registry is unreachable
               ;; (while the module declaration may remain reachable via
               ;; other module registries):
               (make-ephemeron-hasheq))))

(define (module-shareable-known m phase-level sym)
  (hash-ref (hash-ref (module-shareable-knowns m) phase-level #hasheq()) sym #f))

;; A `module-instance` represents the instantiation of a module in a particular
;; namespace. If a module instance is attached from one namespace to another,
;; it's really the `namespace` within the instance that is shared, not the
;; `module-instance` object.
(struct module-instance (namespace
                         module                        ; can be #f for the module being expanded
                         [shifted-requires #:mutable]  ; computed on demand; shifted from `module-requires` or `module-flattened-requires`
                         phase-level-to-state          ; phase-level -> #f, 'available, or 'started
                         [made-available? #:mutable]   ; no #f in `phase-level-to-state`?
                         [attached? #:mutable]         ; whether the instance has been attached elsewhere
                         data-box                      ; for use by module implementation
                         portal-syntaxes               ; for a module being expanded 
                         shared)                       ; #f or `shared-instance`
  #:authentic)

(define (make-module-instance m-ns m portal-syntaxes [shared #f])
  (module-instance m-ns           ; namespace
                   m              ; module
                   #f             ; shifted-requires (not yet computed)
                   (make-small-hasheqv) ; phase-level-to-state
                   #f             ; made-available?
                   #f             ; attached?
                   (if shared     ; data-box
                       (shared-instance-data-box shared)
                       (box #f))
                   portal-syntaxes
                   shared))

;; ----------------------------------------

;; Create a namespace for expanding a module
(define (make-module-namespace ns
                               #:mpi name-mpi
                               #:root-expand-context root-expand-ctx
                               #:for-submodule? for-submodule?
                               #:portal-syntaxes portal-syntaxes)
  (define phase 0) ; always start at 0 when compiling a module
  (define name (module-path-index-resolve name-mpi))
  (define m-ns
    ;; Keeps all module declarations, but makes a fresh space of instances
    (struct-copy namespace (new-namespace ns
                                          #:root-expand-ctx root-expand-ctx
                                          #:register? #f)
                 [mpi name-mpi]
                 [source-name (resolved-module-path-root-name name)]
                 [phase phase]
                 [0-phase phase]
                 [submodule-declarations (if for-submodule?
                                             ;; Same set of submodules:
                                             (namespace-submodule-declarations ns)
                                             ;; Fresh set of submodules:
                                             (make-small-hasheq))]
                 [available-module-instances (make-hasheqv)]
                 [available-cross-phase-module-instances (box null)]
                 [module-instances (make-hasheqv)]
                 [declaration-inspector (current-code-inspector)]))
  (small-hash-set! (namespace-phase-to-namespace m-ns) phase m-ns)
  (define at-phase (make-hasheq))
  (hash-set! (namespace-module-instances m-ns) phase at-phase)
  (hash-set! at-phase name (make-module-instance m-ns #f portal-syntaxes))
  m-ns)

;; ----------------------------------------

(define (declare-module! ns m mod-name #:with-submodules? [with-submodules? #t])
  (define prior-m (and with-submodules?
                       (hash-ref (module-registry-declarations (namespace-module-registry ns))
                                 mod-name
                                 #f)))
  (define prior-mi (and prior-m
                        (not (eq? m prior-m))
                        (namespace->module-instance ns mod-name (namespace-phase ns))))
  (when (and prior-m (not (eq? m prior-m)))
    (check-redeclaration-ok prior-m prior-mi mod-name))
  (if with-submodules?
      (hash-set! (module-registry-declarations (namespace-module-registry ns)) mod-name m)
      (small-hash-set! (namespace-submodule-declarations ns) mod-name m))
  (when with-submodules?
    ;; Register this module's exports for use in resolving bulk
    ;; bindings, so that bulk bindings can be shared among other
    ;; modules when unmarshaling; we don't do this without
    ;; `with-submodules?` to avoid leaking submodules being
    ;; expanded, but see also `bind-all-provides!`
    (register-bulk-provide! (namespace-bulk-binding-registry ns)
                            mod-name
                            (module-self m)
                            (module-provides m))
    ;; Tell resolver that the module is declared
    ((current-module-name-resolver) mod-name #f))
  ;; If this module is already instantiated, re-instantiate it
  (when prior-mi
    (define m-ns (let ([m-ns (module-instance-namespace prior-mi)])
                   (if (module-instance-shared prior-mi)
                       ;; Don't re-instantiate in place, since definitions
                       ;; may be shared with other namespaces
                       (fresh-module-instance-namespace m-ns)
                       m-ns)))
    (define states (module-instance-phase-level-to-state prior-mi))
    (define phase (namespace-phase ns))
    (define visit? (eq? 'started (small-hash-ref states (add1 phase) #f)))
    (define run? (eq? 'started (small-hash-ref states phase #f)))

    (define at-phase (hash-ref (namespace-module-instances ns) phase))
    (hash-set! at-phase mod-name (make-module-instance m-ns m #f))

    (set-module-instance-shifted-requires! prior-mi #f)
    (namespace-clear-shifted-requires! ns mod-name)

    (when visit?
      (namespace-module-visit! ns (namespace-mpi m-ns) phase))
    (when run?
      (namespace-module-instantiate! ns (namespace-mpi m-ns) phase))))

(define (check-redeclaration-ok prior-m prior-mi mod-name)
  (when (module-cross-phase-persistent? prior-m)
    (raise-arguments-error 'module
                           "cannot redeclare cross-phase persistent module"
                           "module name" (module-name->error-string mod-name)))
  (when (and prior-mi
             (or (module-instance-attached? prior-mi)
                 (not (inspector-superior? (current-code-inspector)
                                           (namespace-inspector (module-instance-namespace prior-mi))))))
    (raise-arguments-error 'module
                           "current code inspector cannot redeclare module"
                           "module name" (module-name->error-string mod-name))))

(define (raise-unknown-module-error who mod-name)
  (raise-arguments-error who
                         "unknown module" 
                         "module name" (module-name->error-string mod-name)))

(define (namespace->module-linklet-info ns name phase-level)
  (define m (namespace->module ns name))
  (and m
       ((module-phase-level-linklet-info-callback m) phase-level ns (module-inspector m))))

(define (module-name->error-string mod-name)
  (unquoted-printing-string (format "~a" mod-name)))

;; ----------------------------------------

(define (namespace->module-instance ns name 0-phase
                                    #:complain-on-failure? [complain-on-failure? #f]
                                    #:check-available-at-phase-level [check-available-at-phase-level #f]
                                    #:unavailable-callback [unavailable-callback void])
  (define mi
    (or (hash-ref (hash-ref (namespace-module-instances ns) 0-phase #hasheq())
                  name
                  #f)
        (let ([c-ns (or (namespace-root-namespace ns) ns)])
          (hash-ref (namespace-module-instances c-ns) name #f))
        (and complain-on-failure?
             (error 'require
                    (string-append
                     "namespace mismatch;\n"
                     " reference to a module that is not instantiated\n"
                     "  module: ~a\n"
                     "  phase: ~a")
                    name
                    0-phase))))
  (if (and mi check-available-at-phase-level)
      (check-availablilty mi check-available-at-phase-level unavailable-callback)
      mi))

(define (namespace-install-module-namespace! ns name 0-phase m existing-m-ns run-phase)
  (define m-ns (struct-copy namespace ns
                            [mpi (namespace-mpi existing-m-ns)]
                            [source-name (namespace-source-name existing-m-ns)]
                            [root-expand-ctx (box (unbox (namespace-root-expand-ctx existing-m-ns)))]
                            [phase (namespace-phase existing-m-ns)]
                            [0-phase (namespace-0-phase existing-m-ns)]
                            [phase-to-namespace (make-small-hasheqv)]
                            [phase-level-to-definitions (if (module-cross-phase-persistent? m)
                                                            (namespace-phase-level-to-definitions existing-m-ns)
                                                            (make-small-hasheqv))]
                            [declaration-inspector (module-inspector m)]
                            [inspector (namespace-inspector existing-m-ns)]))
  (define mi (make-module-instance m-ns m #f))
  (set-module-instance-attached?! mi #t)
  (cond
   [(module-cross-phase-persistent? m)
    (small-hash-set! (namespace-phase-to-namespace m-ns) 0 m-ns)
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     0
                     (namespace->definitions existing-m-ns 0))
    (small-hash-set! (namespace-phase-to-namespace m-ns) 1 (namespace->namespace-at-phase m-ns 1))
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     1
                     (namespace->definitions existing-m-ns 1))
    (hash-set! (namespace-module-instances (or (namespace-root-namespace ns) ns))
               name
               mi)
    (small-hash-set! (module-instance-phase-level-to-state mi) 0 'started)]
   [else
    (define run-phase-level (phase- run-phase 0-phase))
    (small-hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
    (small-hash-set! (namespace-phase-level-to-definitions m-ns)
                     run-phase-level
                     (namespace->definitions existing-m-ns run-phase-level))
    (small-hash-set! (module-instance-phase-level-to-state mi) run-phase-level 'started)
    (define at-phase (or (hash-ref (namespace-module-instances ns) 0-phase #f)
                         (let ([at-phase (make-hasheq)])
                           (hash-set! (namespace-module-instances ns) 0-phase at-phase)
                           at-phase)))
    (hash-set! at-phase name mi)]))

(define (namespace-create-module-instance! ns name 0-phase m mpi)
  (define shared (find-or-create-shared-instance ns name 0-phase m mpi))
  (define m-ns (struct-copy namespace ns
                            [mpi (if shared
                                     (shared-instance-mpi shared)
                                     mpi)]
                            [source-name (or (module-source-name m)
                                             (resolved-module-path-root-name
                                              (module-path-index-resolve mpi)))]
                            [root-expand-ctx (box #f)] ; maybe set to non-#f by running
                            [phase 0-phase]
                            [0-phase 0-phase]
                            [phase-to-namespace (make-small-hasheqv)]
                            [phase-level-to-definitions (make-small-hasheqv)]
                            [declaration-inspector (module-inspector m)]
                            [inspector (make-inspector (module-inspector m))]))
  (small-hash-set! (namespace-phase-to-namespace m-ns) 0-phase m-ns)
  (when shared
    ;; Since definitions for these phase levels might be shared, make
    ;; sure they don't retain this instance's namespace context
    (for ([phase-level (in-list (shared-instance-phase-levels shared))])
      (namespace->definitions m-ns phase-level #:detached-data? #t)))
  (define mi (make-module-instance m-ns m #f shared))
  (if (module-cross-phase-persistent? m)
      (hash-set! (namespace-module-instances ns) name mi)
      (let ([at-phase (or (hash-ref (namespace-module-instances ns) 0-phase #f)
                          (let ([at-phase (make-hasheq)])
                            (hash-set! (namespace-module-instances ns) 0-phase at-phase)
                            at-phase))])
        (hash-set! at-phase name mi)))
  mi)

;; Copies a module instance's namespace, but with fresh definitions
(define (fresh-module-instance-namespace m-ns)
  (define new-m-ns (struct-copy namespace m-ns
                                [root-expand-ctx (box #f)]
                                [phase-to-namespace (make-small-hasheqv)]
                                [phase-level-to-definitions (make-small-hasheqv)]))
  (small-hash-set! (namespace-phase-to-namespace new-m-ns) (namespace-0-phase m-ns) new-m-ns)
  new-m-ns)

;; Finds or creates a `shared-instance` record for a module instance,
;; or returns #f if the instance cannot be shared
(define (find-or-create-shared-instance ns name 0-phase m mpi)
  (define cache (module-instance-cache m))
  (and cache
       (not (label-phase? 0-phase))
       (non-self-derived-module-path-index? mpi)
       (let* ([r (namespace-module-registry ns)]
              [by-phase (or (hash-ref cache r #f)
                            (let ([ht (make-hasheqv)])
                              (hash-set! cache r ht)
                              ht))]
              [shareds (hash-ref by-phase 0-phase null)]
              [mpi-shape (module-path-index-shape mpi)])
         (or (for/or ([shared (in-list shareds)])
               (and (eq? name (shared-instance-name shared))
                    (equal? mpi-shape (shared-instance-mpi-shape shared))
                    (begin
                      (set-shared-instance-last-use! shared (registry-tick! r))
                      shared)))
             (let ([shared (shared-instance name
                                            mpi-shape
                                            mpi
                                            (box #f)
                                            (module-shareable-phase-levels m)
                                            (make-hasheqv)
                                            by-phase
                                            0-phase
                                            (registry-tick! r))])
               (hash-set! by-phase 0-phase (cons shared shareds))
               (register-shared-instance! r shared)
               (when (log-sharing-enabled?)
                 (log-sharing (vector name 0-phase mpi-shape 'new)
                              "new shared instance of ~s with phase shift ~a"
                              name 0-phase))
               shared)))))

;; Assumes `non-self-derived-module-path-index?`
(define (module-path-index-shape mpi)
  (let loop ([mpi mpi])
    (define-values (path base) (module-path-index-split mpi))
    (cons path (if base (loop base) null))))

;; Called by a module's `instantiate-phase` callback, so that existing
;; definitions can be used if they were created with the same imports.
;; Returns #t if shared definitions were installed, in which case the
;; caller should not run the phase-level body.
(define (shared-instance-install! shared ns phase-level import-instances)
  (define d+imports (hash-ref (shared-instance-levels shared) phase-level #f))
  (cond
    [(not (memv phase-level (shared-instance-phase-levels shared))) #f]
    [(and d+imports
          (let loop ([a (cdr d+imports)] [b import-instances])
            (cond
              [(null? a) (null? b)]
              [(null? b) #f]
              [else (and (eq? (car a) (car b))
                         (loop (cdr a) (cdr b)))])))
     (small-hash-set! (namespace-phase-level-to-definitions ns) phase-level (car d+imports))
     #t]
    [else #f]))

;; Called by a module's `instantiate-phase` callback when a phase level
;; is about to be run instead of installed via `shared-instance-install!`
(define (log-shared-instance-miss shared ns phase-level import-instances)
  (when (and shared (log-sharing-enabled?))
    (define name (resolved-module-path-name (module-path-index-resolve (namespace-mpi ns))))
    (define phase (phase+ phase-level (namespace-0-phase ns)))
    (cond
      [(not (memv phase-level (shared-instance-phase-levels shared)))
       (log-sharing (vector name phase-level phase 'unshareable)
                    "phase level ~a of ~s is not shareable, so instantiating at phase ~a"
                    phase-level name phase)]
      [(hash-ref (shared-instance-levels shared) phase-level #f)
       => (lambda (d+imports)
            ;; Find the first import that is different
            (define diff-name
              (let loop ([a (cdr d+imports)] [b import-instances])
                (cond
                  [(or (null? a) (null? b)) #f]
                  [(eq? (car a) (car b)) (loop (cdr a) (cdr b))]
                  [else (instance-name (car b))])))
            (log-sharing (vector name phase-level phase 'imports-differ diff-name)
                         "phase level ~a of ~s at phase ~a not shared, because an import differs: ~a"
                         phase-level name phase diff-name))]
      [else
       ;; First instantiation, which will be recorded for sharing
       (void)])))

;; Called by a module's `instantiate-phase` callback after running a
;; phase-level body, so that the resulting definitions can be reused
(define (shared-instance-record! shared ns phase-level import-module-instances import-instances)
  (define levels (shared-instance-levels shared))
  (define r (namespace-module-registry ns))
  (define long-lived (module-registry-shared-linklet-instances r))
  (unless (or (hash-ref levels phase-level #f)
              (not (memv phase-level (shared-instance-phase-levels shared)))
              ;; If an import is specific to this instantiation, then a
              ;; later instantiation cannot match it, and recording
              ;; would retain this instantiation's context:
              (not (for/and ([mi (in-list import-module-instances)]
                             [inst (in-list import-instances)])
                     (or (module-cross-phase-persistent? (module-instance-module mi))
                         (hash-ref long-lived inst #f)))))
    (define d (namespace->definitions ns phase-level))
    (hash-set! long-lived (definitions-variables d) #t)
    (hash-set! levels phase-level (cons d import-instances))))

(define (check-availablilty mi check-available-at-phase-level unavailable-callback)
  (define m (module-instance-module mi))
  (if (and m
           (<= (module-min-phase-level m) (add1 check-available-at-phase-level) (module-max-phase-level m))
           (not (small-hash-ref (module-instance-phase-level-to-state mi) (add1 check-available-at-phase-level) #f)))
      (unavailable-callback mi)
      mi))

(define (namespace->module-namespace ns name 0-phase
                                     #:complain-on-failure? [complain-on-failure? #f]
                                     #:check-available-at-phase-level [check-available-at-phase-level #f]
                                     #:unavailable-callback [unavailable-callback void])
  (define mi (namespace->module-instance ns name 0-phase
                                         #:complain-on-failure? complain-on-failure?
                                         #:check-available-at-phase-level check-available-at-phase-level
                                         #:unavailable-callback unavailable-callback))
  (and mi (module-instance-namespace mi)))

(define (namespace-record-module-instance-attached! ns mod-name phase)
  (define mi (namespace->module-instance ns mod-name phase))
  (set-module-instance-attached?! mi #t))

;; Before attaching amodule declaration to a new namespace, make sure
;; that its syntax deserialization is associated with the original
;; bulk-binding regsitry
(define (module-force-bulk-binding! m ns)
  ((module-force-bulk-binding m) (namespace-bulk-binding-registry ns)))

(define (module-instantiates-at-level? m phase-level)
  (and (not (label-phase? phase-level))
       (<= (module-min-phase-level m) phase-level (module-max-phase-level m))))

;; ----------------------------------------

;; Create a module instance as needed, and then run the specified phase;
;; see also `run-module-instance!`, below
(define (namespace-module-instantiate! ns mpi instance-phase #:run-phase [run-phase (namespace-phase ns)]
                                       #:skip-run? [skip-run? #f]
                                       #:otherwise-available? [otherwise-available? #t]
                                       #:seen [seen #hasheq()]
                                       #:seen-list [seen-list null]
                                       #:recur? [recur? #t]
                                       #:minimum-inspector [minimum-inspector #f]
                                       #:transitive-record [transitive-modules #f])
  (unless (module-path-index? mpi)
    (error "not a module path index:" mpi))
  (define name (module-path-index-resolve mpi #t))
  (when (and transitive-modules (pair? seen-list))
    (hash-set! transitive-modules name (hash-set (hash-ref transitive-modules name #hasheqv()) instance-phase #t)))
  (define m (namespace->module ns name))
  (unless m (raise-unknown-module-error 'instantiate name))
  (define (instantiate! instance-phase run-phase ns)
    ;; Get or create a namespace for the module+phase combination:
    (define mi (or (namespace->module-instance ns name instance-phase)
                   (namespace-create-module-instance! ns name instance-phase m mpi)))
    (run-module-instance! mi ns #:run-phase run-phase
                          #:skip-run? skip-run?
                          #:otherwise-available? otherwise-available?
                          #:seen seen
                          #:seen-list seen-list
                          #:recur? recur?
                          #:minimum-inspector minimum-inspector
                          #:transitive-record transitive-modules))
  ;; If the module is cross-phase persistent, make sure it's instantiated
  ;; at phase 0 and registered in `ns` as phaseless
  (cond
   [(module-cross-phase-persistent? m)
    (instantiate! 0 0 (or (namespace-root-namespace ns) ns))]
   [else
    (instantiate! instance-phase run-phase ns)]))

(define (namespace-module-visit! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)]
                                 #:transitive-record [transitive-modules #f])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase)
                                 #:transitive-record transitive-modules))

(define (namespace-module-make-available! ns mpi instance-phase #:visit-phase [visit-phase (namespace-phase ns)]
                                          #:transitive-record [transitive-modules #f])
  (namespace-module-instantiate! ns mpi instance-phase #:run-phase (add1 visit-phase) #:skip-run? #t
                                 #:transitive-record transitive-modules))

(define (namespace-module-get-portal-syntax-lookup ns mpi phase-shift)
  (define name (module-path-index-resolve mpi #t))
  ;; Get or create a namespace for the module+phase combination:
  (define ready-mi (namespace->module-instance ns name phase-shift))
  (define (try-namespace-lookup ns phase sym)
    (define v (namespace-get-transformer ns phase sym (lambda () #f)))
    (and (portal-syntax? v)
         (portal-syntax-content v)))
  (cond
    [(and ready-mi (module-instance-portal-syntaxes ready-mi))
     => (lambda (portal-syntaxes)
          (lambda (phase sym)
            (hash-ref (hash-ref portal-syntaxes phase #hasheqv()) sym #f)))]
    [(top-level-module-path-index? mpi)
     (lambda (phase sym)
       (try-namespace-lookup ns (phase+ phase phase-shift) sym))]
    [(non-self-module-path-index? mpi)
     (define mi (or ready-mi
                    (let ([m (namespace->module ns name)])
                      (unless m (raise-unknown-module-error 'identifier-binding-portal-syntax name))
                      (namespace-create-module-instance! ns name phase-shift m mpi))))
     (define m-ns (module-instance-namespace mi))
     (define bulk-binding-registry (namespace-bulk-binding-registry m-ns))
     (define m (module-instance-module mi))
     (define insp (module-inspector m))
     (define data-box (module-instance-data-box mi))
     (define prep (module-prepare-instance m))
     (prep data-box m-ns phase-shift mpi bulk-binding-registry insp)
     (define get (module-get-portal-syntax-callback m))
     (lambda (phase sym)
       (or (get data-box phase sym)
           ;; in case a top-level definition happened in the module's namespace:
           (try-namespace-lookup m-ns phase sym)))]
    [else
     ;; self MPI can only refer to a for-label portal binding
     (lambda (phase sym)
       (try-namespace-lookup ns phase sym))]))

;; The `instance-phase` corresponds to the phase shift for the module
;; instances. The module may have content at different phase levels,
;; which are all consistently shifted. The `run-phase` is an absolute
;; phase that should be immediately run, unless `skip-run?` is true;
;; to put it another way, phase level `(phase- instance-phase
;; run-phase)` within the instance should be run immediately.
;; Normally, the instance is made available at all other non-negative
;; phases, but `#:otherwise-available?` controls that behavior.
(define (run-module-instance! mi ns #:run-phase run-phase
                              #:skip-run? skip-run? 
                              #:otherwise-available? otherwise-available?
                              #:recur? [recur? #t]
                              #:seen [seen #hasheq()]
                              #:seen-list [seen-list null]
                              #:minimum-inspector [minimum-inspector #f]
                              #:transitive-record [transitive-modules #f])
  (performance-region
   ['eval 'requires]
   ;; Nothing to do if we've run this phase already and made the
   ;; instance sufficiently available:
   (define m-ns (module-instance-namespace mi))
   (define instance-phase (namespace-0-phase m-ns))
   (define run-phase-level (phase- run-phase instance-phase))
   (define m (module-instance-module mi))
   (define inspector (and m (module-inspector m)))
   (when (and minimum-inspector inspector)
     (unless (or (eq? inspector minimum-inspector)
                 (inspector-superior? inspector minimum-inspector))
       (error 'require
              "cannot import module with weaker code inspector\n  module: ~a"
              (module-path-index-resolve
               (namespace-mpi (module-instance-namespace mi))))))
   (unless (and (or skip-run?
                    (eq? 'started (small-hash-ref (module-instance-phase-level-to-state mi) run-phase-level #f)))
                (or (not otherwise-available?)
                    (module-instance-made-available? mi))
                (or (not transitive-modules)
                    ;; need to traverse cross-phase to get all transitive modules
                    ;; registered, even though we won't have to do more than that
                    (not (module-cross-phase-persistent? (module-instance-module mi)))))
     ;; Something to do...
     (define m (module-instance-module mi))
     (unless m
       (error 'require "import cycle detected; trying to run module being expanded"))
     (define mpi (namespace-mpi m-ns))
     (define phase-shift instance-phase) ; instance phase = phase shift
     (define bulk-binding-registry (namespace-bulk-binding-registry m-ns))
     
     (when recur?

       (when (hash-ref seen mi #f)
         (error 'require
                (apply string-append
                       "import cycle detected during module instantiation\n"
                       "  dependency chain:"
                       (module-instances->indented-module-names mi seen-list))))

       (define record-shifted-requires-at-name
         (cond
           [(module-instance-shifted-requires mi) #f]
           [else
            ;; We haven't shifted required mpis already
            ;; the list of required mpis is pruned to the set that we
            ;; need to explicitly instaniate, where others are presumed
            ;; to be instantiated transitively and we should skip trying
            ;; again for this module's direct require
            (define name (module-path-index-resolve mpi))
            (define shifted-requires (build-module-instance-shifted-requires ns m mpi name))
            (set-module-instance-shifted-requires! mi shifted-requires)
            (and (non-self-derived-module-path-index? mpi)
                 name)]))

       ;; Recur for required modules:
       (define new-seen (hash-set seen mi #t))
       (define (recur-instantiate! req-mpi req-phase req-recur?)
         (namespace-module-instantiate! ns req-mpi (phase+ instance-phase req-phase)
                                        #:run-phase run-phase
                                        #:skip-run? skip-run?
                                        #:otherwise-available? otherwise-available?
                                        #:seen new-seen
                                        #:seen-list (cons mi seen-list)
                                        #:recur? req-recur?
                                        #:minimum-inspector inspector
                                        #:transitive-record transitive-modules))
       (if (not (module-flattened-requires m))
           ;; traverse per-phase list of mpis
           (for ([phase+mpis (in-list (module-instance-shifted-requires mi))]
                 [recurs (in-list (module-recur-requires m))])
             (define req-phase (car phase+mpis))
             (for ([req-mpi (in-list (cdr phase+mpis))]
                   [recur (in-list recurs)]
                   #:when req-mpi)
               (recur-instantiate! req-mpi req-phase recur)))
           ;; traverse per-mpi list of phases
           (for ([mpi+phases (in-list (module-instance-shifted-requires mi))])
             (define req-mpi (vector-ref mpi+phases 0))
             (for ([req-phase (in-list (vector-ref mpi+phases 1))])
               (recur-instantiate! req-mpi req-phase #f))))

       (when record-shifted-requires-at-name
         ;; Recurring forced resolutions, so we can now save resolved paths for future use
         (namespace-save-shifted-requires! ns m
                                           record-shifted-requires-at-name
                                           (module-instance-shifted-requires mi))))

     ;; Run or make available phases of the module body:
     (unless (label-phase? instance-phase)
       (for ([phase-level (in-range (module-max-phase-level m) (sub1 (module-min-phase-level m)) -1)])
         (define phase (phase+ phase-level phase-shift))
         (cond
          [(and (not skip-run?)
                (eqv? phase run-phase))
           ;; This is the phase to make sure that we've run
           (unless (eq? 'started (small-hash-ref (module-instance-phase-level-to-state mi) phase-level #f))
             (small-hash-set! (module-instance-phase-level-to-state mi) phase-level 'started)
             (void (namespace->definitions m-ns phase-level))
             (define p-ns (namespace->namespace-at-phase m-ns phase))
             (define insp (module-inspector m))
             (define data-box (module-instance-data-box mi))
             (define prep (module-prepare-instance m))
             (define go (module-instantiate-phase m))
             (define shared (module-instance-shared mi))
             (prep data-box p-ns phase-shift mpi bulk-binding-registry insp)
             (if shared
                 (go data-box p-ns phase-shift phase-level mpi bulk-binding-registry insp shared)
                 (go data-box p-ns phase-shift phase-level mpi bulk-binding-registry insp)))]
          [(and otherwise-available?
                (not (negative? run-phase))
                (not (small-hash-ref (module-instance-phase-level-to-state mi) phase-level #f)))
           ;; This is a phase to merely make available
           (if (module-cross-phase-persistent? m)
               (let ([bx (namespace-available-cross-phase-module-instances ns)])
                 (set-box! bx (cons mi (unbox bx))))
               (hash-update! (namespace-available-module-instances ns)
                             phase
                             (lambda (l) (cons mi l))
                             null))
           (small-hash-set! (module-instance-phase-level-to-state mi) phase-level 'available)])))

     (when otherwise-available?
       (set-module-instance-made-available?! mi #t))

     (when (label-phase? instance-phase)
       ;; no phase is run for the label phase, so we still need to make
       ;; sure bulk bindings are registered
       (module-force-bulk-binding! m ns))

     (unless skip-run?
       ;; In case there's no such phase for this module instance, claim 'started
       ;; to short-circuit future attempts:
       (small-hash-set! (module-instance-phase-level-to-state mi) run-phase-level 'started)))))

(define (namespace-visit-available-modules! ns [run-phase (namespace-phase ns)])
  (namespace-run-available-modules! ns (add1 run-phase)))

(define (namespace-run-available-modules! ns [run-phase (namespace-phase ns)])
  (registry-call-with-lock
   (namespace-module-registry ns)
   (lambda ()
     (let loop ()
       (define cp-mis (unbox (namespace-available-cross-phase-module-instances ns)))
       (define mis (hash-ref (namespace-available-module-instances ns) run-phase null))
       (unless (and (null? cp-mis) (null? mis))
         (set-box! (namespace-available-cross-phase-module-instances ns) null)
         (hash-set! (namespace-available-module-instances ns) run-phase null)
         (for ([mi (in-list (reverse cp-mis))])
           (run-module-instance! mi ns #:run-phase 0 #:skip-run? #f #:otherwise-available? #f #:recur? #f))
         (for ([mi (in-list (reverse mis))])
           (run-module-instance! mi ns #:run-phase run-phase #:skip-run? #f #:otherwise-available? #f #:recur? #f))
         ;; In case instantiation added more reflectively:
         (loop))))))

(define (namespace-primitive-module-visit! ns name)
  (define mi (hash-ref (namespace-module-instances ns) (make-resolved-module-path name)))
  (run-module-instance! mi ns #:run-phase 1 #:skip-run? #f #:otherwise-available? #t))

(define (namespace-clear-shifted-requires! ns mod-name)
  (hash-remove! (namespace-module-instance-shifted-requires ns) mod-name))

(define (build-module-instance-shifted-requires ns m mpi name)
  (define requires (module-requires m))
  (define recur-requires (module-recur-requires m))
  (define flattened-requires (module-flattened-requires m))
  (define freshen-cache (make-hasheq))
  (if (not flattened-requires)
      (for/list ([phase+mpis (in-list requires)]
                 [recurs (in-list recur-requires)]
                 [resolved-paths (in-list (namespace-find-shifted-requires ns name requires))])
        (cons (car phase+mpis)
              (for/list ([req-mpi (in-list (cdr phase+mpis))]
                         [recur (in-list recurs)]
                         [resolved-path (in-list resolved-paths)])
                (and recur
                     (module-path-index-shift/resolved req-mpi
                                                       (module-self m)
                                                       mpi
                                                       freshen-cache
                                                       resolved-path)))))
      (for/list ([mpi+phases (in-list flattened-requires)]
                 [resolved-path (in-list (namespace-find-shifted-requires ns name flattened-requires
                                                                          #:flattened? #t))])
        (define req-mpi (vector-ref mpi+phases 0))
        (define new-req-mpi (module-path-index-shift/resolved req-mpi
                                                              (module-self m)
                                                              mpi
                                                              freshen-cache
                                                              resolved-path))
        (vector-immutable new-req-mpi
                          (vector-ref mpi+phases 1)))))

(define (namespace-find-shifted-requires ns name requires
                                         #:flattened? [flattened? #f])
  (define bx (hash-ref (namespace-module-instance-shifted-requires ns) name #f))
  (or (and bx (unbox bx))
      (if (not flattened?)
          (for/list ([phase+mpis (in-list requires)])
            (for/list ([mpi (in-list (cdr phase+mpis))])
              #f))
          (for/list ([mpi+phases (in-list requires)])
            #f))))

(define (namespace-shifted-requires-box ns name)
  ;; The indirection through a box allows resolutions to be shared alongside
  ;; attached module declarations
  (or (hash-ref (namespace-module-instance-shifted-requires ns) name #f)
      (let ([bx (box #f)])
        (hash-set! (namespace-module-instance-shifted-requires ns) name bx)
        bx)))

(define (namespace-save-shifted-requires! ns m name shifted-requires)
  (define bx (namespace-shifted-requires-box ns name))
  (unless (unbox bx)
    (define (resolve mpi)
      ;; keep only the resolved path, and keep only for filesystem paths;
      ;; note that module-expansion instances can get an mpi that maps to
      ;; an `'expanded` symbol, and we don't want to keep that  
      (define r (and mpi (module-path-index-resolve mpi)))
      (and r
           (let ([name (resolved-module-path-name r)])
             (and (or (path? name)
                      (and (pair? name) (path? (car name))))
                  r))))
    (set-box! bx
              (if (not (module-flattened-requires m))
                  (for/list ([phase+mpis (in-list shifted-requires)])
                    (for/list ([mpi (in-list (cdr phase+mpis))])
                      (resolve mpi)))
                  (for/list ([mpi+phases (in-list shifted-requires)])
                    (define mpi (vector-ref mpi+phases 0))
                    (resolve mpi))))))

(define (namespace-copy-shifted-requires! dest-namespace src-namespace name)
  (define bx (namespace-shifted-requires-box src-namespace name))
  (hash-set! (namespace-module-instance-shifted-requires dest-namespace) name bx))

;; ----------------------------------------

(define (namespace-module-use->module+linklet-instances ns mu 
                                                        #:shift-from [shift-from #f]
                                                        #:shift-to [shift-to #f]
                                                        #:phase-shift phase-shift
                                                        #:resolution-cache [module-use-resolution-cache #f])
  (define mod (module-use-module mu))
  (define use-name
    (or (and module-use-resolution-cache
             (hash-ref module-use-resolution-cache mod #f))
        (let ([name (module-path-index-resolve
                     (if shift-from
                         (module-path-index-shift mod shift-from shift-to)
                         mod))])
          (when module-use-resolution-cache
            (hash-set! module-use-resolution-cache mod name))
          name)))
  (define mi
    (namespace->module-instance ns 
                                use-name
                                phase-shift
                                #:complain-on-failure? #t))
  (define m-ns (module-instance-namespace mi))
  (define d (small-hash-ref (namespace-phase-level-to-definitions m-ns) (module-use-phase mu) #f))
  (if d
      (values mi (definitions-variables d))
      (error 'eval (string-append "namespace mismatch: phase level not found;\n"
                                  "  module: ~a\n"
                                  "  instance phase: ~a\n"
                                  "  phase level: ~a\n"
                                  "  found phase levels: ~a")
             mod
             (namespace-0-phase m-ns)
             (module-use-phase mu)
             (small-hash-keys (namespace-phase-level-to-definitions m-ns)))))

;; ----------------------------------------

(define (module-compute-access! m)
  (define access
    (for/fold ([access #hasheqv()]) ([(phase+space at-phase+space) (in-hash (module-provides m))])
      (define phase (phase+space-phase phase+space))
      (define phase-access
        (for/fold ([phase-access (hash-ref access phase #hasheq())])
                  ([(sym binding/p) (in-hash at-phase+space)])
          (hash-set phase-access
                    (module-binding-sym (provided-as-binding binding/p))
                    (if (provided-as-protected? binding/p)
                        'protected
                        'provided))))
      (hash-set access phase phase-access)))
  (set-module-access! m access)
  access)

;; ----------------------------------------

(define (module-instances->indented-module-names mi seen-list)
  (let ([mi->name (lambda (mi)
                    (format "\n   ~a" (module-path-index-resolve
                                       (namespace-mpi (module-instance-namespace mi)))))])
    (cons
     (mi->name mi)
     (let loop ([seen-list seen-list])
       (cond
         [(null? seen-list) '()]
         [(eq? mi (car seen-list))
          (list (mi->name mi))]
         [else
          (cons (mi->name (car seen-list)) (loop (cdr seen-list)))])))))
