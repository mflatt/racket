#lang racket/base
(require racket/set
         compiler/zo-parse
         syntax/modcode
         racket/linklet
         (only-in '#%kernel [syntax-deserialize kernel:syntax-deserialize])
         "../private/deserialize.rkt"
         "linklet.rkt"
         "module-path.rkt"
         "run.rkt"
         "syntax.rkt"
         "binding.rkt")

(provide find-modules)

;; includes submodules
(struct mod (compiled zo))

;; module without submodules
(struct one-mod (excluded?
                 imports-enclosing?
                 compiled zo
                 decl
                 min-phase max-phase
                 provides
                 stx-vec stx-mpi portal-stxes))

(struct find-state (mods one-mods excluded-module-mpis included-modules bulk-binding-registry))

(define (find-modules orig-path/submod
                      #:exclude-required? exclude-required?
                      #:state state
                      #:state-rel-mod-path state-rel-mod-path
                      #:exclude explicitly-excluded-modules
                      #:keep-syntax? [keep-syntax? #f]
                      #:all-phases? [all-phases? #f])
  (define top-path/submod orig-path/submod)
  (define orig-path (if (pair? orig-path/submod) (car orig-path/submod) orig-path/submod))
  (define submod (if (pair? orig-path/submod) (cdr orig-path/submod) '()))

  (define can-duplicate-modules #hasheq())

  ;; If we're finding modules for a submodule, then either the
  ;; submodule imports the supermodule with no phase shift, and we
  ;; want to treat all modules folded into the supermodule as part of
  ;; that supermodule, or it doesn't, in which case we'll error if
  ;; there's an overlap. Either way, it's ok to merge the module and
  ;; exclusion info from the supermodule as recorded in `state`.

  (define mods      ; path -> mod 
    (if state
        (hash-copy (find-state-mods state))
        (make-hash)))
  (define one-mods  ; path+submod -> one-mod
    (if state
        (hash-copy (find-state-one-mods state))
        (make-hash)))
  (define excluded-module-mpis  ; path+submod -> mpi
    (if state
        (hash-copy (find-state-excluded-module-mpis state))
        (make-hash)))

  (define (excluded-via-supermodule? path/submod root-phase phase-level root-m)
    (and state
         (one-mod-imports-enclosing? root-m)
         (cond
           [(hash-ref (find-state-included-modules state) path/submod #f)
            => (lambda (phase-levels)
                 (hash-ref phase-levels (cons root-phase phase-level) #f))]
           [else #f])))
  (define (included-in-supermodule? path/submod)
    (and state
         (hash-ref (find-state-included-modules state) path/submod #f)
         #t))

  (define self-mpi (module-path-index-join #f #f))

  (when state
    (define rel-mpi (module-path-index-join state-rel-mod-path self-mpi))
    ;; shift MPIs for previously excluded modules
    (for ([(path mpi) (in-hash excluded-module-mpis)])
      (hash-set! excluded-module-mpis path (module-path-index-reroot mpi rel-mpi)))
    ;; every module already in `one-mods` will be excluded; map all of those modules
    ;; to `state-rel-mod-path`
    (for ([path+submod (in-hash-keys one-mods)])
      (define path (car path+submod))
      (define submod (cdr path+submod))
      (define path/submod (if (null? submod) path (cons path submod)))
      (unless (hash-ref excluded-module-mpis path/submod #f)
        (hash-set! excluded-module-mpis path/submod rel-mpi))))

  (define phase-runs-done (make-hasheqv)) ; root-phase -> path+submod+phase -> #t
  (define phase-runs (make-hasheqv))      ; root-phase -> list of `run`
  (define excluded-modules-to-require (make-hash)) ; path/submod+phase-shift -> #t

  ;; deserialization of syntax objects is too tedious to re-implement, so
  ;; we access the implementation directly from `#%kernel`
  (define-values (real-deserialize-instance new-bulk-binding-registry register!
                                            syntax-shift-module-path-index)
    (kernel:syntax-deserialize))
  (define bulk-binding-registry
    (if state
        (find-state-bulk-binding-registry state)
        new-bulk-binding-registry))

  (define (find-submod compiled submod raise-no-submod #:submod-list? submod-list?)
    (let loop ([compiled compiled] [submod submod])
      (cond
        [(linklet-bundle? compiled)
         (unless (null? submod) (raise-no-submod))
         (if submod-list?
             (values null null)
             compiled)]
        [else
         (cond
           [(null? submod)
            (define ht (linklet-directory->hash compiled))
            (define m-compiled (or (hash-ref ht #f #f)
                                   (raise-no-submod)))
            (if submod-list?
                (let ([ht (linklet-bundle->hash m-compiled)])
                  (values (hash-ref ht 'pre null)
                          (hash-ref ht 'post null)))
                m-compiled)]
           [else
            (loop (or (hash-ref (linklet-directory->hash compiled) (car submod) #f)
                      (raise-no-submod))
                  (cdr submod))])])))

  ;; returns (values min-phase mx-phase)
  (define (find-modules! orig-path+submod rel-mpi exclude? provides?)
    (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
    (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
    (define path (normal-case-path (simplify-path (path->complete-path orig-path))))

    (when exclude?
      (unless (hash-ref excluded-module-mpis orig-path+submod #f)
        (hash-set! excluded-module-mpis orig-path+submod rel-mpi)))

    (unless (hash-ref mods path #f) 
      (define-values (zo-path kind) (get-module-path path))
      (unless (eq? kind 'zo)
        (error 'demodularize "not available in bytecode form\n  path: ~a" path))
      (define zo (call-with-input-file zo-path zo-parse))
      (define compiled (parameterize ([read-accept-compiled #t]
                                      [current-load-relative-directory
                                       (let-values ([(dir file-name dir?) (split-path path)])
                                         dir)])
                         (call-with-input-file zo-path read)))
      (hash-set! mods path (mod compiled zo)))

    (define (find-transitive decl min-phase max-phase)
      (define reqs (instance-variable-value decl 'requires))

      (for/fold ([min-phase min-phase] [max-phase max-phase])
                ([phase+reqs (in-list reqs)]
                 #:do [(define req-phase (car phase+reqs))]
                 #:when req-phase
                 [req (in-list (cdr phase+reqs))])
        (define path/submod (module-path-index->path req path submod))
        (define req-path (if (pair? path/submod) (car path/submod) path/submod))
        (define exclude-req?
          ;; Even if this module is excluded, traverse it to get all
          ;; modules that it requires, so that we don't duplicate those
          ;; modules by accessing them directly                         
          (or exclude?
              exclude-required?
              (set-member? explicitly-excluded-modules req-path) (symbol? req-path)))
        (define-values (req-min-phase req-max-phase ignored-provides)
          (if (symbol? req-path)
              (values 0 0 #hasheqv())
              (find-modules! path/submod (module-path-index-reroot req rel-mpi) exclude-req? #f)))
        (values (min min-phase (+ req-phase req-min-phase))
                (max max-phase (+ req-phase req-max-phase)))))

    (define done-m (hash-ref one-mods (cons path submod) #f))

    ;; We might reach a module first as non-excluded and then later as
    ;; excluded, in which case we need to re-traverse dependencies as
    ;; also excluded
    (when (and exclude?
               done-m
               (not (one-mod-excluded? done-m)))
      (hash-set! one-mods (cons path submod) (struct-copy one-mod done-m
                                                          [excluded? #t]))
      (find-transitive (one-mod-decl done-m)
                       (one-mod-min-phase done-m)
                       (one-mod-max-phase done-m)))

    (unless done-m
      (define m (hash-ref mods path))
      (define compiled (mod-compiled m))
      (define zo (mod-zo m))

      (define (raise-no-submod)
        (error 'demodularize "no such submodule\n  path: ~a\n  submod: ~a"
               path submod))
      (define one-compiled
        (find-submod compiled submod raise-no-submod #:submod-list? #f))
      (define one-zo
        (cond
          [(not zo) #f]
          [(linkl-bundle? zo)
           (unless (null? submod) (raise-no-submod))
           zo]
          [else
           (or (hash-ref (linkl-directory-table zo) submod #f)
               (raise-no-submod))]))

      (define h (linklet-bundle->hash one-compiled))
      (define min-phase (hash-ref h 'min-phase 0))
      (define max-phase (hash-ref h 'max-phase 0))
      (define data-linklet (hash-ref h 'data #f))
      (define decl-linklet (hash-ref h 'decl #f))
      (define stx-data-linklet (and keep-syntax? 
                                    (hash-ref h 'stx-data #f)))
      (unless data-linklet
        (error 'demodularize "could not find module path metadata\n  path: ~a\n  submod: ~a"
               path submod))
      (unless decl-linklet
        (error 'demodularize "could not find module metadata\n  path: ~a\n  submod: ~a"
               path submod))

      (define data-instance (instantiate-linklet data-linklet
                                                 (list deserialize-instance)))
      (define decl (instantiate-linklet decl-linklet
                                        (list deserialize-instance
                                              data-instance)))

      (when keep-syntax?
        (register-provides-for-syntax register! bulk-binding-registry
                                      orig-path submod
                                      decl
                                      ;; use the real deserializer to get the internal form of provides
                                      (instantiate-linklet decl-linklet
                                                           (list real-deserialize-instance
                                                                 data-instance))))

      (define self-mpi (instance-variable-value decl 'self-mpi))

      ;; Transitive requires
      (define-values (trans-min-phase trans-max-phase)
        (find-transitive decl min-phase max-phase))

      ;; Deserialize syntax objects last, because we may need requires to be registered
      ;; in `bulk-binding-registry`
      (define-values (stx-vec stx-mpi)
        (deserialize-syntax real-deserialize-instance stx-data-linklet data-instance
                            bulk-binding-registry
                            syntax-shift-module-path-index
                            path submod self-mpi))

      (define orig-provides (or (and provides?
                                     (instance-variable-value decl 'provides))
                                #hasheqv()))
      (define provides
        (or (and orig-provides
                 ((hash-count orig-provides) . > . 0)
                 (let ([path-mpi (module-path-index-join
                                  (let ([m-path (if (path? orig-path) orig-path `(file ,orig-path))])
                                    (if (null? submod)
                                        m-path
                                        `(submod ,m-path ,@submod)))
                                  #f)])
                   (for/hasheqv ([(phase provs) (in-hash orig-provides)])
                     (values phase
                             (for/hasheq ([(name bind) (in-hash provs)])
                               (values name
                                       (binding-module-path-index-shift bind self-mpi path-mpi)))))))
            orig-provides))

      (define portal-stxes (instance-variable-value decl 'portal-stxes))

      (define imports-enclosing?
        (for/or ([phase+reqs (in-list (instance-variable-value decl 'requires))]                 
                 #:do [(define req-phase (car phase+reqs))]
                 #:when (eqv? req-phase 0)
                 [req (in-list (cdr phase+reqs))])
          (define-values (name base) (module-path-index-split req))
          (and (equal? name '(submod ".."))
               (eq? self-mpi base))))

      (hash-set! one-mods (cons path submod) (one-mod exclude?
                                                      imports-enclosing?
                                                      one-compiled one-zo decl
                                                      trans-min-phase trans-max-phase provides
                                                      stx-vec stx-mpi
                                                      portal-stxes)))

    (if all-phases?
        (let ([m (hash-ref one-mods (cons path submod) #f)])
          (values (one-mod-min-phase m)
                  (one-mod-max-phase m)
                  (one-mod-provides m)))
        (values 0 0 #hasheqv())))

  (define from-path orig-path/submod)
  
  (define (find-phase-runs! orig-path+submod orig-mpi
                            #:phase-level [phase-level 0]
                            #:root-phase [root-phase 0]
                            #:root-m [in-root-m #f])
    (define check-done (make-hash))
    (let find-loop ([orig-path+submod orig-path+submod]
                    [orig-mpi orig-mpi]
                    [phase-level phase-level]
                    [in-root-m in-root-m])
      (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
      (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
      (define path (normal-case-path (simplify-path (path->complete-path orig-path))))
      (define path/submod (if (pair? submod) (cons path submod) path))

      (unless (hash-ref (hash-ref phase-runs-done root-phase #hash()) (cons (cons path submod) phase-level) #f)
        (define one-m (hash-ref one-mods (cons path submod) #f))
        (define root-m (or in-root-m one-m))
        (cond
          [(excluded-via-supermodule? path/submod root-phase phase-level root-m)
           => (lambda (super-path/submod)
                (hash-set! excluded-modules-to-require (cons super-path/submod 0) #t))]
          [(one-mod-excluded? one-m)
           ;; Root of an excluded subtree; keep it as a `require`, even if there
           ;; turns out to be no imported variables at the linklet level. It's
           ;; possible that this subtree is covered by another one, and we clean
           ;; those up with a second pass
           (hash-set! excluded-modules-to-require (cons path/submod (- phase-level root-phase)) #t)
           (when state
             ;; A traversal like the one enclosing this case, just to
             ;; check for transitive requires that are not ok to duplicate
             (let loop ([path/submod path/submod]
                        [phase-level phase-level])
               (define key (cons path/submod phase-level))
               (unless (hash-ref check-done key #f)
                 (hash-set! check-done key #t)
                 (define path (if (pair? path/submod) (car path/submod) path/submod))
                 (define submod (if (pair? path/submod) (cdr path/submod) null))
                 (define one-m (hash-ref one-mods (cons path submod)))
                 (define decl (one-mod-decl one-m))                      
                 (define reqs (instance-variable-value decl 'requires))
                 (for* ([phase+reqs (in-list reqs)]
                        #:when (car phase+reqs)
                        [req (in-list (cdr phase+reqs))])
                   (define at-phase-level (- phase-level (car phase+reqs)))
                   (define path/submod (module-path-index->path req path submod))
                   (define req-path (if (pair? path/submod) (car path/submod) path/submod))
                   (cond
                     [(symbol? req-path) (void)]
                     [(and (included-in-supermodule? path/submod)
                           (not (hash-ref can-duplicate-modules path/submod #f)))
                      (error 'demodularize
                             (string-append "submodule's transitive exclusion references a wrong module;\n"
                                            " the referenced module is instantiated by an enclosing module,\n"
                                            " and the module is neither excluded nor duplicable\n"
                                            "  submodule: ~a\n"
                                            "  referenced module: ~a\n"
                                            "  phase level: ~a")
                             (cond
                               [(null? (cddr orig-path/submod)) (cadr orig-path/submod)]
                               [(cdr orig-path/submod)])
                             (if (pair? path/submod)
                                 (cons 'submod path/submod)
                                 path/submod)
                             (- phase-level))]
                     [else (loop path/submod at-phase-level)])))))]
          [else
           (define decl (one-mod-decl one-m))
           (define stx-vec (one-mod-stx-vec one-m))
           (define stx-mpi (one-mod-stx-mpi one-m))

           (define linkl-table (linkl-bundle-table (one-mod-zo one-m)))
           (define linkl (hash-ref linkl-table phase-level #f))
           (define meta-linkl (hash-ref linkl-table (add1 phase-level) #f))
           (define uses
             (list*
              ;; The first implicit import might get used for syntax literals;
              ;; recognize it with a `#%syntax-literals` module
              (cons '#%syntax-literals root-phase)
              ;; The second implicit import might get used to register a macro
              (cons '#%transformer-register root-phase)
              (for/list ([u (hash-ref (instance-variable-value decl 'phase-to-link-modules)
                                      phase-level
                                      null)])
                (define path/submod (module-path-index->path (module-use-module u) path submod))
                (cons path/submod (module-use-phase u)))))
           (define import-uses
             ;; rewrite imports of modules that are flattend into a supermodule
             (if state
                 (for/list ([use (in-list uses)])
                   (cond
                     [(excluded-via-supermodule? (car use) root-phase (cdr use) root-m)
                      => (lambda (super-path/submod)
                           (cons super-path/submod root-phase))]
                     [else use]))
                 uses))

           (define shifted-stx-vec
             (let ([phase-shift (- phase-level root-phase)])
               (if (eqv? phase-shift 0)
                   stx-vec
                   (and stx-vec
                        (for/vector ([e (in-vector stx-vec)]) (syntax-shift-phase-level e phase-shift))))))

           (define portal-stxes (hash-ref (one-mod-portal-stxes one-m) phase-level #hasheq()))

           (define r (run (if (null? submod) path (cons path submod)) phase-level linkl meta-linkl
                          uses import-uses
                          shifted-stx-vec stx-mpi
                          portal-stxes))
           (define runs-done (or (hash-ref phase-runs-done root-phase #f)
                                 (let ([ht (make-hash)])
                                   (hash-set! phase-runs-done root-phase ht)
                                   ht)))
           (hash-set! runs-done (cons (cons path submod) phase-level) #t)

           (define reqs (instance-variable-value decl 'requires))
           (for* ([phase+reqs (in-list reqs)]
                  #:when (car phase+reqs)
                  [req (in-list (cdr phase+reqs))])
             (define at-phase-level (- phase-level (car phase+reqs)))
             (define path/submod (module-path-index->path req path submod))
             (define full-mpi (module-path-index-reroot req orig-mpi))
             (define req-path (if (pair? path/submod) (car path/submod) path/submod))
             (cond
               [(symbol? req-path)
                ;; primitive modules are always excluded
                (hash-set! excluded-modules-to-require (cons path/submod (- at-phase-level root-phase)) #t)]
               [else
                (find-loop path/submod full-mpi at-phase-level root-m)]))

           ;; Adding after requires, so that each list in `phase-runs` ends up in the
           ;; reverse order that we want to emit code
           (when linkl (hash-set! phase-runs root-phase (cons r (hash-ref phase-runs root-phase null))))]))))

  (define (clear-redundant-excluded-to-require!)
    (define done (make-hash))
    (for ([path/submod+phase (in-list (hash-keys excluded-modules-to-require))]
          #:unless (symbol? (car path/submod+phase)))
      (let loop ([path/submod+phase path/submod+phase])
        (unless (hash-ref done path/submod+phase #f)
          (define path/submod (car path/submod+phase))
          (define path (if (pair? path/submod) (car path/submod) path/submod))
          (define submod (if (pair? path/submod) (cdr path/submod) null))
          (define phase (cdr path/submod+phase))
          (define one-m (hash-ref one-mods (cons path submod)))
          
          (define decl (one-mod-decl one-m))                 
          (define reqs (instance-variable-value decl 'requires))
          
          (for ([phase+reqs (in-list reqs)]
                #:when (car phase+reqs)
                [req (in-list (cdr phase+reqs))])
            (define at-phase-level (- phase (car phase+reqs)))
            (define path/submod (module-path-index->path req path submod))
            (define req-path (if (pair? path/submod) (car path/submod) path/submod))
            (define path/submod+phase (cons path/submod at-phase-level))
            (hash-remove! excluded-modules-to-require path/submod+phase)
            (unless (symbol? req-path)
              (loop path/submod+phase)))

          (hash-set! done path/submod+phase #t)))))

  (define-values (reachable-min-phase reachable-max-phase provides)
    (find-modules! (cons orig-path submod) self-mpi #f #t))

  (for ([root-phase (in-range reachable-min-phase (add1 reachable-max-phase))])
    (find-phase-runs! (cons orig-path submod) (module-path-index-join #f #f)
                      #:phase-level root-phase
                      #:root-phase root-phase))

  (clear-redundant-excluded-to-require!)

  (define-values (pre-submod-names post-submod-names)
    (find-submod (mod-compiled (hash-ref mods orig-path)) submod void #:submod-list? #t))

  ;; Gather info on included modules to communicate to submodules, which
  ;; may need to reference exports from other modules from the demodularized
  ;; encloding module (and should not duplicate modules that are inaccessible
  ;; via that route)
  (define included-modules ; path/submod -> (cons root-phase phase) -> super-path/submod
    (for*/fold ([ht (if state
                       (let ([included-modules (find-state-included-modules state)])
                         ;; If this submodule doesn't import its supermodule, then
                         ;; start with empty phase sets from supermodule
                         (if (not (one-mod-imports-enclosing?
                                   (hash-ref one-mods (cons orig-path submod))))
                             (for/hash ([(k v) (in-hash included-modules)])
                               (values k #hash()))
                             included-modules))
                       #hash())])
               ([(root-phase runs) (in-hash phase-runs)]
                [r (in-list runs)])
      (hash-set ht
                (run-path/submod r)
                (hash-set (hash-ref ht (run-path/submod r) #hash())
                          (cons root-phase (run-phase r))
                          orig-path/submod))))

  (define new-state (find-state mods one-mods excluded-module-mpis included-modules bulk-binding-registry))

  (log-debug "Merging for ~a:" orig-path/submod)
  (for* ([(phase runs) (in-hash phase-runs)]
         #:do [(log-debug "~a:" phase)]
         [r (in-list runs)])
    (log-debug "~a ~a" (run-path/submod r) (run-phase r)))

  (values (for/hasheqv ([(root-phase runs) (in-hash phase-runs)])
            (values root-phase (reverse runs)))
          (hash-keys excluded-modules-to-require)
          excluded-module-mpis
          provides
          pre-submod-names post-submod-names
          new-state))
