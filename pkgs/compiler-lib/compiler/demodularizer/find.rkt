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
         "syntax.rkt")

(provide find-modules
         current-excluded-modules)

(struct mod (compiled zo))                  ; includes submodules; `zo` is #f for excluded
(struct one-mod (compiled zo decl min-phase max-phase stx-vec stx-mpi)) ; module without submodules

(define current-excluded-modules (make-parameter (set)))

(define (find-modules orig-path
                      #:submodule [submod '()]
                      #:keep-syntax? [keep-syntax? #f]
                      #:all-phases? [all-phases? #f])
  (define mods (make-hash))           ; path -> mod 
  (define one-mods (make-hash))       ; path+submod -> one-mod
  (define phase-runs-done (make-hasheqv)) ; root-phase -> path+submod+phase -> #t
  (define phase-runs (make-hasheqv))      ; root-phase -> list of `run`
  (define excluded-module-mpis (make-hash)) ; path -> mpi
  (define excluded-modules (make-hash)) ; path/submod+phase-shift -> #t

  ;; deserialization of syntax objects is too tedious to re-implement, so
  ;; we access the implementation directly from `#%kernel`
  (define-values (real-deserialize-instance bulk-binding-registry register!
                                            syntax-shift-module-path-index)
    (kernel:syntax-deserialize))

  ;; returns (values min-phase mx-phase)
  (define (find-modules! orig-path+submod exclude?)
    (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
    (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
    (define path (normal-case-path (simplify-path (path->complete-path orig-path))))

    (unless (hash-ref mods path #f) 
      (define-values (zo-path kind) (get-module-path path))
      (unless (eq? kind 'zo)
        (error 'demodularize "not available in bytecode form\n  path: ~a" path))
      (define zo (and (not exclude?)
                      (call-with-input-file zo-path zo-parse)))
      (define compiled (parameterize ([read-accept-compiled #t]
                                      [current-load-relative-directory
                                       (let-values ([(dir file-name dir?) (split-path path)])
                                         dir)])
                         (call-with-input-file zo-path read)))
      (hash-set! mods path (mod compiled zo)))

    (unless (hash-ref one-mods (cons path submod) #f)
      (define m (hash-ref mods path))
      (define compiled (mod-compiled m))
      (define zo (mod-zo m))

      (define (raise-no-submod)
        (error 'demodularize "no such submodule\n  path: ~a\n  submod: ~a"
               path submod))
      (define one-compiled
        (let loop ([compiled compiled] [submod submod])
          (cond
            [(linklet-bundle? compiled)
             (unless (null? submod) (raise-no-submod))
             compiled]
            [else
             (cond
               [(null? submod)
                (or (hash-ref (linklet-directory->hash compiled) #f #f)
                    (raise-no-submod))]
               [else
                (loop (or (hash-ref (linklet-directory->hash compiled) (car submod) #f)
                          (raise-no-submod))
                      (cdr submod))])])))
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

      ;; Transitive requires
      
      (define reqs (instance-variable-value decl 'requires))

      (define-values (trans-min-phase trans-max-phase)
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
            (or exclude? (set-member? (current-excluded-modules) req-path) (symbol? req-path)))
          (define-values (req-min-phase req-max-phase)
            (if (symbol? req-path)
                (values 0 0)
                (find-modules! path/submod exclude-req?)))
          (values (min min-phase (+ req-phase req-min-phase))
                  (max max-phase (+ req-phase req-max-phase)))))          

      ;; Deserialize syntax objects last, because we may need requires to be registered
      ;; in `bulk-binding-registry`
      (define-values (stx-vec stx-mpi)
        (deserialize-syntax real-deserialize-instance stx-data-linklet data-instance
                            bulk-binding-registry
                            syntax-shift-module-path-index
                            path submod (instance-variable-value decl 'self-mpi)))

      (hash-set! one-mods (cons path submod) (one-mod one-compiled one-zo decl trans-min-phase trans-max-phase stx-vec stx-mpi)))

    (if all-phases?
        (let ([m (hash-ref one-mods (cons path submod) #f)])
          (values (one-mod-min-phase m)
                  (one-mod-max-phase m)))
        (values 0 0)))

  (define (find-phase-runs! orig-path+submod orig-mpi
                            #:phase-level [phase-level 0]
                            #:root-phase [root-phase 0])
    (define orig-path (if (pair? orig-path+submod) (car orig-path+submod) orig-path+submod))
    (define submod (if (pair? orig-path+submod) (cdr orig-path+submod) '()))
    (define path (normal-case-path (simplify-path (path->complete-path orig-path))))
    (define path/submod (if (pair? submod) (cons path submod) path))

    (unless (hash-ref (hash-ref phase-runs-done root-phase #hash()) (cons (cons path submod) phase-level) #f)
      (define one-m (hash-ref one-mods (cons path submod) #f))
      (when (one-mod-zo one-m) ; not excluded
        (define decl (one-mod-decl one-m))
        (define stx-vec (one-mod-stx-vec one-m))
        (define stx-mpi (one-mod-stx-mpi one-m))

        (define linkl-table (linkl-bundle-table (one-mod-zo one-m)))
        (define linkl (hash-ref linkl-table phase-level #f))
        (define meta-linkl (hash-ref linkl-table (add1 phase-level) #f))
        (define uses
          (list*
           ;; The first implicit import might get used for syntax literals;
           ;; recognize it with a 'syntax-literals "phase"
           (cons path/submod 'syntax-literals)
           ;; The second implicit import might get used to register a macro;
           ;; we'll map those registrations to the same implicit import:
           '(#%transformer-register . transformer-register)
           (for/list ([u (hash-ref (instance-variable-value decl 'phase-to-link-modules)
                                   phase-level
                                   null)])
             (define path/submod (module-path-index->path (module-use-module u) path submod))

             ;; In case the import turns out to stay imported:
             (define req-path (if (pair? path/submod) (car path/submod) path/submod))
             (hash-set! excluded-module-mpis req-path (module-path-index-reroot (module-use-module u) orig-mpi))

             (cons path/submod (module-use-phase u)))))

        (define shifted-stx-vec
          (if (eqv? phase-level 0)
              stx-vec
              (and stx-vec
                   (for/vector ([e (in-vector stx-vec)]) (syntax-shift-phase-level e (- phase-level))))))

        (define r (run (if (null? submod) path (cons path submod)) phase-level linkl meta-linkl uses
                       shifted-stx-vec stx-mpi))
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
            [(or (symbol? req-path)
                 (set-member? (current-excluded-modules) req-path))
             ;; Root of an excluded subtree; keep it as a `require`, even if there
             ;; turn out to be no imported variables at the linklet level
             (hash-set! excluded-modules (cons path/submod (- at-phase-level root-phase)) #t)]
            [else
             (find-phase-runs! path/submod full-mpi
                               #:phase-level at-phase-level
                               #:root-phase root-phase)]))

        ;; Adding after requires, so that each list in `phase-runs` ends up in the
        ;; reverse order that we want to emit code
        (when linkl (hash-set! phase-runs root-phase (cons r (hash-ref phase-runs root-phase null)))))))

  (define-values (reachable-min-phase reachable-max-phase)
    (find-modules! (cons orig-path submod) #f))

  (for ([root-phase (in-range reachable-min-phase (add1 reachable-max-phase))])
    (find-phase-runs! (cons orig-path submod) (module-path-index-join #f #f)
                      #:phase-level root-phase
                      #:root-phase root-phase))

  (values (for/hasheqv ([(root-phase runs) (in-hash phase-runs)])
            (values root-phase (reverse runs)))
          (hash-keys excluded-modules)
          excluded-module-mpis))
