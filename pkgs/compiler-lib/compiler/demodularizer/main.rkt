#lang racket/base
(require racket/set
         compiler/cm
         racket/file
         racket/path
         compiler/zo-structs
         "module.rkt"
         "pane.rkt"
         "runs.rkt"
         "name.rkt"
         "import-name.rkt"
         "merge.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt"
         "linklet.rkt"
         "one-mod.rkt"
         "path-submod.rkt"
         "log.rkt")

(provide demodularize

         garbage-collect-toplevels-enabled
         current-excluded-modules
         recompile-enabled
         current-work-directory
         syntax-object-preservation-enabled
         submodule-preservation-enabled
         current-maximum-phase
         current-merged-output-file)

(define current-excluded-modules (make-parameter (set)))
(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter 'auto))
(define current-work-directory (make-parameter #f))
(define syntax-object-preservation-enabled (make-parameter #f))
(define submodule-preservation-enabled (make-parameter #t))
(define current-maximum-phase (make-parameter 1))
(define current-merged-output-file (make-parameter #f))

(define (demodularize given-input-file [given-output-file #f]
                      #:submodule-specs [submodule-specs #hash()]
                      #:demod-submodules? [demod-submodules? #t]
                      #:exclude [given-explicitly-excluded-modules (current-excluded-modules)]
                      #:work-directory [given-work-directory (current-work-directory)]
                      #:keep-syntax? [keep-syntax? (syntax-object-preservation-enabled)]
                      #:max-phase [maximum-phase (current-maximum-phase)]
                      #:gc-toplevels? [gc-toplevels? (garbage-collect-toplevels-enabled)]
                      #:recompile [recompile-mode (recompile-enabled)]
                      #:return-bundle? [return-bundle? #f]
                      #:dump-output-file [dump-output-file (current-merged-output-file)]
                      #:keep-submodules? [keep-submodules? (submodule-preservation-enabled)]
                      #:external-singetons? [external-singletons? #t])
  (define input-path (normalize-path given-input-file))
  (define explicitly-excluded-modules
    (for/set ([path (in-set given-explicitly-excluded-modules)])
      (normalize-path path)))
  (define work-directory (or given-work-directory
                             (make-temporary-file "demod-work-~a" 'directory)))

  (log-demodularizer-info (format "Compiling modules to ~s" work-directory))
  (parameterize ([current-namespace (make-empty-namespace)]
                 [current-compiled-file-roots (list (build-path work-directory "native")
                                                    (build-path work-directory "linklet"))]
                 [current-compile-target-machine #f]
                 [current-multi-compile-any #t])
    (namespace-attach-module (variable-reference->namespace (#%variable-reference)) ''#%builtin)
    (managed-compile-zo input-path))

  (log-demodularizer-info "Finding modules")
  (define-values (all-one-mods submods common-excluded-module-mpis)
    (parameterize ([current-compiled-file-roots (if work-directory
                                                    (list (build-path work-directory "linklet"))
                                                    (current-compiled-file-roots))])
      (find-modules input-path
                    #:exclude-required? #f
                    #:exclude explicitly-excluded-modules
                    #:keep-syntax? keep-syntax?
                    #:all-phases? keep-syntax?)))

  (when (and work-directory (not given-work-directory))
    (delete-directory/files work-directory))

  (log-demodularizer-info "Partitioning modules")
  (define all-sorted-panes
    (partition-panes all-one-mods input-path submods
                     #:external-singetons? external-singletons?))
  (define-values (top-path/submods excluded-module-mpiss one-mods)
    (reify-panes all-sorted-panes all-one-mods common-excluded-module-mpis))

  (log-demodularizer-info "Finding module bodies to merge")
  (define-values (phase-runss excluded-modules-to-requires)
    (for/lists (phase-runss excluded-modules-to-requires)
        ([top-path/submod (in-list top-path/submods)]
         [excluded-module-mpis (in-list excluded-module-mpiss)])
      (find-runs top-path/submod
                 one-mods
                 excluded-module-mpis)))

  (log-demodularizer-info "Selecting names")
  (define-values (names internals)
    (select-names one-mods
                  phase-runss))
  (define new-phase-runss
    (for/list ([phase-runs (in-list phase-runss)]
               [excluded-module-mpis (in-list excluded-module-mpiss)])
      (add-import-maps phase-runs names
                       one-mods excluded-module-mpis
                       #:maximum-phase maximum-phase)))

  (log-demodularizer-info "Merging linklets")
  (define-values (phase-mergeds name-importss stx-vecs portal-stxess)
    (for/lists (phase-mergeds name-importss stx-vecs portal-stxess)
        ([phase-runs (in-list new-phase-runss)]
         [excluded-module-mpis (in-list excluded-module-mpiss)])
      (merge-linklets phase-runs names
                      excluded-module-mpis
                      #:maximum-phase maximum-phase)))

  (define new-phase-mergeds
    (cond
      [keep-syntax?
       ;; any definition might be referenced reflectively
       phase-mergeds]
      [else
       (log-demodularizer-info "GCing definitions")
       (for/list ([phase-merged (in-list phase-mergeds)]) 
         (gc-definitions phase-merged
                         #:keep-defines? keep-syntax?
                         #:assume-pure? gc-toplevels?))]))

  (log-demodularizer-info "Bundling linklet")
  (define dir-ht
    (for/hash ([top-path/submod (in-list top-path/submods)]
               [phase-merged (in-list new-phase-mergeds)]
               [name-imports (in-list name-importss)]
               [stx-vec (in-list stx-vecs)]
               [portal-stxes (in-list portal-stxess)]
               [excluded-modules-to-require (in-list excluded-modules-to-requires)]
               [excluded-module-mpis (in-list excluded-module-mpiss)])
      (define m (hash-ref one-mods top-path/submod))
      (define path (path/submod-path top-path/submod))
      (define submod (path/submod-submod top-path/submod))
      (define file-name
        (let-values ([(base name dir?) (split-path path)])
          (string->symbol (path->string (path-replace-extension name #"")))))
      (define module-name (if (pair? submod)
                              (cons file-name submod)
                              file-name))
      (define bundle
        (wrap-bundle module-name phase-merged name-imports
                     stx-vec portal-stxes
                     excluded-modules-to-require excluded-module-mpis (one-mod-provides m)
                     names
                     #:export? keep-syntax?
                     #:pre-submodules (one-mod-pre-submodules m)
                     #:post-submodules (one-mod-post-submodules m)
                     #:dump-output-file dump-output-file))
      (values submod bundle)))

  (define bundle
    (if (= 1 (hash-count dir-ht))
        (hash-ref dir-ht '())
        (linkl-directory dir-ht)))

  (cond
    [return-bundle?
     (log-demodularizer-info "Writing bytecode")
     (define o (open-output-bytes))
     (write-module o bundle)
     (parameterize ([read-accept-compiled #t])
       (read (open-input-bytes (get-output-bytes o))))]
    [else
     (log-demodularizer-info "Writing bytecode")
     (define output-file (or given-output-file
                             (path-add-suffix input-path #"_merged.zo")))
     (write-module output-file bundle)

     (when (or (eq? (recompile-enabled) #t)
               (eq? (recompile-enabled) 'auto))
       (log-demodularizer-info "Recompiling and rewriting bytecode")
       (define zo (compiled-expression-recompile
                   (parameterize ([read-accept-compiled #t])
                     (call-with-input-file* output-file read))))
       (call-with-output-file* output-file
                               #:exists 'replace
                               (lambda (out) (write zo out))))]))
