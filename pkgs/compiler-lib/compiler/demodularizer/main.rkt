#lang racket/base
(require racket/set
         compiler/cm
         racket/file
         compiler/zo-structs
         "find.rkt"
         "name.rkt"
         "merge.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt"
         "linklet.rkt")

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

(define logger (make-logger 'demodularizer (current-logger)))

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
                      #:keep-submodules? [keep-submodules? (submodule-preservation-enabled)])
  (define (normal-path p) (normal-case-path (simplify-path (path->complete-path p))))
  (define input-file (normal-path given-input-file))
  (define explicitly-excluded-modules
    (for/set ([path (in-set given-explicitly-excluded-modules)])
      (normal-path path)))
  (define work-directory (and (or (not recompile-mode)
                                  (not (eq? 'racket (system-type 'vm)))
                                  keep-syntax?)
                              (or given-work-directory
                                  (make-temporary-file "demod-work-~a" 'directory))))

  (define-values (bundle linkl-mode)
    (demodularize-tree input-file
                       #:submodule-specs submodule-specs
                       #:demod-submodules? demod-submodules?
                       #:exclude explicitly-excluded-modules
                       #:work-directory work-directory
                       #:keep-syntax? keep-syntax?
                       #:maximum-phase maximum-phase
                       #:gc-toplevels? gc-toplevels?
                       #:keep-submodules? keep-submodules?
                       #:dump-output-file dump-output-file))

  (when (and work-directory (not given-work-directory))
    (delete-directory/files work-directory))

  (cond
    [return-bundle?
     (log-info "Writing bytecode")
     (define o (open-output-bytes))
     (write-module o bundle)
     (parameterize ([read-accept-compiled #t])
       (read (open-input-bytes (get-output-bytes o))))]
    [else
     (log-info "Writing bytecode")
     (define output-file (or given-output-file
                             (path-add-suffix input-file #"_merged.zo")))
     (write-module output-file bundle)

     (when (or (eq? (recompile-enabled) #t)
               (and (eq? (recompile-enabled) 'auto)
                    (eq? linkl-mode 's-exp)))
       (log-info "Recompiling and rewriting bytecode")
       (define zo (compiled-expression-recompile
                   (parameterize ([read-accept-compiled #t])
                     (call-with-input-file* output-file read))))
       (call-with-output-file* output-file
                               #:exists 'replace
                               (lambda (out) (write zo out))))]))

(define (demodularize-tree input-file
                           #:submodule-specs submodule-specs
                           #:demod-submodules? demod-submodules?
                           #:exclude explicitly-excluded-modules
                           #:work-directory work-directory
                           #:keep-syntax? keep-syntax?
                           #:maximum-phase maximum-phase
                           #:gc-toplevels? gc-toplevels?
                           #:keep-submodules? keep-submodules?
                           #:dump-output-file dump-output-file)
  (define root-sym
    (let-values ([(base name dir?) (split-path input-file)])
      (string->symbol (path->string (path-replace-extension name #"")))))
  (let tree-loop ([submod '()]
                  [find-state-in #f]
                  [select-state-in #f]
                  [accum-uses #f]
                  [indent (lambda (s) s)])

    (define input-path/submod (if (null? submod)
                                  input-file
                                  (cons input-file submod)))
    
    (parameterize ([current-logger logger])

      (when (null? submod)
        (cond
          [work-directory
           (log-info (indent (format "Compiling modules to ~s" work-directory)))
           (parameterize ([current-namespace (make-empty-namespace)]
                          [current-compiled-file-roots (list (build-path work-directory "native")
                                                             (build-path work-directory "linklet"))]
                          [current-compile-target-machine #f]
                          [current-multi-compile-any #t])
             (namespace-attach-module (variable-reference->namespace (#%variable-reference)) ''#%builtin)
             (managed-compile-zo input-file))]
          [else
           (log-info (indent "Compiling module"))
           (parameterize ([current-namespace (make-base-empty-namespace)])
             (managed-compile-zo input-file))]))

      (log-info (indent (if (null? submod)
                            "Finding modules"
                            (format "Finding modules for submodule ~a" submod))))
      (define-values (phase-runs excluded-modules-to-require excluded-module-mpis provides
                                 pre-submods post-submods
                                 find-state)
        (parameterize ([current-compiled-file-roots (if work-directory
                                                        (list (build-path work-directory "linklet"))
                                                        (current-compiled-file-roots))])
          (find-modules input-path/submod
                        #:exclude-required? (and (pair? submod)
                                                 (not demod-submodules?)
                                                 (let ([v (hash-ref submodule-specs submod #f)])
                                                   (not (and v (hash-ref v 'demod #f)))))
                        #:state find-state-in
                        #:state-rel-mod-path '(submod "..")
                        #:exclude explicitly-excluded-modules
                        #:keep-syntax? keep-syntax?
                        #:all-phases? keep-syntax?)))

      (define submod-names
        (if keep-submodules?
            (append pre-submods post-submods)
            null))

      (log-info (indent "Selecting names"))
      (define-values (names phase-internals phase-lifts phase-name-imports phase-imports select-state)
        (select-names phase-runs
                      #:state select-state-in))

      (log-info (indent "Merging linklets"))
      (define-values (phase-body phase-first-internal-pos phase-merged-internals linkl-mode phase-import-keys
                                 portal-stxes phase-defined-names
                                 get-merge-info)
        (merge-linklets phase-runs names phase-internals phase-lifts phase-name-imports phase-imports
                        #:maximum-phase maximum-phase))

      ;; Handle submodules before GCing:
      (unless (null? submod-names)
        (log-info (indent "Building submodules")))
      (define sub-accum-uses ; this table is not per-phase, because names are unique across phase levels
        (and (pair? submod-names)
             (or accum-uses (not keep-syntax?))
             (make-hasheq)))
      (define directory-ht
        (for/fold ([ht #hash()])
                  ([sub (in-list submod-names)])
          (let ([submod (append submod (list sub))])
            (define-values (bundle new-linkl-mode)
              (tree-loop submod
                         find-state
                         select-state
                         sub-accum-uses
                         (lambda (s)
                           (string-append "  " (indent s)))))
            (cond
              [(linkl-directory? bundle)
               (for/fold ([ht ht]) ([(k l) (in-hash (linkl-directory-table bundle))])
                 (hash-set ht k l))]
              [else
               (hash-set ht submod bundle)]))))
            
      (define-values (phase-new-body phase-new-internals phase-new-lifts phase-new-defined-names)
        (cond
          [(and keep-syntax?
                (not accum-uses))
           ;; any definition might be referenced reflectively
           (values phase-body phase-internals phase-lifts phase-defined-names)]
          [else
           (log-info (indent "GCing definitions"))
           (gc-definitions linkl-mode phase-body phase-internals phase-lifts phase-first-internal-pos phase-merged-internals
                           phase-defined-names names phase-name-imports
                           #:initial-uses sub-accum-uses
                           #:accum-uses accum-uses
                           #:keep-defines? keep-syntax?
                           #:assume-pure? gc-toplevels?)]))

      (log-info (indent "Bundling linklet"))
      (define bundle (wrap-bundle linkl-mode phase-new-body phase-new-internals phase-new-lifts phase-import-keys
                                  portal-stxes phase-new-defined-names
                                  excluded-modules-to-require excluded-module-mpis provides
                                  names phase-name-imports
                                  get-merge-info
                                  (if (null? submod)
                                      root-sym
                                      (cons root-sym submod))
                                  #:export? keep-syntax?
                                  #:external-uses sub-accum-uses
                                  #:pre-submodules (if keep-submodules? pre-submods null)
                                  #:post-submodules (if keep-submodules? post-submods null)
                                  #:dump-output-file dump-output-file))

      (cond
        [(= 0 (hash-count directory-ht))
         (values bundle linkl-mode)]
        [else
         (define ht (hash-set directory-ht submod bundle))
         (values (linkl-directory ht) linkl-mode)]))))
