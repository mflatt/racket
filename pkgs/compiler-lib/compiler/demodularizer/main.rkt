#lang racket/base
(require racket/set
         compiler/cm
         "find.rkt"
         "name.rkt"
         "merge.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt"
         racket/pretty)

(provide demodularize

         garbage-collect-toplevels-enabled
         current-excluded-modules
         recompile-enabled)

(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize input-file [given-output-file #f]
                      #:work-directory [given-work-directory #f])
  (define work-directory (and (eq? 'chez-scheme (system-type 'vm))
                              (or given-work-directory
                                  (build-path (find-system-path 'temp-dir) "demod-work"))))
  
  (parameterize ([current-logger logger]
                 [current-excluded-modules (for/set ([path (in-set (current-excluded-modules))])
                                             (normal-case-path (simplify-path (path->complete-path path))))])

    (cond
      [work-directory
       (log-info "Compiling modules to ~s" work-directory)
       (parameterize ([current-namespace (make-empty-namespace)]
                      [current-compiled-file-roots (list (build-path work-directory "native")
                                                         (build-path work-directory "linklet"))]
                      [current-compile-target-machine #f]
                      [current-multi-compile-any #t])
         (namespace-attach-module (variable-reference->namespace (#%variable-reference)) ''#%builtin)
         (managed-compile-zo input-file))]
      [else
       (log-info "Compiling module")
       (parameterize ([current-namespace (make-base-empty-namespace)])
         (managed-compile-zo input-file))])

    (log-info "Finding modules")
    (define-values (runs excluded-module-mpis)
      (parameterize ([current-compiled-file-roots (if work-directory
                                                      (list (build-path work-directory "linklet"))
                                                      (current-compiled-file-roots))])
        (find-modules input-file)))

    (log-info "Selecting names")
    (define-values (names internals lifts imports) (select-names runs))

    (log-info "Merging linklets")
    (define-values (body first-internal-pos new-internals linkl-mode get-merge-info)
      (merge-linklets runs names internals lifts imports))

    (log-info "GCing definitions")
    (define-values (new-body gced-internals new-lifts)
      (gc-definitions linkl-mode body internals lifts first-internal-pos new-internals
                      #:assume-pure? (garbage-collect-toplevels-enabled)))

    (pretty-print new-body)

    (log-info "Bundling linklet")
    (define bundle (wrap-bundle new-body gced-internals new-lifts
                                excluded-module-mpis
                                get-merge-info
                                (let-values ([(base name dir?) (split-path input-file)])
                                  (string->symbol (path->string name)))))

    (log-info "Writing bytecode")
    (define output-file (or given-output-file
                            (path-add-suffix input-file #"_merged.zo")))
    (write-module output-file bundle)

    (when (recompile-enabled)
      (log-info "Recompiling and rewriting bytecode")
      (define zo (compiled-expression-recompile
                  (parameterize ([read-accept-compiled #t])
                    (call-with-input-file* output-file read))))
      (call-with-output-file* output-file
                              #:exists 'replace
                              (lambda (out) (write zo out))))))
