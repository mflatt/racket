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
         "binding.rkt"
         "one-mod.rkt"
         "log.rkt")

(provide find-runs)

(define (find-runs top-path/submod
                   one-mods
                   excluded-module-mpis
                   #:max-phase max-phase)

  (define phase-rev-runs (make-hasheqv))  ; root-phase -> (list run ...) in reverse order
  (define excluded-modules-to-require (make-hash)) ; path/submod+phase-shift -> #t

  (define (find-phase-runs! path/submod
                            #:phase-level [phase-level 0]
                            #:root-phase [root-phase 0])
    (define runs-done (make-hash)) ; path+submod+phase -> #t

    (let find-loop ([path/submod path/submod]
                    ;; phase level within the module; note that this corresponds
                    ;; to the negation of a phase shift
                    [phase-level phase-level])
      (unless (hash-ref runs-done (cons path/submod phase-level) #f)
        (cond
          [(or (symbol? path/submod)
               (hash-ref excluded-module-mpis path/submod #f))
           => (lambda (excluded-mpi+phase)
                ;; Root of an excluded subtree; keep it as a `require`, even if there
                ;; turn out to be no imported variables at the linklet level. It's
                ;; possible that this subtree is covered by another one, and we clean
                ;; those up with a second pass.
                (hash-set! excluded-modules-to-require (cons path/submod (- root-phase phase-level)) #t))]
          [else
           (define one-m (hash-ref one-mods path/submod))
           (define decl (one-mod-decl one-m))
           (define stx-vec (one-mod-stx-vec one-m))
           (define stx-mpi (one-mod-stx-mpi one-m))
           (define zo (one-mod-zo one-m))

           (define linkl-table (if zo (linkl-bundle-table zo) #hasheqv()))
           (define linkl (hash-ref linkl-table phase-level #f))
           (define meta-linkl (hash-ref linkl-table (add1 phase-level) #f))
           (define uses (hash-ref (one-mod-phase-uses one-m) phase-level null))

           (define shifted-stx-vec
             (let ([phase-shift (- root-phase phase-level)])
               (if (eqv? phase-shift 0)
                   stx-vec
                   (and stx-vec
                        (for/vector ([e (in-vector stx-vec)]) (syntax-shift-phase-level e phase-shift))))))

           (define portal-stxes (hash-ref (one-mod-portal-stxes one-m) phase-level #hasheq()))
           
           (define r (run path/submod phase-level linkl meta-linkl
                          uses
                          #f ; import-map filled in later
                          shifted-stx-vec stx-mpi
                          portal-stxes))
           (hash-set! runs-done (cons path/submod phase-level) #t)

           (for* ([(phase-shift req-path/submods) (in-hash (one-mod-reqs one-m))]
                  [req-path/submod (in-list req-path/submods)])
             (define at-phase-level (- phase-level phase-shift))
             (find-loop req-path/submod at-phase-level))

           ;; Adding after requires, so that each list in `phase-runs` ends up in the
           ;; reverse order that we want to emit code
           (when linkl (hash-set! phase-rev-runs root-phase (cons r (hash-ref phase-rev-runs root-phase null))))]))))

  (define (clear-redundant-excluded-to-require!)
    ;; Clear redundant requires based on transitive requires of the source modules.
    ;; We'll leave it to a later pass that maps source modules to panes to remove
    ;; duplicate panes.
    (define done (make-hash))
    (for ([path/submod+phase (in-list (hash-keys excluded-modules-to-require))]
          #:unless (symbol? (car path/submod+phase)))
      (let loop ([path/submod+phase-shift path/submod+phase]
                 [excluded? #f])
        (unless (hash-ref done path/submod+phase #f)
          (define path/submod (car path/submod+phase-shift))
          (define phase-shift (cdr path/submod+phase-shift))
          (define one-m (hash-ref one-mods path/submod))
          
          (for* ([(req-phase req-path/submods) (in-hash (one-mod-reqs one-m))]
                 [req-path/submod (in-list req-path/submods)])
            (define at-phase-shift (+ phase-shift req-phase))
            (define req-path/submod+phase-shift (cons req-path/submod at-phase-shift))
            (when excluded?
              (hash-remove! excluded-modules-to-require req-path/submod+phase-shift))
            (unless (symbol? req-path/submod)
              (loop req-path/submod+phase-shift
                    (or excluded?
                        (hash-ref excluded-modules-to-require req-path/submod+phase-shift #f)))))

          (hash-set! done path/submod+phase #t)))))

  (define top-m (hash-ref one-mods top-path/submod))

  (for ([root-phase (in-range (one-mod-min-phase top-m) (add1 (one-mod-max-phase top-m)))])
    (find-phase-runs! top-path/submod
                      #:phase-level root-phase
                      #:root-phase root-phase))

  (clear-redundant-excluded-to-require!)

  (define (keep-phase? root-phase)
    (or (not max-phase)
        (root-phase . <= . max-phase)))

  (log-demodularizer-debug " Merging for ~a:" top-path/submod)
  (for* ([(phase rev-runs) (in-hash phase-rev-runs)]
         #:when (keep-phase? phase)
         #:do [(log-demodularizer-debug "  ~a:" phase)]
         [r (in-list (reverse rev-runs))])
    (log-demodularizer-debug "    ~a ~a" (run-path/submod r) (run-phase r)))
  (log-demodularizer-debug "  require:")
  (for ([path/submod+phase (in-hash-keys excluded-modules-to-require)])
    (log-demodularizer-debug "    ~a ~a" (car path/submod+phase) (cdr path/submod+phase)))

  (values (for/hasheqv ([(root-phase rev-runs) (in-hash phase-rev-runs)]
                        #:when (keep-phase? root-phase))
            (values root-phase (reverse rev-runs)))
          excluded-modules-to-require))
