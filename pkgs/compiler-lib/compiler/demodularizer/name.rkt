#lang racket/base
(require racket/match
         "linklet.rkt"
         "run.rkt"
         "import.rkt"
         "remap.rkt"
         (only-in racket/linklet linklet-body-reserved-symbol?))

(provide select-names
         find-name)

(struct select-state (names used-names phase-name-imports))

(define (select-names phase-runs
                      #:state state)
  (define (make-table make)
    (for/hasheqv ([phase (in-hash-keys phase-runs)])
      (values phase (make))))
  (define (phase-map table f)
    (for/hasheqv ([(phase v) (in-hash table)])
      (values phase (f v))))

  ;; Don't need a per-root-phase table of names, because
  ;; we'll pick a consistent name for a given module and phase-level
  ;; no matter which root phase it's in
  (define names   ; path/submod+phase+sym -> symbol
    (if state
        (hash-copy (select-state-names state))
        (make-hash)))
  (define used-names
    (if state
        (hash-copy (select-state-used-names state))
        (make-hasheq)))

  ;; We do need per-root-phase import table mapping new
  ;; names to import information. In the case of a submodule,
  ;; the encloding module might provide the same binding as
  ;; an import in some root phases and not in others.
  (define phase-name-imports
    (if state
        ;; Any name bound by the encloding submodule is an import
        ;; for the submodule at root phase 0
        (let ([table (make-table make-hasheq)])
          (for/fold ([table table]) ([(phase name-imports) (in-hash (select-state-phase-name-imports state))])
            (hash-set table phase (hash-copy name-imports))))
        (make-table make-hasheq)))
  
  (define phase-internals (make-table (lambda () (box '()))))
  (define phase-lifts (make-table (lambda () (box '()))))
  (define phase-imports (make-table make-hash)) ; root-phase -> path/submod+phase -> (list (cons path/submod+phase sym) ...)
  (define phase-imports-done (make-table make-hash)) ; root-phase -> 

  (define done (make-hash))

  (define phase-submod-name-imports (make-table make-hasheq))

  ;; choose names in phase 0, first, to bias name choice toward that phase
  (for ([root-phase (cons 0 (remv 0 (hash-keys phase-runs)))])
    (define runs (hash-ref phase-runs root-phase))
    (define lifts (hash-ref phase-lifts root-phase))
    (define internals (hash-ref phase-internals root-phase))
    (define imports (hash-ref phase-imports root-phase))
    (define name-imports (hash-ref phase-name-imports root-phase))
    (define submod-name-imports (hash-ref phase-submod-name-imports root-phase))

    (define defined-names (make-hasheq))

    ;; Reserve the syntax-literals and transformer-register names:
    (define reserved-names '(.get-syntax-literal!
                             .set-transformer!))

    (define (pick-name name)
      (let loop ([try-name name] [i 0])
        (cond
          [(or (linklet-body-reserved-symbol? try-name)
               (hash-ref used-names try-name #f))
           (let ([i (add1 i)])
             (loop (string->symbol (format "~a_~a" name i)) i))]
          [else
           (hash-set! used-names try-name #t)
           try-name])))
    
    (for ([r (in-list (reverse runs))]) ; biases names to starting module
      (define linkl (run-linkl r))
      (define meta-linkl (run-meta-linkl r))
      (define portal-stxes (run-portal-stxes r))
      (define path/submod+phase (cons (run-path/submod r) (run-phase r)))

      ;; Same linklet can be used (as shifted) in multiple root phases,
      ;; so check that we haven't covered this one already:
      (cond
        [(hash-ref done path/submod+phase #f)
         => (lambda (run-defined-names)
              (for ([name (in-hash-keys run-defined-names)])
                (hash-set! defined-names name #t)
                (hash-set! submod-name-imports name (import name root-phase #f name #f))))]
        [else
         (define run-defined-names (make-hasheq))
         (hash-set! done path/submod+phase run-defined-names)

         (define (record-defined! new-name)
           (hash-set! run-defined-names new-name #t)
           (hash-set! defined-names new-name #t)
           ;; Anything defined in this module becomes an import for submodules:
           (hash-set! submod-name-imports new-name (import new-name root-phase #f new-name #f)))
      
         ;; Process local definitions, first
         (define (select-names! name-list category)
           (for ([name (in-list name-list)])
             (cond
               [(hash-ref names (cons path/submod+phase name) #f)
                => (lambda (new-name) (record-defined! new-name))]
               [else
                (define new-name (pick-name name))
                (hash-set! names (cons path/submod+phase name) new-name)
                (when category
                  (set-box! category (cons new-name (unbox category))))
                (record-defined! new-name)])))

         (select-names! (linklet*-exports linkl) internals)
         (select-names! (linklet*-internals linkl) internals)
         (select-names! (linklet*-lifts linkl) lifts)
         
         (when meta-linkl
           (remap-names (linklet*-body meta-linkl)
                        (lambda (name) name)
                        #:application-hook
                        (lambda (rator rands remap)
                          (cond
                            [(eq? rator '.set-transformer!)
                             (match rands
                               [`((quote ,name) ,_)
                                (select-names! (list name) #f)]
                               [_ (error "unrecognized transformer registration")])]))))

         (select-names! (hash-keys portal-stxes) #f)]))

    ;; Record any imports that will remain as imports; anything
    ;; not yet mapped must be a leftover import, plus anything mapped
    ;; as an import by a supermodule
    (define imports-done (make-hash)) ; path/submod+phase -> set-of-sym
    (for ([r (in-list runs)])
      (define linkl (run-linkl r))
      (for ([import-names (in-list (linklet*-importss linkl))]
            [import-internal-names (in-list (linklet*-internal-importss linkl))]
            [import-shapes (in-list (linklet*-import-shapess linkl))]
            [use (in-list (run-uses r))]
            [import-use (in-list (run-import-uses r))])
        (for ([name (in-list import-names)]
              [internal-name (in-list import-internal-names)]
              [shape (in-list import-shapes)])
          (define n (hash-ref names (cons use name) #f))
          (define defined? (and n (hash-ref defined-names n #f)))
          (define i (and n (hash-ref name-imports n #f)))
          (unless (and defined? (not i))
            ;; either new import or supermodule import (which is maybe new to here)
            (define done-names (hash-ref imports-done use #hasheq()))
            (unless (hash-ref done-names name #f)
              (hash-set! imports import-use (cons (cons use name)
                                                  (hash-ref imports import-use null)))
              (hash-set! imports-done use (hash-set done-names name #t))))
          (unless defined?
            (define new-name ; used for S-expression mode
              (cond
                [n n]
                [else
                 (define n
                   (if (memq internal-name reserved-names)
                       internal-name
                       (pick-name internal-name)))
                 (hash-set! names (cons use name) n)
                 n]))
            (unless (hash-ref name-imports new-name #f) ; may be propogated for supermodule
              (hash-set! name-imports new-name (import name (cdr use) shape new-name #f))))))))

  ;; Propagate any imports to submodules
  (for ([(root-phase submod-name-imports) (in-hash phase-submod-name-imports)])
    (for ([(name i) (in-hash (hash-ref phase-name-imports root-phase))])
      (hash-set! submod-name-imports name i)))

  (define new-state (select-state names used-names phase-submod-name-imports))

  (values names (phase-map phase-internals unbox) (phase-map phase-lifts unbox)
          phase-name-imports phase-imports
          new-state))

(define (find-name names use name)
  (hash-ref names (cons use name)))
