#lang racket/base
(require racket/match
         "../private/deserialize.rkt"
         "import.rkt")

(provide binding-module-path-index-shift
         binding-mpis
         binding-sym
         serialize-binding)

(define (binding-module-path-index-shift bind from-mpi to-mpi)
  (cond
    [(provided? bind) (struct-copy provided bind
                                   [binding (binding-module-path-index-shift (provided-binding bind)
                                                                             from-mpi
                                                                             to-mpi)])]
    [else
     (define (shift mpi)
       (cond
         [(eq? mpi from-mpi) to-mpi]
         [else
          (define-values (name base) (module-path-index-split mpi))
          (define new-base (and base (shift base)))
          (if (eq? new-base base)
              mpi
              (module-path-index-join name new-base))]))
     (define content (binding-content bind))
     (define new-content
       (match content
         [`(,mod ,sym ,phase ,nom-mod) (list (shift mod) sym phase (shift nom-mod))]
         [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
          ;; Currently dropping free-id=? and extra nominals
          (list (shift mod) sym phase (shift nom-mod) nom-phase nom-sym req-phase #f insp null)]))
     (struct-copy binding bind
                  [content new-content])]))

(define (binding-mpis bind)
  (cond
    [(provided? bind) (binding-mpis (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) (list mod nom-mod)]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        (list mod nom-mod)])]))

(define (binding-sym bind)
  (cond
    [(provided? bind) (binding-sym (provided-binding bind))]
    [else
     (match (binding-content bind)
       [`(,mod ,sym ,phase ,nom-mod) sym]
       [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
        sym])]))

(define (serialize-binding bind root-phase
                           external-path-pos excluded-module-mpis included-module-phases
                           names transformer-names name-imports
                           mpi-count)
  (let loop ([bind bind])
    (cond
      [(provided? bind)
       `(#:provided
         ,@(loop (provided-binding bind))
         ,(provided-protected? bind)
         ,(provided-syntax? bind))]
      [else
       (define (lookup mpi)
         (define r (module-path-index-resolve mpi))
         (define pos
           (or (hash-ref external-path-pos (resolved-module-path-name r) #f)
               ;; self-mpi:
               0))
         (when (pos . >= . mpi-count)
           (error 'bundle-binding "nonsense pos: ~a for ~s" pos (resolved-module-path-name r)))
         pos)
       (define (lookup-sym mpi phase sym)
         (define r (module-path-index-resolve mpi))
         (define path/submod (resolved-module-path-name r))
         (cond
           [(symbol? path/submod)
            (values sym 0)]
           [(or (hash-ref names (cons (cons path/submod phase) sym) #f)
                (hash-ref transformer-names (cons (cons path/submod phase) sym) #f))
            => (lambda (new-sym)
                 (cond
                   [(hash-ref name-imports new-sym #f)
                    => (lambda (i)
                         (values (import-src-ext-name i) (cdr (import-path/submod+phase i))))]
                   [else
                    ;; Get a potential phase shift
                    (define mpi+phase (hash-ref excluded-module-mpis path/submod #f))
                    (define phase-shift (if mpi+phase
                                            (cdr mpi+phase)
                                            (hash-ref included-module-phases path/submod 0)))
                    (values new-sym (+ phase phase-shift))]))]
           [(hash-ref excluded-module-mpis path/submod #f)
            (values sym phase)]
           [else
            (error 'provides
                   "cannot find name for provided identifier: ~s ~s" sym mpi)]))
       (match (binding-content bind)
         [`(,mod ,sym ,phase ,nom-mod)
          (define-values (new-sym new-phase) (lookup-sym mod phase sym))
          `(#:simple-module-binding
            #:mpi ,(lookup mod)
            ,new-sym
            ,new-phase
            #:mpi ,(lookup nom-mod))]
         [`(,mod ,sym ,phase ,nom-mod ,nom-phase ,nom-sym ,req-phase ,free-id ,insp ,more-noms)
          ;; Currently dropping free-id=? and extra nominals
          (define-values (new-sym new-phase) (lookup-sym mod phase sym))
          `(#:module-binding
            #:mpi ,(lookup mod)
            ,new-sym
            ,new-phase
            #:mpi ,(lookup nom-mod)
            ,nom-phase
            ,nom-sym
            ,req-phase
            ,#f
            ,insp
            ,null)])])))
