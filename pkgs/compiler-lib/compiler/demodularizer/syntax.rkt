#lang racket/base
(require (only-in '#%kernel [syntax-serialize kernel:syntax-serialize])
         racket/linklet
         "linklet.rkt")

(provide register-provides-for-syntax
         deserialize-syntax
         serialize-syntax
         build-stx-data-linklet
         build-stx-linklet)

(define (register-provides-for-syntax register! bulk-binding-registry
                                      orig-path submod
                                      decl
                                      real-decl)
  (register! bulk-binding-registry
             (make-resolved-module-path (let ([p (if (string? orig-path)
                                                     (string->path orig-path)
                                                     orig-path)])
                                          (if (pair? submod) (cons p submod) p)))
             (instance-variable-value decl 'self-mpi)
             (instance-variable-value real-decl 'provides)))

(define (deserialize-syntax real-deserialize-instance stx-data-linklet data-instance
                            bulk-binding-registry)
  (cond
    [stx-data-linklet
     (define stx-data-instance (instantiate-linklet stx-data-linklet
                                                    (list real-deserialize-instance
                                                          data-instance)))
     (define vec (instance-variable-value stx-data-instance '.deserialized-syntax-vector))
     (when vec
       (unless (vector-ref vec 0)
         (define deserialize-syntax (instance-variable-value stx-data-instance '.deserialize-syntax))
         (deserialize-syntax bulk-binding-registry)))
     vec]
    [else #f]))

(define (serialize-syntax stx-vec stx-mpi-map import-keys excluded-module-mpis names)
  (define self-mpi (module-path-index-join #f #f))

  (define import-mpis
    (for/list ([path/submod+phase (in-list import-keys)])
      (define path (car path/submod+phase))
      (cond
        [(symbol? path) (module-path-index-join `(quote ,path) #f)]
        [else (hash-ref excluded-module-mpis path)])))
  
  (define-values (serialized-stx stx-mpis-vec)
    (cond
      [(= 0 (vector-length stx-vec))
       (values #f (list->vector (cons self-mpi import-mpis)))]
      [else
       (kernel:syntax-serialize stx-vec
                                #f ; base-mpi
                                '() ; preserve-prop-keys
                                #f ; provides-namespace
                                #f ; as-data?
                                (cons self-mpi import-mpis) ;; these mpis first, needed for imports below
                                ;; map-mpi
                                (lambda (mpi)
                                  (cond
                                    [(hash-ref stx-mpi-map mpi #f) self-mpi]
                                    [else
                                     (define-values (name base) (module-path-index-split mpi))
                                     (if (or name base)
                                         mpi
                                         self-mpi)]))
                                ;; map-binding-symbol
                                (lambda (mpi sym)
                                  (cond
                                    [(hash-ref stx-mpi-map mpi #f)
                                     => (lambda (path/submod)
                                          (hash-ref names (cons path/submod sym) sym))]
                                    [else sym])))]))

  (for ([stx-mpi (in-vector stx-mpis-vec)]
        [orig-mpi (in-list (cons self-mpi import-mpis))])
    (unless (eq? stx-mpi orig-mpi)
      (error "unexpected MPI for import")))

  (define all-mpis (vector->list stx-mpis-vec))

  (values self-mpi all-mpis serialized-stx))

(define (build-stx-data-linklet stx-vec serialized-stx)
  (s-exp->linklet
   'syntax-literals-data
   `(linklet
        ([deserialize-module-path-indexes
          syntax-module-path-index-shift
          syntax-shift-phase-level
          module-use
          deserialize]
         [.mpi-vector])
        (.deserialized-syntax-vector
         .deserialize-syntax)
      (define-values (.deserialized-syntax-vector)
        (make-vector ,(vector-length stx-vec) #f))
      (define-values (.deserialize-syntax)
        (lambda (.bulk-binding-registry)
          (begin
            (vector-copy! .deserialized-syntax-vector
                          '0
                          (let-values ([(.inspector) #f])
                            ,serialized-stx))
            (set! .deserialize-syntax #f)))))))

(define (build-stx-linklet stx-vec)
  (s-exp->linklet
   'syntax-literals
   `(linklet
        ([force-syntax-object]
         [.mpi-vector]
         [.deserialized-syntax-vector
          .deserialize-syntax]
         [.namespace
          .phase
          .self
          .inspector
          .bulk-binding-registry
          .set-transformer!])
        (.get-syntax-literal!
         get-encoded-root-expand-ctx)
      (define-values (.syntax-literals)
        (make-vector ,(vector-length stx-vec) #f))
      (define-values (.get-syntax-literal!)
        (lambda (pos)
          (let-values ([(ready-stx) (unsafe-vector*-ref .syntax-literals pos)])
            (if ready-stx
                ready-stx
                (force-syntax-object .syntax-literals
                                     pos
                                     (vector-ref .mpi-vector 0) ; compile-time self
                                     .self ; run-time self
                                     .phase
                                     .inspector
                                     .deserialized-syntax-vector
                                     .bulk-binding-registry
                                     .deserialize-syntax)))))
      (define-values (get-encoded-root-expand-ctx) (lambda () #f)))))
