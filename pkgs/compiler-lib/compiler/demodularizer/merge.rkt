#lang racket/base
(require racket/match
         compiler/zo-structs
         "run.rkt"
         "name.rkt"
         "import.rkt")

(provide merge-linklets)

(define (merge-linklets runs names internals lifts imports)
  (define import-keys (hash-keys imports))
  (define import-counter 1)
  (define ordered-importss
    (for/list ([key (in-list import-keys)])
      (define ordered-imports (hash-ref imports key))
      (for ([name (in-list ordered-imports)])
        (define i (hash-ref names (cons key name)))
        (set-import-pos! i import-counter)
        (set! import-counter (add1 import-counter)))
      ordered-imports))
  (define import-shapess
    (for/list ([key (in-list import-keys)])
      (for/list ([name (in-list (hash-ref imports key))])
        (import-shape (hash-ref names (cons key name))))))

  (define positions
    (for/hash ([name (in-list (append internals lifts))]
               [i (in-naturals import-counter)])
      (values name i)))

  (define (make-position-mapping r)
    (define h (make-hasheqv))
    (define linkl (run-linkl r))
    (define importss (linkl-importss linkl))
    (define pos 1)
    (for ([imports (in-list importss)]
          [use (in-list (run-uses r))])
      (for ([name (in-list imports)])
        (hash-set! h pos (find-name names use name))
        (set! pos (add1 pos))))
    (define path/submod+phase (cons (run-path/submod r) (run-phase r)))
    (for ([name (in-list (append (linkl-exports linkl)
                                 (linkl-internals linkl)
                                 (linkl-lifts linkl)))]
          [pos (in-naturals pos)])
      (hash-set! h pos (find-name names path/submod+phase name)))
    h)

  (define saw-zero-pos-toplevel? #f)

  (define body
    (apply
     append
     (for/list ([r (in-list runs)])
       (define pos-to-name/import (make-position-mapping r))
       (define graph (make-hasheq))
       (make-reader-graph
        (for/list ([b (in-list (linkl-body (run-linkl r)))])
          (let remap ([b b])
            (match b
              [(toplevel depth pos const? ready?)
               (cond
                 [(zero? pos)
                  (set! saw-zero-pos-toplevel? #t)
                  b]
                 [else
                  (define new-name/import (hash-ref pos-to-name/import pos))
                  (define new-pos (if (import? new-name/import)
                                      (import-pos new-name/import)
                                      (hash-ref positions new-name/import)))
                  (toplevel depth new-pos const? ready?)])]
              [(def-values ids rhs)
               (def-values (map remap ids) (remap rhs))]
              [(inline-variant direct inline)
               (inline-variant (remap direct) (remap inline))]
              [(closure code gen-id)
               (cond
                 [(hash-ref graph gen-id #f)
                  => (lambda (ph) ph)]
                 [else
                  (define ph (make-placeholder #f))
                  (hash-set! graph gen-id ph)
                  (define cl (closure (remap code) gen-id))
                  (placeholder-set! ph cl)
                  cl])]
              [(let-one rhs body type unused?)
               (let-one (remap rhs) (remap body) type unused?)]
              [(let-void count boxes? body)
               (let-void count boxes? (remap body))]
              [(install-value count pos boxes? rhs body)
               (install-value count pos boxes? (remap rhs) (remap body))]
              [(let-rec procs body)
               (let-rec (map remap procs) (remap body))]
              [(boxenv pos body)
               (boxenv pos (remap body))]
              [(application rator rands)
               (application (remap rator) (map remap rands))]
              [(branch tst thn els)
               (branch (remap tst) (remap thn) (remap els))]
              [(with-cont-mark key val body)
               (with-cont-mark (remap key) (remap val) (remap body))]
              [(beg0 forms)
               (beg0 (map remap forms))]
              [(seq forms)
               (seq (map remap forms))]
              [(varref constant? toplevel dummy)
               (varref constant? (remap toplevel) (remap dummy))]
              [(assign id rhs undef-ok?)
               (assign id (remap rhs) undef-ok?)]
              [(apply-values proc args-expr)
               (apply-values (remap proc) (remap args-expr))]
              [(with-immed-mark key def-val body)
               (with-immed-mark (remap key) (remap def-val) (remap body))]
              [(case-lam name clauses)
               (case-lam name (map remap clauses))]
              [_
               (cond
                 [(lam? b)
                  (struct-copy lam b [body (remap (lam-body b))])]
                 [else b])])))))))

  (define new-linkl
    (linkl 'demodularized
           ordered-importss
           import-shapess
           '() ; exports
           internals
           lifts
           #hasheq()
           body
           (for/fold ([m 0]) ([r (in-list runs)])
             (max m (linkl-max-let-depth (run-linkl r))))
           saw-zero-pos-toplevel?))

  (linkl-bundle (hasheq 0 new-linkl)))

