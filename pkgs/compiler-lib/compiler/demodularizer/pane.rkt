#lang racket/base
(require racket/path
         racket/set
         "module-path.rkt"
         "path-submod.rkt"
         "one-mod.rkt"
         "log.rkt")

(provide partition-panes
         reify-panes)

;; Returns an order list mapping an optional path/submod to the path/submods that
;; are to be uniquely demodularized into the path/submod. If the option path/submod
;; is #f, that's a module to reference directly, because it cannot be usefuly
;; combined with any other module and `external-singetons?` is true. Every non-excluded
;; module is represented exactly once in the list.
(define (partition-panes one-mods orig-top-path submods
                         #:external-singetons? [external-singletons? #t])
  (define top-path (normalize-path orig-top-path))

  ; pane = (cons (set entry ...) (set phase ...))

  (define uses (make-hash)) ; path/submod -> pane

  (define (traverse! path/submod entry phase)
    (unless (symbol? path/submod)
      (define one-m (hash-ref one-mods path/submod))
      (unless (one-mod-excluded? one-m)
        (define done (hash-ref uses path/submod '(#hash() . #hash())))
        (unless (and (hash-ref (car done) entry #f)
                     (hash-ref (cdr done) phase #f))
          (hash-set! uses path/submod
                     (cons (hash-set (car done) entry #t)
                           (hash-set (cdr done) phase #t)))
          (for* ([(phase-shift path/submods) (in-hash (one-mod-reqs one-m))]
                 [(path/submod) (in-list path/submods)])
            (traverse! path/submod entry (+ phase phase-shift)))))))

  (for ([submod (in-list (cons '() submods))])
    (traverse! (path/submod-join top-path submod) submod 0))

  (define pre-panes ; pane -> (set path/submod ...)
    (for/fold ([panes (hash)]) ([(path/submod entries-and-phases) (in-hash uses)])
      (hash-set panes entries-and-phases
                (hash-set (hash-ref panes entries-and-phases (hash))
                          path/submod
                          #t))))

  ;; If two panes have the same entry points and the same phase shifts, but
  ;; shifted releative to each other, then the panes can be merged
  (define merges ; pane -> (cons pane-to-merge-key-into phase shift)
    (let loop ([entries+phasess (sort (hash-keys pre-panes)
                                      <
                                      #:key (lambda (entrys+phases)
                                              (apply min (hash-keys (cdr entrys+phases)))))]
               [merges (hash)])
      (cond
        [(null? entries+phasess) merges]
        [else
         (define entries+phases (car entries+phasess))
         (cond
           [(hash-ref merges entries+phases #f)
            ;; already merged
            (loop (cdr entries+phasess) merges)]
           [else
            (define new-merges
              (for/fold ([merges merges]) ([entries+phases2 (in-list (cdr entries+phasess))])
                (cond
                  [(equal? (car entries+phases) (car entries+phases2))
                   (define phases (cdr entries+phases))
                   (define phases2 (cdr entries+phases2))
                   (define (get-min-phase phases) (apply min (hash-keys phases)))
                   (cond
                     [(and (= (hash-count phases) (hash-count phases2))
                           (let ([delta (- (get-min-phase phases2)
                                           (get-min-phase phases))])
                             (and (for/and ([phase (in-hash-keys phases)])
                                    (hash-ref phases2 (+ phase delta) #f))
                                  delta)))
                      => (lambda (delta)
                           (hash-set merges entries+phases2 (cons entries+phases
                                                                  delta)))]
                     [else merges])]
                  [else merges])))
            (loop (cdr entries+phasess) new-merges)])])))

  (define merge-ins ; pane -> (list (cons panes-to-merge-into-key phase-shift))
    (for/fold ([merge-ins #hash()]) ([(from to+delta) (in-hash merges)])
      (define to (car to+delta))
      (define delta (cdr to+delta))
      (hash-set merge-ins to (cons (cons from delta) (hash-ref merge-ins to null)))))
  
  (define panes ; pane -> (list (cons path/submod phase-shift) ...)
    (for/hash ([(pane path/submods) (in-hash pre-panes)]
               #:unless (hash-ref merges pane #f))
      (define path/submod+shifts
        (append (for/list ([path/submod (in-hash-keys path/submods)])
                  (cons path/submod 0))
                (apply
                 append
                 (for/list ([pane+delta (in-list (hash-ref merge-ins pane null))])
                   (define pane (car pane+delta))
                   (define delta (cdr pane+delta))
                   (for/list ([path/submod (in-hash-keys (hash-ref pre-panes pane))])
                     (cons path/submod delta))))))
      (values pane
              (sort path/submod+shifts <
                    #:key (lambda (path/submod+delta)
                            (one-mod-order (hash-ref one-mods (car path/submod+delta))))))))

  ;; Name the panes, using an existing submodule name if one is within the pane,
  ;; or an external module if there's only one module in the pane
  (define-values (named-panes ; (list (cons path/submod-or-#f (list (cons path/submod delta) ...)) ...)
                  added-submods)
    (for/fold ([named-panes null]
               [added-submods null])
              ([(pane path/submod+deltas) (in-hash panes)]
               [i (in-naturals)])
      (define unique-submod
        (for/fold ([submod #f]) ([path/submod+delta (in-list path/submod+deltas)])
          (define path/submod (car path/submod+delta))
          (cond
            [(eq? submod 'many) 'many]
            [(equal? (path/submod-path path/submod) top-path)
             (if (not submod)
                 (path/submod-submod path/submod)
                 'many)]
            [else submod])))
      (when (eq? unique-submod 'many)
        (error "two entry-point submodules are in the same pane"))
      (define-values (name added-submod)
        (cond
          [unique-submod
           (values (path/submod-join top-path unique-submod)
                   #f)]
          [(and (null? (cdr path/submod+deltas))
                external-singletons?)
           ;; one none-submodule; no demodularization is useful
           (values #f #f)]
          [else
           (define added-submod (string->symbol (format "demod-pane-~a" i)))
           (values (path/submod-join top-path (list added-submod))
                   added-submod)]))

      (log-demodularizer-debug "  ~a = ~a ~a"
                               name
                               (hash-keys (car pane))
                               (hash-keys (cdr pane)))

      (values (cons (cons name
                          path/submod+deltas)
                    named-panes)
              (if added-submod
                  (cons added-submod added-submods)
                  added-submods))))

  ;; sort panes based on shallowest (largest order index) module in pane
  (define sorted-panes ; (list (cons path/submod-or-#f (list (cons path/submod delta) ...)) ...)
    (sort named-panes
          <
          #:cache-keys? #t
          #:key (lambda (pane+path/submod+deltas)
                  (apply max (for/list ([path/submod+delta (in-list (cdr pane+path/submod+deltas))])
                               (define path/submod (car path/submod+delta))
                               (define m (hash-ref one-mods path/submod))
                               (one-mod-order m))))))

  (values sorted-panes
          added-submods))

;; Remove panes that have `#f` names, and set the corresponding module in `one-mods`
;; to be excluded. For panes that are new, synthesized submodules, create
;; an entry on `one-mods` for the modules. Return just the list of path/names
;; for the submodules to (re-)export demodularized content.
(define (reify-panes sorted-panes one-mods common-excluded-module-mpis)
  (define new-sorted-panes
    (for/list ([path/submod+pane-content (in-list sorted-panes)]
               #:do [(define path/submod (car path/submod+pane-content))
                     (when (not path/submod)
                       ;; Singleton to change to excluded
                       (define content (cdr path/submod+pane-content))
                       (define path/submod+phase (car content))
                       (define path/submod (car path/submod+phase))
                       (define one-m (hash-ref one-mods path/submod))
                       (log-demodularizer-debug " Dropping single-module pane: ~a" path/submod)
                       (set! common-excluded-module-mpis
                             (hash-set common-excluded-module-mpis path/submod (cons (one-mod-rel-mpi one-m) 0)))
                       (hash-set! one-mods path/submod (struct-copy one-mod one-m
                                                                    [excluded? #t])))]
               #:when path/submod)
      (unless (hash-ref one-mods path/submod #f)
        ;; Synthesize a `one-mod` record for submodule that holds a pane
        (define rev-reqs
          (for/fold ([reqs #hasheqv()])
                    ([path/submod+delta (in-list (cdr path/submod+pane-content))])
            (define path/submod (car path/submod+delta))
            (define delta (cdr path/submod+delta))
            (hash-set reqs delta (cons path/submod (hash-ref reqs delta null)))))
        (define-values (min-phase max-phase)
          (for/fold ([min-phase 0]
                     [max-phase 0])
                    ([path/submod+delta (in-list (cdr path/submod+pane-content))])
            (define path/submod (car path/submod+delta))
            (define m (hash-ref one-mods path/submod))
            (values (min min-phase (one-mod-min-phase m))
                    (max max-phase (one-mod-max-phase m)))))
        (hash-set! one-mods path/submod
                   (one-mod 0
                            #f ; excluded?
                            #f ; rel-mpi
                            #f ; zo
                            #f ; decl
                            #hasheqv() ; phase-uses
                            (for/hasheqv ([(phase rev-path/submods) (in-hash rev-reqs)])
                              (values phase (reverse rev-path/submods)))
                            #hasheqv() ; exports
                            min-phase
                            max-phase
                            #hasheqv() ; provides
                            #() ; stx-vec
                            #f ; stx-mpi
                            #hasheqv() ; portal-stxes
                            null       ; pre-submodules
                            null)))    ; post-submodules
      path/submod+pane-content))
  
  ;; For each pane submodule, build an exclusion list that points the other submodules
  (define self-mpi (module-path-index-join #f #f))
  (define excluded-module-mpiss
    (for/list ([path/submod+pane-content (in-list new-sorted-panes)])
      (define path/submod (car path/submod+pane-content))
      (define submod (path/submod-submod path/submod))
      (define dots (map (lambda (s) "..") submod))
      (for/fold ([excluded-module-mpis common-excluded-module-mpis])
                ([other-path/submod+pane-content (in-list new-sorted-panes)]
                 #:do [(define other-path/submod (car other-path/submod+pane-content))
                       (define pane-content (cdr other-path/submod+pane-content))]
                 #:unless (equal? path/submod other-path/submod))
        (define other-submod (path/submod-submod other-path/submod))
        (define mpi (let* ([mpi self-mpi]
                           [mpi (if (pair? dots)
                                    (module-path-index-join `(submod ,@dots) mpi)
                                    mpi)]
                           [mpi (if (pair? other-submod)
                                    (module-path-index-join `(submod "." ,@other-submod) mpi)
                                    mpi)])
                      mpi))
        (for/fold ([excluded-module-mpis excluded-module-mpis])
                  ([path/submod+phase (in-list pane-content)])
          (define path/submod (car path/submod+phase))
          (hash-set excluded-module-mpis path/submod (cons mpi (cdr path/submod+phase)))))))

  (log-demodularizer-debug " Panes: ~a" (length new-sorted-panes))
  (for ([phase/submod+content (in-list new-sorted-panes)])
    (define phase/submod (car phase/submod+content))
    (define content (cdr phase/submod+content))
    (log-demodularizer-debug "  ~s:" phase/submod)
    (for ([path/submod+phase (in-list content)])
      (log-demodularizer-debug "    ~a ~a" (car path/submod+phase) (cdr path/submod+phase))))

  (values (map car new-sorted-panes)
          excluded-module-mpiss
          ;; `one-mods` return value is just a hacky hint that this function is meant to change it
          one-mods))
