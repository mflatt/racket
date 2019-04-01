
(define (authentic-hash? v) (or (intmap? v) (mutable-hash? v)))
(define (hash? v) (or (authentic-hash? v)
                      (and (impersonator? v)
                           (authentic-hash? (impersonator-val v)))))

(define-syntax define-hash-constructors
  (syntax-rules ()
    [(_ vararg-ctor list-ctor empty-hash)
     (begin
       (define vararg-ctor
         (case-lambda
          [() empty-hash]
          [kvs
           (let loop ([kvs kvs] [h empty-hash])
             (cond
              [(null? kvs) h]
              [else (loop (cddr kvs) (intmap-set h (car kvs) (cadr kvs)))]))]))

       (define list-ctor
         (case-lambda
          [() empty-hash]
          [(alist)
           (check 'list-ctor
                  :test (and (list? alist) (andmap pair? alist))
                  :contract "(listof pair?)"
                  alist)
           (let loop ([h (vararg-ctor)] [alist alist])
             (if (null? alist)
                 h
                 (loop (intmap-set h (caar alist) (cdar alist))
                       (cdr alist))))])))]))

(define-hash-constructors hash make-immutable-hash empty-hash)
(define-hash-constructors hasheqv make-immutable-hasheqv empty-hasheqv)
(define-hash-constructors hasheq make-immutable-hasheq empty-hasheq)

(define (hash-set! ht k v)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (mutable-hash-set! ht k v)
    (lock-release (mutable-hash-lock ht))]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (impersonate-hash-set! ht k v)]
   [else (raise-argument-error 'hash-set! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-remove! ht k)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (mutable-hash-remove! ht k)
    (lock-release (mutable-hash-lock ht))]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (impersonate-hash-remove! ht k)]
   [else (raise-argument-error 'hash-remove! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-clear! ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (mutable-hash-clear! ht)
    (lock-release (mutable-hash-lock ht))]
   [(and (impersonator? ht)
         (let ([ht (impersonator-val ht)])
           (mutable-hash? ht)))
    (unless (impersonate-hash-clear ht #t)
      ;; fall back to iterated remove
      (let loop ([i (hash-iterate-first ht)])
          (when i
            (hash-remove! ht (hash-iterate-key ht i))
            (loop (hash-iterate-next ht i)))))]
   [else (raise-argument-error 'hash-clear! "(and/c hash? (not/c immutable?))" ht)]))

(define (hash-copy ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (let ([new-ht (mutable-hash-copy ht)])
      (lock-release (mutable-hash-lock ht))
      new-ht)]
   [(intmap? ht)
    (let ([new-ht (cond
                   [(intmap-eq? ht) (make-hasheq)]
                   [(intmap-eqv? ht) (make-hasheqv)]
                   [else (make-hash)])])
      (let loop ([i (intmap-iterate-first ht)])
        (when i
          (let-values ([(k v) (intmap-iterate-key+value ht i #f)])
            (hash-set! new-ht k v)
            (loop (intmap-iterate-next ht i)))))
      new-ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-copy ht)]
   [else (raise-argument-error 'hash-copy "hash?" ht)]))

(define (hash-set ht k v)
  (cond
   [(intmap? ht) (intmap-set ht k v)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-set ht k v)]
   [else (raise-argument-error 'hash-set "(and/c hash? immutable?)" ht)]))

(define (hash-remove ht k)
  (cond
   [(intmap? ht) (intmap-remove ht k)]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (impersonate-hash-remove ht k)]
   [else (raise-argument-error 'hash-remove "(and/c hash? immutable?)" ht)]))

(define (hash-clear ht)
  (cond
   [(intmap? ht)
    (cond
     [(hash-eq? ht) empty-hasheq]
     [(hash-eqv? ht) empty-hasheqv]
     [else empty-hash])]
   [(and (impersonator? ht)
         (intmap? (impersonator-val ht)))
    (or (impersonate-hash-clear ht #f)
        ;; fall back to iterated remove
        (let loop ([ht ht])
          (let ([i (hash-iterate-first ht)])
            (if i
                (loop (hash-remove ht (hash-iterate-key ht i)))
                ht))))]
   [else (raise-argument-error 'hash-clear! "(and/c hash? immutable?)" ht)]))

(define (hash-eq? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (mutable-hash-kind ht) 'eq)]
   [(intmap? ht)
    (intmap-eq? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eq? (impersonator-val ht))]
   [else (raise-argument-error 'hash-eq? "hash?" ht)]))

(define (hash-eqv? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (mutable-hash-kind ht) 'eqv)]
   [(intmap? ht)
    (intmap-eqv? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-eqv? (impersonator-val ht))]
   [else (raise-argument-error 'hash-eqv? "hash?" ht)]))

(define (hash-equal? ht)
  (cond
   [(mutable-hash? ht)
    (eq? (mutable-hash-kind ht) 'equal)]
   [(intmap? ht)
    (intmap-equal? ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-equal? (impersonator-val ht))]
   [else (raise-argument-error 'hash-equal? "hash?" ht)]))

(define (hash-weak? ht)
  (cond
   [(mutable-hash? ht)
    (and (mutable-hash-weak ht) #t)]
   [(intmap? ht) #f]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-weak? (impersonator-val ht))]
   [else (raise-argument-error 'hash-weak? "hash?" ht)]))

(define hash-ref
  (case-lambda
    [(ht k)
     (let ([v (hash-ref ht k none)])
       (if (eq? v none)
           (raise-arguments-error
            'hash-ref
            "no value found for key"
            "key" k)
           v))]
    [(ht k fail)
     (cond
      [(mutable-hash? ht)
       (lock-acquire (mutable-hash-lock ht))
       (let ([v (mutable-hash-ref ht k none)])
         (lock-release (mutable-hash-lock ht))
         (if (eq? v none)
             ($fail fail)
             v))]
      [(intmap? ht) (intmap-ref ht k fail)]
      [(and (impersonator? ht)
            (authentic-hash? (impersonator-val ht)))
       (let ([v (impersonate-hash-ref ht k)])
         (if (eq? v none)
             ($fail fail)
             v))]
      [else (raise-argument-error 'hash-ref "hash?" ht)])]))

(define/who hash-for-each
  (case-lambda
   [(ht proc) (hash-for-each ht proc #f)]
   [(ht proc try-order?)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [try-order?
      (for-each (lambda (p) (proc (car p) (cdr p)))
                (try-sort-keys (hash-map ht cons)))]
     [(intmap? ht) (intmap-for-each ht proc)]
     [else
      ;; mutable or impersonated:
      (let loop ([i (hash-iterate-first ht)])
        (when i
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (|#%app| proc key val))
          (loop (hash-iterate-next ht i))))])]))

(define/who hash-map
  (case-lambda
   [(ht proc) (hash-map ht proc #f)]
   [(ht proc try-order?)
    (check who hash? ht)
    (check who (procedure-arity-includes/c 2) proc)
    (cond
     [try-order?
      (map (lambda (p) (proc (car p) (cdr p)))
           (try-sort-keys (hash-map ht cons)))]
     [(intmap? ht) (intmap-map ht proc)]
     [else
      ;; mutable or impersonated:
      (let loop ([i (hash-iterate-first ht)])
        (if (not i)
            '()
            (cons
             (let-values ([(key val) (hash-iterate-key+value ht i)])
               (|#%app| proc key val))
             (loop (hash-iterate-next ht i)))))])]))

;; In sorted hash-table travesals, make some effort to sort the key.
;; This attempt is useful for making hash-table traversals more
;; deterministic, especially for marshaling operations.
(define (try-sort-keys ps)
  (cond
   [(#%andmap (lambda (p) (orderable? (car p))) ps)
    (#%list-sort (lambda (a b) (orderable<? (car a) (car b))) ps)]
   [else ps]))

(define (orderable-major v)
  (cond
   [(boolean? v)    0]
   [(char? v)       1]
   [(real? v)       2]
   [(symbol? v)     3]
   [(keyword? v)    4]
   [(string? v)     5]
   [(bytevector? v) 6]
   [(null? v)       7]
   [(void? v)       8]
   [(eof-object? v) 9]
   [else #f]))

(define (orderable? v) (orderable-major v))

(define (orderable<? a b)
  (let ([am (orderable-major a)]
        [bm (orderable-major b)])
    (cond
     [(or (not am) (not bm))
      #f]
     [(fx=? am bm)
      (cond
       [(boolean? a) (not a)]
       [(char? a) (char<? a b)]
       [(real? a) (< a b)]
       [(symbol? a)
        (cond
         [(symbol-interned? a)
          (and (symbol-interned? b)
               (symbol<? a b))]
         [(symbol-interned? b) #t]
         [(symbol-unreadable? a)
          (and (symbol-unreadable? b)
               (symbol<? a b))]
         [(symbol-unreadable? b) #t]
         [else (symbol<? a b)])]
       [(keyword? a) (keyword<? a b)]
       [(string? a) (string<? a b)]
       [(bytevector? a) (bytes<? a b)]
       [else #f])]
     [else (fx<? am bm)])))

(define (hash-count ht)
  (cond
   [(mutable-hash? ht) (mutable-hash-actual-count ht)]
   [(intmap? ht) (intmap-count ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (hash-count (impersonator-val ht))]
   [else (raise-argument-error 'hash-count "hash?" ht)]))

(define (hash-keys-subset? ht1 ht2)
  (cond
   [(and (intmap? ht1)
         (intmap? ht2)
         (or (and (intmap-eq? ht1)
                  (intmap-eq? ht2))
             (and (intmap-eqv? ht1)
                  (intmap-eqv? ht2))
             (and (intmap-equal? ht1)
                  (intmap-equal? ht2))))
    (intmap-keys-subset? ht1 ht2)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2))))
    (and (<= (hash-count ht1) (hash-count ht2))
         (let ([ok? #t])
           (hash-for-each
            ht1
            (lambda (k v)
              (when ok?
                (set! ok? (not (eq? none (hash-ref ht2 k none)))))))
           ok?))]
   [(not (hash? ht1))
    (raise-argument-error 'hash-keys-subset? "hash?" ht1)]
   [(not (hash? ht2))
    (raise-argument-error 'hash-keys-subset? "hash?" ht2)]
   [else
    (raise-arguments-error 'hash-keys-subset?
                           "given hash tables do not use the same key comparison"
                           "first table" ht1
                           "first table" ht2)]))

;; Use `eql?` for recursive comparisons
(define (hash=? ht1 ht2 eql?)
  (cond
   [(and (intmap? ht1)
         (intmap? ht2))
    (intmap=? ht1 ht2 eql?)]
   [(and (hash? ht1)
         (hash? ht2)
         (or (and (hash-eq? ht1)
                  (hash-eq? ht2))
             (and (hash-eqv? ht1)
                  (hash-eqv? ht2))
             (and (hash-equal? ht1)
                  (hash-equal? ht2)))
         (eq? (hash-weak? ht1) (hash-weak? ht2)))
    (and (= (hash-count ht1) (hash-count ht2))
         ;; This generic comparison supports impersonators
         (let loop ([i (hash-iterate-first ht1)])
           (cond
            [(not i) #t]
            [else
             (let-values ([(key val) (hash-iterate-key+value ht1 i)])
               (let ([val2 (hash-ref ht2 key none)])
                 (cond
                  [(eq? val2 none) #f]
                  [else (and (eql? val val2)
                             (loop (hash-iterate-next ht1 i)))])))])))]
   [else #f]))


;; Use `hash` for recursive hashing
(define (hash-hash-code ht hash)
  (cond
   [(intmap? ht) (intmap-hash-code ht hash)]
   [else
    ;; This generic hashing supports impersonators
    (let loop ([hc 0] [i (hash-iterate-first ht)])
      (cond
       [(not i) hc]
       [else
        (let* ([eq-key? (hash-eq? ht)]
               [eqv-key? (and (not eq?) (hash-eqv? ht))])
          (let-values ([(key val) (hash-iterate-key+value ht i)])
            (let ([hc (hash-code-combine-unordered hc
                                                   (cond
                                                    [eq-key? (eq-hash-code key)]
                                                    [eqv-key? (eqv-hash-code key)]
                                                    [else (hash key)]))])
              (loop (hash-code-combine-unordered hc (hash val))
                    (hash-iterate-next ht i)))))]))]))

(define/who (hash-iterate-first ht)
  (cond
   [(mutable-hash? ht)
    (lock-acquire (mutable-hash-lock ht))
    (let ([i (mutable-hash-iterate-next ht -1)])
      (lock-release (mutable-hash-lock ht))
      i)]
   [(intmap? ht)
    (intmap-iterate-first ht)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    ;; `hash-iterate-first` must not hash any keys:
    (hash-iterate-first (impersonator-val ht))]
   [else (raise-argument-error who "hash?" ht)]))

(define (check-i who i)
  (check who exact-nonnegative-integer? i))

(define/who (hash-iterate-next ht i)
  (cond
   [(mutable-hash? ht)
    (check-i 'hash-iterate-next i)
    (lock-acquire (mutable-hash-lock ht))
    (let ([i (mutable-hash-iterate-next ht i)])
      (lock-release (mutable-hash-lock ht))
      i)]
   [(intmap? ht)
    (check-i 'hash-iterate-next i)
    (intmap-iterate-next ht i)]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    ;; `hash-iterate-next` must not hash any keys:
    (hash-iterate-next (impersonator-val ht) i)]
   [else (raise-argument-error who "hash?" ht)]))

(define (do-hash-iterate-key+value who ht i
                                   intmap-iterate-key+value
                                   mutable-hash-iterate-key+value
                                   key? value? pair?
                                   bad-index-v)
  (cond
   [(intmap? ht)
    (check-i who i)
    (call-with-values (lambda () (intmap-iterate-key+value ht i none))
      (case-lambda
        [(v) (if (eq? v none)
                 (if (eq? bad-index-v none)
                     (raise-arguments-error who "no element at index"
                                            "index" i)
                     (bad-index-result key? value? pair? bad-index-v))
                 v)]
        [(k v) (values k v)]))]
   [(mutable-hash? ht)
    (check-i who i)
    (lock-acquire (mutable-hash-lock ht))
    (call-with-values (lambda () (mutable-hash-iterate-key+value ht i none))
      (case-lambda
       [(v)
        (lock-release (mutable-hash-lock ht))
        (if (eq? v none)
            (if (eq? bad-index-v none)
                (raise-arguments-error who "no element at index"
                                       "index" i)
                (bad-index-result key? value? pair? bad-index-v))
            v)]
       [(k v)
        (lock-release (mutable-hash-lock ht))
        (values k v)]))]
   [(and (impersonator? ht)
         (authentic-hash? (impersonator-val ht)))
    (impersonate-hash-iterate-key+value who ht i key? value? pair? bad-index-v)]
   [else (raise-argument-error who "hash?" ht)]))

(define hash-iterate-key
  (case-lambda
   [(ht i) (hash-iterate-key ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-key ht i
                               intmap-iterate-key
                               mutable-hash-iterate-key
                               #t #f #f
                               bad-index-v)]))

(define hash-iterate-value
  (case-lambda
   [(ht i) (hash-iterate-value ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-value ht i
                               intmap-iterate-value
                               mutable-hash-iterate-value
                               #f #t #f
                               bad-index-v)]))

(define hash-iterate-key+value
  (case-lambda
   [(ht i) (hash-iterate-key+value ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-key+value ht i
                               intmap-iterate-key+value
                               mutable-hash-iterate-key+value
                               #t #t #f
                               bad-index-v)]))

(define hash-iterate-pair
  (case-lambda
   [(ht i) (hash-iterate-pair ht i none)]
   [(ht i bad-index-v)
    (do-hash-iterate-key+value 'hash-iterate-pair ht i
                               intmap-iterate-pair
                               mutable-hash-iterate-pair
                               #t #t #t
                               bad-index-v)]))

(define (unsafe-immutable-hash-iterate-first ht)
  (if (impersonator? ht)
      (hash-iterate-first ht)
      (unsafe-intmap-iterate-first ht)))

(define (iterator-for-impersonator? i) (fixnum? i))

(define (unsafe-immutable-hash-iterate-next ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-next ht i)
      (unsafe-intmap-iterate-next ht i)))

(define (unsafe-immutable-hash-iterate-key ht i)
  (if (iterator-for-impersonator? i)
      (hash-iterate-key ht i)
      (unsafe-intmap-iterate-key ht i)))

(define unsafe-immutable-hash-iterate-value
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-value ht i none)]
   [(ht i bad-index-v)
    (if (iterator-for-impersonator? i)
        (hash-iterate-value ht i bad-index-v)
        (unsafe-intmap-iterate-value ht i))]))

(define unsafe-immutable-hash-iterate-key+value
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-key+value ht i none)]
   [(ht i bad-index-v)
    (if (iterator-for-impersonator? i)
        (hash-iterate-key+value ht i bad-index-v)
        (unsafe-intmap-iterate-key+value ht i))]))

(define unsafe-immutable-hash-iterate-pair
  (case-lambda
   [(ht i) (unsafe-immutable-hash-iterate-pair ht i none)]
   [(ht i bad-index-v) 
    (if (iterator-for-impersonator? i)
        (hash-iterate-pair ht i bad-index-v)
        (unsafe-intmap-iterate-pair ht i))]))

(define unsafe-mutable-hash-iterate-first hash-iterate-first)
(define unsafe-mutable-hash-iterate-next hash-iterate-next)
(define unsafe-mutable-hash-iterate-key hash-iterate-key)
(define unsafe-mutable-hash-iterate-value hash-iterate-value)
(define unsafe-mutable-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-mutable-hash-iterate-pair hash-iterate-pair)

(define unsafe-weak-hash-iterate-first hash-iterate-first)
(define unsafe-weak-hash-iterate-next hash-iterate-next)
(define unsafe-weak-hash-iterate-key hash-iterate-key)
(define unsafe-weak-hash-iterate-value hash-iterate-value)
(define unsafe-weak-hash-iterate-key+value hash-iterate-key+value)
(define unsafe-weak-hash-iterate-pair hash-iterate-pair)

;; ----------------------------------------

(define (set-hash-hash!)
  (record-type-equal-procedure (record-type-descriptor mutable-hash)
                               hash=?)
  (record-type-hash-procedure (record-type-descriptor mutable-hash)
                              hash-hash-code)

  (record-type-hash-procedure (record-type-descriptor hash-impersonator)
                              hash-hash-code)
  (record-type-hash-procedure (record-type-descriptor hash-chaperone)
                              hash-hash-code))

;; ----------------------------------------

;; `eq?` identity of a `hash-procs` instance matters for
;; `impersonator-of?` and `chaperone-of?`:
(define-record hash-procs (ref set remove key clear equal-key))

(define-record hash-impersonator impersonator (procs))
(define-record hash-chaperone chaperone (procs))

(define/who (impersonate-hash ht ref set remove key . args)
  (check who
         (lambda (p) (mutable-hash? (strip-impersonator p)))
         :contract "(and/c hash? (not/c immutable?))"
         ht)
  (do-impersonate-hash who ht ref set remove key args
                       make-hash-impersonator))

(define/who (chaperone-hash ht ref set remove key . args)
  (check who hash? ht)
  (do-impersonate-hash who ht ref set remove key args
                       make-hash-chaperone))

(define (do-impersonate-hash who ht ref set remove key args
                             make-hash-chaperone)
  (check who (procedure-arity-includes/c 2) ref)
  (check who (procedure-arity-includes/c 3) set)
  (check who (procedure-arity-includes/c 2) remove)
  (check who (procedure-arity-includes/c 2) key)
  (let* ([clear-given? (and (pair? args)
                            (or (not (car args))
                                (procedure? (car args))))]
         [clear (if clear-given?
                    (let ([clear (car args)])
                      (check who (procedure-arity-includes/c 1) :or-false clear)
                      clear)
                    void)]
         [args (if clear-given? (cdr args) args)]
         [equal-key-given? (and (pair? args)
                                (or (not (car args))
                                    (procedure? (car args))))]
         [equal-key (if equal-key-given?
                        (let ([equal-key (car args)])
                          (check who (procedure-arity-includes/c 2) :or-false equal-key)
                          equal-key)
                        (lambda (ht k) k))]
         [args (if equal-key-given? (cdr args) args)])
    (make-hash-chaperone (strip-impersonator ht)
                         ht
                         (add-impersonator-properties who
                                                      args
                                                      (if (impersonator? ht)
                                                          (impersonator-props ht)
                                                          empty-hasheq))
                         (make-hash-procs ref set remove key clear equal-key))))

;; ----------------------------------------

(define (impersonate-hash-ref ht k)
  (impersonate-hash-ref/set 'hash-ref #f
                            (lambda (ht k v) (hash-ref ht k none))
                            (lambda (procs ht k none-v)
                              ((hash-procs-ref procs) ht k))
                            hash-procs-ref
                            ht k none))

(define (impersonate-hash-set! ht k v)
  (impersonate-hash-ref/set 'hash-set! #t
                            hash-set!
                            (lambda (procs ht k v)
                              ((hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-set ht k v)
  (impersonate-hash-ref/set 'hash-set #t
                            hash-set
                            (lambda (procs ht k v)
                              ((hash-procs-set procs) ht k v))
                            hash-procs-set
                            ht k v))

(define (impersonate-hash-remove! ht k)
  (impersonate-hash-ref/set 'hash-remove! #t
                            (lambda (ht k false-v) (hash-remove! ht k))
                            (lambda (procs ht k false-v)
                              (values ((hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-remove ht k)
  (impersonate-hash-ref/set 'hash-remove #t
                            (lambda (ht k false-v) (hash-remove ht k))
                            (lambda (procs ht k false-v)
                              (values ((hash-procs-remove procs) ht k) #f))
                            hash-procs-remove
                            ht k #f))

(define (impersonate-hash-ref/set who set? authentic-op apply-wrapper get-wrapper ht k v)
  (let ([wrap-key? (hash-equal? ht)])
    (let loop ([ht ht] [get-k (and wrap-key? values)] [k k] [v v])
      (cond
       [(or (hash-impersonator? ht)
            (hash-chaperone? ht))
        (let ([chaperone? (hash-chaperone? ht)]
              [procs (if (hash-impersonator? ht)
                         (hash-impersonator-procs ht)
                         (hash-chaperone-procs ht))]
              [next-ht (impersonator-next ht)])
          (let ([get-k (and wrap-key? (extend-get-k who get-k procs next-ht chaperone?))])
            (call-with-values
                (lambda () (apply-wrapper procs next-ht k v))
              (case-lambda
               [(new-k new-v-or-wrap)
                ;; In `ref` mode, `new-v-or-wrap` is a wrapper procedure for the result.
                ;; In `set` mode, `new-v-or-wrap` is a replacement value.
                (when chaperone?
                  (unless (or (not chaperone?) (chaperone-of? new-k k))
                    (raise-chaperone-error who "key" new-k k))
                  (when set?
                    (unless (or (not chaperone?) (chaperone-of? new-v-or-wrap v))
                      (raise-chaperone-error who "value" new-v-or-wrap v))))
                ;; Recur...
                (let ([r (loop next-ht get-k new-k (if set? new-v-or-wrap none))])
                  ;; In `ref` mode, `r` is the result value.
                  ;; In `set` mode, `r` is void or an updated hash table.
                  (cond
                   [(and set? (void? r))
                    (void)]
                   [set?
                    ((if chaperone? make-hash-chaperone make-hash-impersonator)
                     (strip-impersonator r)
                     r
                     (impersonator-props ht)
                     procs)]
                   [(eq? r none) none]
                   [else
                    (let ([new-r (new-v-or-wrap next-ht new-k r)])
                      (when chaperone?
                        (unless (chaperone-of? new-r r)
                          (raise-chaperone-error who "value" new-r r)))
                      new-r)]))]
               [args
                (raise-arguments-error who
                                       (string-append (if chaperone? "chaperone" "impersonator")
                                                      " did not return 2 values")
                                       (string-append (if chaperone? "chaperone" "impersonator")
                                                      " procedure")
                                       (get-wrapper procs)
                                       "number of returned values" (length args))]))))]
       [(impersonator? ht)
        (let ([r (loop (impersonator-next ht) get-k k v)])
          (cond
           [(and set? (void? r))
            (void)]
           [set?
            (rewrap-props-impersonator ht r)]
           [else r]))]
       [else
        (if (and get-k (not (eq? get-k values)))
            (call-with-equality-wrap
             get-k
             k
             (lambda () (authentic-op ht k v)))
            (authentic-op ht k v))]))))

;; Add a layer of interposition on `equal?` and `equal-hash-code`:
(define (extend-get-k who get-k procs next-ht chaperone?)
  (lambda (k)
    (let* ([k (get-k k)]
           [new-k ((hash-procs-equal-key procs) next-ht k)])
      (unless (or (not chaperone?) (chaperone-of? new-k k))
        (raise-chaperone-error who "key" new-k k))
      new-k)))

(define (impersonate-hash-clear ht mutable?)
  (let loop ([ht ht])
    (cond
     [(or (hash-impersonator? ht)
          (hash-chaperone? ht))
      (let ([procs (if (hash-impersonator? ht)
                       (hash-impersonator-procs ht)
                       (hash-chaperone-procs ht))]
            [next-ht (impersonator-next ht)])
        (let ([clear (hash-procs-clear procs)])
          (cond
           [clear
            (clear next-ht)
            (if mutable?
                (loop next-ht)
                (let ([r (loop next-ht)])
                  (and r
                       ((if (chaperone? ht) make-hash-chaperone make-hash-impersonator)
                        (strip-impersonator r)
                        r
                        (impersonator-props ht)
                        procs))))]
           [else
            ;; Fall back to iterate of remove
            #f])))]
     [(impersonator? ht)
      (if mutable?
          (loop (impersonator-next ht))
          (let ([r (loop (impersonator-next ht))])
            (and r
                 (rewrap-props-impersonator ht r))))]
     [else
      (if mutable?
          (hash-clear! ht)
          (hash-clear ht))])))

(define (impersonate-hash-copy ht)
  (let* ([val-ht (impersonator-val ht)]
         [mutable? (mutable-hash? val-ht)]
         [new-ht
          (cond
           [mutable?
            (cond
             [(hash-weak? ht)
              (cond
               [(hash-eq? val-ht) (make-weak-hasheq)]
               [(hash-eqv? val-ht) (make-weak-hasheq)]
               [else (make-weak-hash)])]
             [else
              (cond
               [(hash-eq? val-ht) (make-hasheq)]
               [(hash-eqv? val-ht) (make-hasheq)]
               [else (make-hash)])])]
           [else
            (cond
             [(hash-eq? val-ht) (make-hasheq)]
             [(hash-eqv? val-ht) (make-hasheqv)]
             [else (make-hash)])])])
    (let loop ([i (hash-iterate-first ht)])
      (cond
       [i (let-values ([(key val) (hash-iterate-key+value ht i)])
            (hash-set! new-ht key val)
            (loop (hash-iterate-next ht i)))]
       [else new-ht]))))

(define (impersonate-hash-iterate-key+value who ht i key? value? pair? bad-index-v)
  (let ([key (impersonate-hash-iterate-key who ht i (if (eq? bad-index-v none) none none2))])
    (cond
     [(eq? key none2) (bad-index-result key? value? pair? bad-index-v)]
     [(not value?) key]
     [else
      (let ([val (hash-ref ht key none)])
        (cond
         [(eq? val none)
          (raise-arguments-error who
                                 (string-append "no value found for post-"
                                                (if (impersonator? ht) "impersonator" "chaperone")
                                                " key")
                                 "key" key)]
         [pair? (cons key val)]
         [key? (values key val)]
         [else val]))])))

(define (impersonate-hash-iterate-key who ht i bad-index-v)
  ;; We don't have to set up `get-k`, because `hash-iterate-key`
  ;; is prohibited from hashing any keys
  (let loop ([ht ht])
    (cond
     [(hash-impersonator? ht)
      (let ([procs (hash-impersonator-procs ht)]
            [ht (impersonator-next ht)])
        ((hash-procs-key procs) ht (loop ht)))]
     [(hash-chaperone? ht)
      (let ([procs (hash-chaperone-procs ht)]
            [ht (impersonator-next ht)])
        (let* ([k (loop ht)]
               [new-k ((hash-procs-key procs) ht k)])
          (unless (chaperone-of? new-k k)
            (raise-chaperone-error who "key" new-k k))
          new-k))]
     [(impersonator? ht)
      (loop (impersonator-next ht))]
     [else
      ;; The same as `hash-iterate-key`, but with the correct `who`:
      (do-hash-iterate-key+value who ht i
                                 intmap-iterate-key
                                 #t #f #f
                                 bad-index-v)])))

(define (bad-index-result key? value? pair? bad-index-v)
  (cond
   [pair? (cons bad-index-v bad-index-v)]
   [(and value? key?) (values bad-index-v bad-index-v)]
   [else bad-index-v]))
