;; Chez Scheme has mutable hash tables, but
;;  - there's not weak `equal?`-based hash table
;;  - there's no iteration API
;; By implementing our own, we get those features,
;; and we also trigger secondary hashing functions
;; like the traditional Racket implementation.

(define-record mutable-hash (kind      ; 'eq, 'eqv, or 'equal
                             lock
                             weak      ; #f or weak hashtable (for weak hashes)
                             count
                             wasted
                             entries)) ; array of #f-or-pair; weak pair has just key for weak hashes

(define create-mutable-hash
  (case-lambda
   [(kind weak? lock)
    (make-mutable-hash kind lock (and weak? (make-weak-eq-hashtable))
                       0 0 (make-vector 8 #f))]
   [(kind weak?)
    (create-mutable-hash kind weak? (make-lock kind))]))

(define make-hash
  (case-lambda
   [() (create-mutable-hash 'equal #f)]
   [(alist) (fill-hash! 'make-hash (make-hash) alist)]))

(define make-hasheq
  (case-lambda
   [() (create-mutable-hash 'eq #f)]
   [(alist) (fill-hash! 'make-hasheq (make-hasheq) alist)]))

(define make-hasheqv
  (case-lambda
   [() (create-mutable-hash 'eqv #f)]
   [(alist) (fill-hash! 'make-hasheqv (make-hasheqv) alist)]))

(define make-weak-hash
  (case-lambda
   [() (create-mutable-hash 'equal #t)]
   [(alist) (fill-hash! 'make-hash (make-weak-hash) alist)]))

(define make-weak-hasheq
  (case-lambda
   [() (create-mutable-hash 'eq #t)]
   [(alist) (fill-hash! 'make-hasheq (make-weak-hasheq) alist)]))

(define make-weak-hasheqv
  (case-lambda
   [() (create-mutable-hash 'eqv #t)]
   [(alist) (fill-hash! 'make-weak-hasheqv (make-weak-hasheqv) alist)]))

(define/who (fill-hash! who ht alist)
  (check who :test (and (list? alist) (andmap pair? alist)) :contract "(listof pair?)" alist)
  (for-each (lambda (p)
              (hash-set! ht (car p) (cdr p)))
            alist)
  ht)

;; ----------------------------------------

(define-syntax-rule (secondary-hash-code et k primary-code)
  (cond
   [(or (eq? et 'eq)
        (eq? et 'eqv))
    (fxxor (fxsrl primary-code 1)
           (fxsrl primary-code 30))]
   [else
    (key-equal-secondary-hash-code k)]))

;; ----------------------------------------

;; Note: `default` cannot be failure thunk
(define mutable-hash-ref
  (case-lambda
   [(ht key default)
    (mutable-hash-ref ht key default #f)]
   [(ht key default code)
    (let ([entries (mutable-hash-entries ht)]
          [kind (mutable-hash-kind ht)])
      (let ([mask (fx- (#%vector-length entries) 1)]
            [code (or code (hash-code kind key))])
        (let loop ([i (fxand mask code)]
                   [hc2 #f])
          (let ([p (#%vector-ref entries i)])
            (cond
             [(not p) default]
             [(key=? kind key (car p))
              (let ([weak (mutable-hash-weak ht)])
                (if weak
                    (hashtable-ref weak (car p) default)
                    (cdr p)))]
             [else
              (let ([hc2 (or hc2
                             (fxior 1 (secondary-hash-code kind key code)))])
                (loop (fxand (fx+ i (fxand hc2 mask)) mask)
                      hc2))])))))]))
   
(define mutable-hash-set!
  (case-lambda
   [(ht key val)
    (mutable-hash-set! ht key val #f)]
   [(ht key val code)
    (let ([entries (mutable-hash-entries ht)]
          [kind (mutable-hash-kind ht)])
      (let ([mask (fx- (#%vector-length entries) 1)]
            [code (or code (hash-code kind key))])
        (let loop ([i (fxand mask code)]
                   [hc2 #f])
          (let ([p (#%vector-ref entries i)])
            (cond
             [(not p)
              (cond
               [(> (fx* 2 (fx+ (mutable-hash-count ht)
                               (mutable-hash-wasted ht)))
                   mask)
                (mutable-hash-resize! ht)
                (mutable-hash-set! ht key val)]
               [else
                (let ([weak (mutable-hash-weak ht)])
                  (cond
                   [(not weak)
                    (#%vector-set! entries i (cons key val))]
                   [else
                    (#%vector-set! entries i (weak-cons key #f))
                    (hashtable-set! weak key val)])
                  (set-mutable-hash-count! ht (fx+ (mutable-hash-count ht) 1)))])]
             [(key=? kind key (car p))
              (let ([weak (mutable-hash-weak ht)])
                (cond
                 [(not weak)
                  (set-cdr! p val)]
                 [else
                  (let ([old-key (car p)])
                    (cond
                     [(eq? old-key #!bwp)
                      ;; Disappeared at the last instant! Try again.
                      (mutable-hash-set! ht key val)]
                     [else
                      (hashtable-set! weak old-key val)]))]))]
             [else
              (let ([hc2 (or hc2
                             (fxior 1 (secondary-hash-code kind key code)))])
                (loop (fxand (fx+ i (fxand hc2 mask)) mask)
                      hc2))])))))]))

(define (mutable-hash-remove! ht key)
  (let ([entries (mutable-hash-entries ht)]
        [kind (mutable-hash-kind ht)])
    (let ([mask (fx- (#%vector-length entries) 1)]
          [code (hash-code kind key)])
      (let loop ([i (fxand mask code)]
                 [hc2 #f])
        (let ([p (#%vector-ref entries i)])
          (cond
           [(not p) (void)]
           [(key=? kind key (car p))
            (#%vector-set! entries i '(#!bwp . #!bwp))
            (let ([weak (mutable-hash-weak ht)])
              (when weak
                (hashtable-delete! weak (car p))))
            (set-mutable-hash-count! ht (fx- (mutable-hash-count ht) 1))
            (set-mutable-hash-wasted! ht (fx+ (mutable-hash-wasted ht) 1))]
           [else
            (let ([hc2 (or hc2
                           (fxior 1 (secondary-hash-code kind key code)))])
              (loop (fxand (fx+ i (fxand hc2 mask)) mask)
                    hc2))]))))))

(define (mutable-hash-resize! ht)
  (let ([size (let ([c (mutable-hash-actual-count ht)])
                (fxmax 8 (fxsll 1 (integer-length (fx* 2 c)))))]
        [entries (mutable-hash-entries ht)]
        [weak (mutable-hash-weak ht)])
    (set-mutable-hash-count! ht 0)
    (set-mutable-hash-wasted! ht 0)
    (set-mutable-hash-entries! ht (make-vector size #f))
    (when weak
      (set-mutable-hash-weak! ht #f))
    (let loop ([i (#%vector-length entries)])
      (let ([i (fx- i 1)])
        (let ([p (#%vector-ref entries i)])
          (when p
            (let ([key (car p)])
              (unless (eq? key #!bwp)
                (mutable-hash-set! ht key (cdr p))))))
        (unless (fx= i 0)
          (loop i))))
    (when weak
      (set-mutable-hash-weak! ht weak))))

(define (mutable-hash-actual-count ht)
  (cond
   [(mutable-hash-weak ht)
    => (lambda (weak)
         (hashtable-size weak))]
   [else
    (mutable-hash-count ht)]))

(define (weak-mutable-hash-ref-key ht key)
  (let ([entries (mutable-hash-entries ht)]
        [kind (mutable-hash-kind ht)])
    (let ([mask (fx- (#%vector-length entries) 1)]
          [code (hash-code kind key)])
      (let loop ([i (fxand mask code)]
                 [hc2 #f])
        (let ([p (#%vector-ref entries i)])
          (cond
           [(not p) #f]
           [(key=? kind key (car p))
            (car p)]
           [else
            (let ([hc2 (or hc2
                           (fxior 1 (secondary-hash-code kind key code)))])
              (loop (fxand (fx+ i (fxand hc2 mask)) mask)
                      hc2))]))))))
   
;; ----------------------------------------

(define (mutable-hash-clear! ht)
  (set-mutable-hash-count! ht 0)
  (set-mutable-hash-wasted! ht 0)
  (set-mutable-hash-entries! ht (make-vector 8 #f))
  (when (mutable-hash-weak ht)
    (set-mutable-hash-weak! ht (make-weak-eq-hashtable))))

(define (mutable-hash-copy ht)
  (let* ([entries (mutable-hash-entries ht)]
         [len (#%vector-length entries)]
         [new-entries (make-vector len #f)])
    (let loop ([i (#%vector-length entries)])
      (let ([i (fx- i 1)])
        (let* ([p (#%vector-ref entries i)])
          (when p
            (#%vector-set! new-entries i (if (not (eq? (car p) #!bwp))
                                             (cons (car p) (cdr p))
                                             p)))
          (unless (fx= i 0)
            (loop i)))))
    (let ([kind (mutable-hash-kind ht)]
          [weak (mutable-hash-weak ht)])
      (make-mutable-hash kind (make-lock kind)
                         (and weak (hashtable-copy weak))
                         (mutable-hash-count ht)
                         (mutable-hash-wasted ht)
                         new-entries))))

;; ----------------------------------------

(define (mutable-hash-iterate-next ht i)
  (let* ([entries (mutable-hash-entries ht)]
         [len (#%vector-length entries)])
    (cond
     [(>= i len) #f]
     [else
      (let loop ([i (fx+ i 1)])
        (cond
         [(fx= i len) #f]
         [else
          (let ([p (#%vector-ref entries i)])
            (cond
             [(and p (not (eq? #!bwp (car p))))
              i]
             [else (loop (fx+ i 1))]))]))])))

(define (mutable-hash-iterate-cell ht i not-found)
  (let* ([entries (mutable-hash-entries ht)]
         [len (#%vector-length entries)])
    (cond
     [(>= i len) not-found]
     [else
      (let ([p (#%vector-ref entries i)])
        (cond
         [(not p) not-found]
         [else p]))])))

(define (mutable-hash-iterate-key ht i not-found)
  (let ([p (mutable-hash-iterate-cell ht i not-found)])
    (if (pair? p)
        (let ([key (car p)])
          (if (eq? key #!bwp)
              not-found
              key))
        not-found)))
  
(define (mutable-hash-iterate-value ht i not-found)
  (let ([p (mutable-hash-iterate-cell ht i not-found)])
    (if (pair? p)
        (let ([key (car p)])
          (if (eq? key #!bwp)
              not-found
              (let ([weak (mutable-hash-weak ht)])
                (cond
                 [(not weak) (cdr p)]
                 [else (hashtable-ref weak key not-found)]))))
        not-found)))

(define (mutable-hash-iterate-key+value ht i not-found)
  (let ([p (mutable-hash-iterate-cell ht i not-found)])
    (if (pair? p)
        (let ([key (car p)])
          (if (eq? key #!bwp)
              not-found
              (let ([weak (mutable-hash-weak ht)])
                (cond
                 [(not weak) (values key (cdr p))]
                 [else (values key
                               (hashtable-ref weak key not-found))]))))
        not-found)))

(define (mutable-hash-iterate-pair ht i not-found)
  (let ([p (mutable-hash-iterate-cell ht i not-found)])
    (if (pair? p)
        (let ([key (car p)])
          (if (eq? key #!bwp)
              not-found
              (let ([weak (mutable-hash-weak ht)])
                (cond
                 [(not weak) (cons key (cdr p))]
                 [else (cons key
                             (hashtable-ref weak key not-found))]))))
        not-found)))

;; ----------------------------------------

(define (eq-hashtable->hash ht)
  (let ([new-ht (make-hasheq)])
    (hash-table-for-each ht
                         (lambda (k v) (hash-set! new-ht k v)))
    new-ht))

(define (hash->eq-hashtable ht)
  (let ([new-ht (make-eq-hashtable)])
    (hash-for-each ht
                   (lambda (k v) (hashtable-set! new-ht k v)))
    new-ht))
