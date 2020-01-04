;; HAMT

;; A HAMT "bnode" is implemented here a stencil vector. A stencil
;; vector has 26 or 58 slots, so we make each node in the HAMT
;; of size 8 (32-bit platforms) or 16 (64-bit platforms). That way,
;; we have use three bits per slot: child, key, and value:
;;
;;  * A child bit set means that the trie continues with a child node
;;
;;  * A key bit set meants that the trie ends with a specific key and
;;    value; the key and child bits are mutually exclusive
;;
;;  * A value if is set only if the corresponding key bit is set. If
;;    the key bit is not set, then the implicit value for the key is
;;    `#t`.
;;
;; We use one extra slot in the stencil vector ro record the tree size
;; and equality type (both packed into a single fixnum). That's first
;; in the stencil vector, so the stencil vector mask always has the
;; low bit set. After that "static field", the order is as above:
;; children, keys, then values.
;;
;; The high bit in the stencil vector's stencil is used to indicate a
;; "shell" indirection to support cyclic hash tables.
;;
;; A "cnode" collision node is an association list. It's never a root
;; node, but it can be a child at any non-root depth.

(define (intmap? x) (stencil-vector? x))

;; log of node size:
(define BNODE-BITS (if (fx> (stencil-vector-mask-width) 24)
                       4
                       3))
(define BNODE-MASK (fx1- (fxsll 1 BNODE-BITS)))

;; node size:
(define HAMT-WIDTH (fxsll 1 BNODE-BITS))
(define HAMT-GROUP-MASK (fx- (fxsll 1 HAMT-WIDTH) 1))

;; Static fields in stencil vector:
(define HAMT-STATIC-FIELD-COUNT 1)
;;  First field is a count with eqtype encoded as a single fixnum:
(define HAMT-COUNT+EQTYPE-INDEX 0)
(define HAMT-COUNT+EQTYPE-BIT (fxsll 1 HAMT-COUNT+EQTYPE-INDEX))

;; Equality types:
(define HAMT-EQTYPE-EQ 0)
(define HAMT-EQTYPE-EQV 1)
(define HAMT-EQTYPE-EQUAL 2)
(define HAMT-EQTYPE-MASK (fx- (fxsll 1 (integer-length HAMT-EQTYPE-EQUAL)) 1))

(define HAMT-COUNT-OFFSET (integer-length HAMT-EQTYPE-MASK))
(define ONE-COUNT-IN-COUNT+EQTYPE (fxsll 1 HAMT-COUNT-OFFSET))

(define (hamt-eqtype h)
  (fxand (stencil-vector-ref h HAMT-COUNT+EQTYPE-INDEX) HAMT-EQTYPE-MASK))
(define (hamt-count h)
  (fxsrl (stencil-vector-ref h HAMT-COUNT+EQTYPE-INDEX) HAMT-COUNT-OFFSET))
(define (hamt-count+eqtype h)
  (stencil-vector-ref h HAMT-COUNT+EQTYPE-INDEX))

;; packs count and eqtype into a fixnum:
(define (count+eqtype c t)
  (fxior (fxsll c HAMT-COUNT-OFFSET) t))

;; to dispatch on a bnode's equality type:
(define-syntax eqtype-case
  (syntax-rules (eq eqv else)
    [(_ h [(eq) a] [(eqv) b] [else c])
     (let ([eqt (hamt-eqtype h)])
       (cond
        [(fx= eqt HAMT-EQTYPE-EQ) a]
        [(fx= eqt HAMT-EQTYPE-EQV) b]
        [else c]))]))

;; Child, key, and value bits in the stencil-vector mask:
(define HAMT-CHILD-OFFSET HAMT-STATIC-FIELD-COUNT)
(define HAMT-KEY-OFFSET (fx+ HAMT-CHILD-OFFSET HAMT-WIDTH))
(define HAMT-VAL-OFFSET (fx+ HAMT-KEY-OFFSET HAMT-WIDTH))

(define HAMT-SHELL-OFFSET (fx+ HAMT-VAL-OFFSET HAMT-WIDTH))
(define HAMT-SHELL-BIT (fxsll 1 HAMT-SHELL-OFFSET))

(define HAMT-CHILD-MASK (fxsll HAMT-GROUP-MASK HAMT-CHILD-OFFSET))
(define HAMT-KEY-MASK (fxsll HAMT-GROUP-MASK HAMT-KEY-OFFSET))
(define HAMT-VAL-MASK (fxsll HAMT-GROUP-MASK HAMT-VAL-OFFSET))

(define (hamt-mask->child-count mask)
  (fxpopcount (fxand HAMT-CHILD-MASK mask)))
(define (hamt-mask->key-count mask)
  (fxpopcount (fxand HAMT-KEY-MASK mask)))
(define (hamt-mask->val-count mask)
  (fxpopcount (fxand HAMT-VAL-MASK mask)))

(define (bnode? x) (stencil-vector? x))

(define-record-type cnode
  [fields (immutable hash)
          (immutable content)] ; association list
  [nongenerative #{cnode pfwh0bwrq2nqlke97ikru0ds2-0}]
  [sealed #t])

(define (bnode-bit-pos hash shift)
  (bnode-mask hash shift))

(define (bnode-mask hash shift)
  (fxand (fxsrl hash shift) BNODE-MASK))

(define (bnode-maps-key? node bit)
  (fxbit-set? (stencil-vector-mask node) (fx+ bit HAMT-KEY-OFFSET)))

(define (bnode-maps-child? node bit)
  (fxbit-set? (stencil-vector-mask node) (fx+ bit HAMT-CHILD-OFFSET)))

(define (down shift)
  (fx+ shift BNODE-BITS))

(define (child-ref n bit)
  (stencil-vector-ref n (fxpopcount
                         (fxand (stencil-vector-mask n)
                                (fx- (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)) 1)))))

(define (key-ref n bit)
  (stencil-vector-ref n (fxpopcount
                         (fxand (stencil-vector-mask n)
                                (fx- (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)) 1)))))

(define (val-ref n bit)
  (let ([mask-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))]
        [mask (stencil-vector-mask n)])
    (if (fx= 0 (fxand mask-bit mask))
        #t ; not stored => #t
        (stencil-vector-ref n (fxpopcount
                               (fxand mask (fx- mask-bit 1)))))))

;; assumes no children
(define (only-key-ref n)
  (stencil-vector-ref n HAMT-STATIC-FIELD-COUNT))

;; assumes no children
(define (only-val-ref n)
  (if (fx= 0 (fxand (stencil-vector-mask n) HAMT-VAL-MASK))
      #t ; not stored => #t
      (stencil-vector-ref n (fx+ 1 HAMT-STATIC-FIELD-COUNT))))

;; i counts from 0 through children + keys
(define (child-index-ref n i)
  (stencil-vector-ref n (fx+ HAMT-CHILD-OFFSET i)))

;; i counts from 0 through children + keys, so it encodes the number of preceding children
(define (key-index-ref n i)
  (stencil-vector-ref n (fx+ HAMT-CHILD-OFFSET i)))

;; i counts from 0 through children + keys
(define (val-index-ref n i)
  (let* ([mask (stencil-vector-mask n)]
         [val-mask (fxand mask HAMT-VAL-MASK)])
    (cond
     [(fx= 0 val-mask)
      ;; All values in this node are implicitly #t
      #t]
     [(fx= (fxsrl val-mask (fx- HAMT-VAL-OFFSET HAMT-KEY-OFFSET))
           (fxand mask HAMT-KEY-MASK))
      ;; All keys supplied, so the value-relative index
      ;; matches the key-relative index
      (let* ([child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)]
             [val-i (fx- i child-count)]) ; same as key index
        (val-local-index-ref n child-count key-count val-i))]
     [else
      ;; Complicated case: we have to figure out how many
      ;; previous keys have values
      (let* ([child-count (hamt-mask->child-count mask)]
             [key-count (hamt-mask->key-count mask)]
             [key-i (fx- i child-count)])
        (let loop ([i 0] [val-i 0] [bit HAMT-KEY-OFFSET])
          (cond
           [(fxbit-set? mask bit)
            ;; Found a key
            (if (fxbit-set? mask (fx+ bit (fx- HAMT-VAL-OFFSET HAMT-KEY-OFFSET)))
                ;; Also found a value:
                (if (= i key-i)
                    (val-local-index-ref n child-count key-count val-i)
                    (loop (fx+ i 1) (fx+ val-i 1) (fx+ bit 1)))
                ;; Did not find a value
                (if (= i key-i)
                    #t ; implicit #t
                    (loop (fx+ i 1) val-i (fx+ bit 1))))]
           [else
            (loop i val-i (fx+ bit 1))])))])))

(define (val-local-index-ref n child-count key-count val-i)
  (stencil-vector-ref n (fx+ val-i
                             HAMT-STATIC-FIELD-COUNT
                             child-count
                             key-count)))

;; Should only be called three times to create the canonical empty
;; hashes:
(define (make-empty-bnode eqtype)
  (stencil-vector HAMT-COUNT+EQTYPE-BIT
                  (count+eqtype 0 eqtype)))

;; intmap interface

(define empty-hasheq (make-empty-bnode HAMT-EQTYPE-EQ))
(define empty-hasheqv (make-empty-bnode HAMT-EQTYPE-EQV))
(define empty-hash (make-empty-bnode HAMT-EQTYPE-EQUAL))

;; A "shell" intmap, used to create graphs, is a stencil vector with
;; the `HAMT-SHELL-BIT` entry as another (non-empty) intmap
(define (make-intmap-shell eqtype)
  (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                         HAMT-SHELL-BIT)
                  (count+eqtype 0 (case eqtype
                                    [(eq) HAMT-EQTYPE-EQ]
                                    [(eqv) HAMT-EQTYPE-EQV]
                                    [else HAMT-EQTYPE-EQUAL]))
                  #f))

(define (intmap-shell-sync! dest src)
  (stencil-vector-set! dest HAMT-STATIC-FIELD-COUNT src)
  (stencil-vector-set! dest HAMT-COUNT+EQTYPE-INDEX (stencil-vector-ref src HAMT-COUNT+EQTYPE-INDEX)))

(define (unwrap-shell h)
  (if (fxbit-set? (stencil-vector-mask h) HAMT-SHELL-OFFSET)
      (stencil-vector-ref h HAMT-STATIC-FIELD-COUNT)
      h))

(define (intmap-eq? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQ))

(define (intmap-eqv? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQV))

(define (intmap-equal? h)
  (eq? (hamt-eqtype h) HAMT-EQTYPE-EQUAL))

(define (intmap-empty? h)
  (fxzero? (hamt-count h)))

(define (intmap-has-key? h key)
  (let ([h (unwrap-shell h)])
    (bnode-has-key? h key (hash-code h key) 0)))

(define (intmap-ref h key default)
  (cond
   [(intmap-empty? h)
    ;; Access on an empty HAMT is common, so don't even hash in that case
    default]
   [else
    (let ([h (unwrap-shell h)])
      (bnode-ref h key (hash-code h key) 0 default))]))

(define (intmap-ref-key h key default)
  (cond
   [(intmap-empty? h)
    default]
   [else
    (let ([h (unwrap-shell h)])
      (bnode-ref-key h key (hash-code h key) 0 default))]))

(define (intmap-set h key val)
  (let ([h (unwrap-shell h)])
    (bnode-set h key val (hash-code h key) 0)))

(define (intmap-remove h key)
  (let ([h (unwrap-shell h)])
    (bnode-remove h key (hash-code h key) 0)))

(define (intmap-count h)
  (hamt-count h))

(define (node-count h)
  (if (bnode? h)
      (hamt-count h)
      (length (cnode-content h))))

(define (intmap=? a b eql?)
  (let ([a (unwrap-shell a)]
        [b (unwrap-shell b)])
    (and (eq? (hamt-count+eqtype a)
              (hamt-count+eqtype b))
         (bnode=? a b eql? 0))))

(define (intmap-hash-code a hash)
  (let ([a (unwrap-shell a)])
    (bnode-hash-code a hash 0)))

(define (intmap-keys-subset? a b)
  (let ([a (unwrap-shell a)]
        [b (unwrap-shell b)])
    (or (intmap-empty? a)
        (bnode-keys-subset? a b 0))))

;; generatic iteration by counting
(define (intmap-iterate-first h)
  (let ([h (unwrap-shell h)])
    (and (not (intmap-empty? h))
         0)))

(define (intmap-iterate-next h pos)
  (let ([h (unwrap-shell h)])
    (let ([pos (fx1+ pos)])
      (and (not (fx= pos (intmap-count h)))
           pos))))

(define (intmap-iterate-key h pos fail)
  (let ([h (unwrap-shell h)])
    (let ([p (bnode-entry-at-position h pos)])
      (if p
          (car p)
          fail))))

(define (intmap-iterate-value h pos fail)
  (let ([h (unwrap-shell h)])
    (let ([p (bnode-entry-at-position h pos)])
      (if p
          (cdr p)
          fail))))

(define (intmap-iterate-key+value h pos fail)
  (let ([h (unwrap-shell h)])
    (let ([p (bnode-entry-at-position h pos)])
      (if p
          (values (car p) (cdr p))
          fail))))

(define (intmap-iterate-pair h pos fail)
  (let ([h (unwrap-shell h)])
    (let ([p (bnode-entry-at-position h pos)])
      (or p fail))))

;; unsafe iteration; position is a stack
;; represented by a list of (cons node index)
(define (unsafe-intmap-iterate-first h)
  (let ([h (unwrap-shell h)])
    (and (not (intmap-empty? h))
         (unsafe-node-iterate-first h '()))))

(define (unsafe-node-iterate-first n stack)
  (cond
   [(bnode? n)
    (let ([mask (stencil-vector-mask n)])
      (let ([child-count (hamt-mask->child-count mask)]
            [key-count (hamt-mask->key-count mask)])
        (let ([stack (cons (cons n (fx+ key-count child-count -1)) stack)])
          (if (fx= key-count 0)
              (unsafe-node-iterate-first (child-index-ref n (fx- child-count 1)) stack)
              stack))))]
   [(cnode? n)
    (cons (box (cnode-content n))
          stack)]))

(define (unsafe-intmap-iterate-next h pos)
  ;; (unwrap-shell h) - not needed
  (unsafe-node-iterate-next pos))

(define (unsafe-node-iterate-next pos)
  (cond
   [(null? pos)
    ;; Stack is empty, so we're done
    #f]
   [else
    (let ([p (car pos)]
          [stack (cdr pos)])
      (cond
       [(box? p)
        ;; in a cnode
        (let ([new-p (cdr (unbox p))])
          (if (null? new-p)
              ;; Exhausted this node, so return to parent node
              (unsafe-node-iterate-next stack)
              ;; still in cnode:
              (cons (box new-p) stack)))]
       [else
        (let ([n (car p)]
              [i (cdr p)])
          (cond
           [(fx= 0 i)
            ;; Exhausted this node, so return to parent node
            (unsafe-node-iterate-next stack)]
           [else
            ;; Move to next (lower) index in the current node
            (let ([i (fx1- i)])
              (let ([child-count (hamt-mask->child-count (stencil-vector-mask n))]
                    [key-count (hamt-mask->key-count (stencil-vector-mask n))]
                    [stack (cons (cons n i) stack)])
                (if (fx< i child-count)
                    (unsafe-node-iterate-first (child-index-ref n i) stack)
                    stack)))]))]))]))

(define (unsafe-intmap-iterate-key h pos)
  ;; (unwrap-shell h) - not needed
  (let ([p (car pos)])
    (cond
     [(box? p)
      ;; in a cnode
      (caar (unbox p))]
     [else
      (key-index-ref (car p) (cdr p))])))

(define (unsafe-intmap-iterate-value h pos)
  ;; (unwrap-shell h) - not needed
  (let ([p (car pos)])
    (cond
     [(box? p)
      ;; in a cnode
      (cdar (unbox p))]
     [else
      (val-index-ref (car p) (cdr p))])))

(define (unsafe-intmap-iterate-key+value h pos)
  ;; (unwrap-shell h) - not needed
  (let ([p (car pos)])
    (cond
     [(box? p)
      ;; in a cnode
      (let ([pr (car (unbox p))])
        (values (car pr) (cdr pr)))]
     [else
      (let ([n (car p)]
            [i (cdr p)])
        (values (key-index-ref n i)
                (val-index-ref n i)))])))

(define (unsafe-intmap-iterate-pair h pos)
  ;; (unwrap-shell h) - not needed
  (let ([p (car pos)])
    (cond
     [(box? p)
      ;; in a cnode
      (car (unbox p))]
     [else
      (let ([n (car p)]
            [i (cdr p)])
        (cons (key-index-ref n i)
              (val-index-ref n i)))])))

;; hnode operations
(define (key=? n k1 k2)
  (eqtype-case
   n
   [(eq)  (eq? k1 k2)]
   [(eqv) (eqv? k1 k2)]
   [else  (key-equal? k1 k2)]))

(define (hash-code n k)
  (eqtype-case
   n
   [(eq)  (eq-hash-code k)]
   [(eqv) (eqv-hash-code k)]
   [else  (key-equal-hash-code k)]))

(define (bnode-hash-code n hash hc)
  (let* ([mask (stencil-vector-mask n)]
         [hc (hash-code-combine hc mask)]
         [child-count (hamt-mask->child-count mask)]
         [key-count (hamt-mask->key-count mask)]
         [val-count (hamt-mask->val-count mask)])
    (let loop ([i 0] [hc hc])
      (cond
       [(fx< i child-count)
        (loop (fx1+ i)
              (let ([c (child-index-ref n i)])
                (cond
                 [(bnode? c)
                  (bnode-hash-code c hash hc)]
                 [else
                  ;; Hash code needs to be order-independent, so
                  ;; collision nodes are a problem; simplify by just
                  ;; using the hash code and hope that collisions are
                  ;; rare.
                  (hash-code-combine hc (cnode-hash c))])))]
       [else
        (let loop ([i 0] [hc hc])
          (cond
           [(fx< i val-count)
            (loop (fx1+ i)
                  (hash-code-combine hc (hash (stencil-vector-ref n (fx+ i child-count key-count)))))]
           [else hc]))]))))

;; bnode operations
(define (bnode-ref node key keyhash shift default)
  (let ([bit (bnode-bit-pos keyhash shift)])
    (cond
     [(bnode-maps-key? node bit)
      (let* ([k (key-ref node bit)])
        (if (key=? node key k)
            (val-ref node bit)
            default))]

     [(bnode-maps-child? node bit)
      (let* ([c (child-ref node bit)])
        (cond
         [(bnode? c)
          (bnode-ref c key keyhash (down shift) default)]
         [else
          (cnode-ref c node key keyhash default)]))]

     [else
      default])))

(define (bnode-ref-key node key keyhash shift default)
  (let ([bit (bnode-bit-pos keyhash shift)])
    (cond
     [(bnode-maps-key? node bit)
      (let* ([k (key-ref node bit)])
        (if (key=? node key k)
            k
            default))]

     [(bnode-maps-child? node bit)
      (let* ([c (child-ref node bit)])
        (cond
         [(bnode? c)
          (bnode-ref-key c key keyhash (down shift) default)]
         [else
          (cnode-ref-key c node key keyhash default)]))]

     [else
      default])))

(define (bnode-has-key? n key keyhash shift)
  (not (eq? none2 (bnode-ref-key n key keyhash shift none2))))

(define (bnode-set node key val keyhash shift)
  (let ([bit (bnode-bit-pos keyhash shift)])
    (cond
     [(bnode-maps-key? node bit)
      (let* ([k (key-ref node bit)]
             [v (val-ref node bit)])
        (cond
         [(key=? node key k)
          (if (eq? val v)
              node
              (bnode-replace-val node bit val))]
         [else
          (let* ([h (hash-code node k)]
                 [eqtype (hamt-eqtype node)]
                 [child (node-merge eqtype k v h key val keyhash (down shift))])
            (bnode-remove-key-add-child node child bit))]))]

     [(bnode-maps-child? node bit)
      (let* ([child (child-ref node bit)]
             [new-child (cond
                         [(bnode? child)
                          (bnode-set child key val keyhash (down shift))]
                         [else
                          (cnode-set child node key val keyhash)])])
        (if (eq? new-child child)
            node
            (bnode-replace-child node child new-child bit)))]

     [else
      (bnode-add-key node key val bit)])))

(define (bnode-remove node key keyhash shift)
  (let ([bit (bnode-bit-pos keyhash shift)])

    (cond
     [(bnode-maps-key? node bit)
      (let* ([k (key-ref node bit)])
        (cond
         [(key=? node key k)
          (let ([mask (stencil-vector-mask node)])
            (cond
             [(and (fx= (fxand mask HAMT-KEY-MASK) (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                   (fxzero? (fxand mask HAMT-CHILD-MASK)))
              ;; return canonical empty value
              (eqtype-case
               node
               [(eq)  empty-hasheq]
               [(eqv) empty-hasheqv]
               [else  empty-hash])]
             [else
              (bnode-remove-key node bit)]))]
         [else
          node]))]

     [(bnode-maps-child? node bit)
      (let* ([child (child-ref node bit)]
             [new-child (cond
                         [(bnode? child)
                          (bnode-remove child key keyhash (down shift))]
                         [else
                          (cnode-remove child node key keyhash)])])
        (cond
         [(eq? new-child child) node]
         [(and (bnode? new-child)
               (fx= 1 (hamt-count new-child)))
          ;; Replace child with its sole key and value
          (bnode-remove-child-add-key node (only-key-ref new-child) (only-val-ref new-child) bit)]
         [(and (cnode? new-child)
               (null? (cdr (cnode-content new-child))))
          ;; Replace child with its sole key and value
          (let ([p (car (cnode-content new-child))])
            (bnode-remove-child-add-key node (car p) (cdr p) bit))]
         [else
          (bnode-replace-child node child new-child bit)]))]

     [else
      node])))

(define (bnode=? a b eql? shift)
  (or
   (eq? a b)
   (and
    (fx= (hamt-count a) (hamt-count b))
    (let ([a-mask (stencil-vector-mask a)]
          [b-mask (stencil-vector-mask b)]) 
      (and
       (fx= a-mask b-mask)
       (let ([child-count (hamt-mask->child-count a-mask)])
         (let loop ([i 0])
           (cond
            [(fx= i child-count)
             (let ([key-count (hamt-mask->key-count a-mask)])
               (let loop ([j 0])
                 (cond
                  [(fx= j key-count) #t]
                  [else
                   (let ([i (fx+ j child-count)])
                     (let ([ak (key-index-ref a i)]
                           [bk (key-index-ref b i)])
                       (and (key=? a ak bk)
                            (eql? (val-index-ref a i) (val-index-ref b i))
                            (loop (fx+ j 1)))))])))]
            [else
             (let ([an (child-index-ref a i)]
                   [bn (child-index-ref b i)])
               (and (or (eq? an bn)
                        (cond
                         [(bnode? an)
                          (and (bnode? b)
                               (bnode=? an bn eql? (down shift)))]
                         [else
                          (cnode=? an bn a eql?)]))
                    (loop (fx+ i 1))))]))))))))

(define (bnode-keys-subset? a b shift)
  (or
   (eq? a b)
   (cond
    [(cnode? b)
     ;; only possible if `anode` has just one key, since
     ;; it doesn't have any collisions
     (and (fx= (hamt-count a) 1)
          (let* ([k (only-key-ref a)]
                 [hashcode (hash-code a k)])
            (cnode-has-key? b a k hashcode)))]
    [(fx> (hamt-count a) (hamt-count b))
     #f]
    [else
     (let* ([a-mask (stencil-vector-mask a)]
            [akm (fxand (fxsrl a-mask HAMT-KEY-OFFSET) HAMT-GROUP-MASK)]
            [acm (fxand (fxsrl a-mask HAMT-CHILD-OFFSET) HAMT-GROUP-MASK)]
            [abm (fxior acm akm)]
            [b-mask (stencil-vector-mask b)]
            [bcm (fxand (fxsrl b-mask HAMT-CHILD-OFFSET) HAMT-GROUP-MASK)]
            [bkm (fxand (fxsrl b-mask HAMT-KEY-OFFSET) HAMT-GROUP-MASK)]
            [bbm (fxior bcm bkm)])
       (and
        (fx= abm (fxand abm bbm))
        (let loop ([abm abm] [bit 0] [aki (fxpopcount acm)] [bki (fxpopcount bcm)] [aci 0] [bci 0])
          (cond
           [(fxzero? abm) #t]
           [(fxbit-set? akm bit)
            (cond
             [(fxbit-set? bkm bit)
              (and
               (key=? a (key-index-ref a aki) (key-index-ref b bki))
               (loop (fxsrl abm 1) (fx1+ bit) (fx1+ aki) (fx1+ bki) aci bci))]
             [else
              (and
               (let ([akey (key-index-ref a aki)]
                     [bchild (child-index-ref b bci)])
                 (cond
                  [(bnode? bchild)
                   (bnode-has-key? bchild akey (hash-code a akey) (down shift))]
                  [else
                   (cnode-has-key? bchild b akey (hash-code a akey))]))
               (loop (fxsrl abm 1) (fx1+ bit) (fx1+ aki) bki aci (fx1+ bci)))])]
           [(fxbit-set? acm bit)
            (cond
             [(fxbit-set? bkm bit) #f]
             [else
              (and (let ([ac (child-index-ref a aci)]
                         [bc (child-index-ref b bci)])
                     (cond
                      [(bnode? ac)
                       (bnode-keys-subset? ac bc (down shift))]
                      [else
                       (cnode-keys-subset/equal? ac bc a #f)]))
                   (loop (fxsrl abm 1) (fx1+ bit) aki bki (fx1+ aci) (fx1+ bci)))])]
           [(fxbit-set? bkm bit)
            (loop (fxsrl abm 1) (fx1+ bit) aki (fx1+ bki) aci bci)]
           [(fxbit-set? bcm bit)
            (loop (fxsrl abm 1) (fx1+ bit) aki bki aci (fx1+ bci))]
           [else
            (loop (fxsrl abm 1) (fx1+ bit) aki bki aci bci)]))))])))

(define (bnode-add-key node key val bit)
  (if (eq? val #t)
      (stencil-vector-update node
                             HAMT-COUNT+EQTYPE-BIT
                             (fxior HAMT-COUNT+EQTYPE-BIT
                                    (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                             (fx+ (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                  ONE-COUNT-IN-COUNT+EQTYPE)
                             key)
      (stencil-vector-update node
                             HAMT-COUNT+EQTYPE-BIT
                             (fxior HAMT-COUNT+EQTYPE-BIT
                                    (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                    (fxsll 1 (fx+ bit HAMT-VAL-OFFSET)))
                             (fx+ (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                  ONE-COUNT-IN-COUNT+EQTYPE)
                             key
                             val)))

(define (bnode-remove-key node bit)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
    (stencil-vector-update node
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                  (fxand (stencil-vector-mask node) val-bit))
                           HAMT-COUNT+EQTYPE-BIT
                           (fx- (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                ONE-COUNT-IN-COUNT+EQTYPE))))

(define (bnode-replace-val node bit val)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
    (cond
     [(fx= 0 (fxand (stencil-vector-mask node) val-bit))
      ;; old value was #t
      (cond
       [(eq? val #t)
        node]
       [else
        (stencil-vector-update node 0 val-bit val)])]
     [else
      (cond
       [(eq? val #t)
        (stencil-vector-update node val-bit 0)]
       [else
        (stencil-vector-update node val-bit val-bit val)])])))

(define (bnode-remove-key-add-child node child bit)
  (let ([val-bit (fxsll 1 (fx+ bit HAMT-VAL-OFFSET))])
    (stencil-vector-update node
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                  (fxand (stencil-vector-mask node) val-bit))
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                           (fx+ (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                ONE-COUNT-IN-COUNT+EQTYPE)
                           child)))

(define (bnode-remove-child-add-key node key val bit)
  (cond
   [(eq? val #t)
    (stencil-vector-update node
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-KEY-OFFSET)))
                           (fx- (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                ONE-COUNT-IN-COUNT+EQTYPE)
                           key)]
   [else
    (stencil-vector-update node
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET)))
                           (fxior HAMT-COUNT+EQTYPE-BIT
                                  (fxsll 1 (fx+ bit HAMT-KEY-OFFSET))
                                  (fxsll 1 (fx+ bit HAMT-VAL-OFFSET)))
                           (fx- (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                ONE-COUNT-IN-COUNT+EQTYPE)
                           key
                           val)]))

(define (bnode-replace-child node old-child new-child bit)
  (let ([child-bit (fxsll 1 (fx+ bit HAMT-CHILD-OFFSET))]
        [delta (cond
                [(and (bnode? old-child)
                      (bnode? new-child))
                 (fx- (hamt-count+eqtype new-child)
                      (hamt-count+eqtype old-child))]
                [else
                 (fxsll (fx- (node-count new-child)
                             (node-count old-child))
                        HAMT-COUNT-OFFSET)])])
    (cond
     [(fx= 0 delta)
      (stencil-vector-update node child-bit child-bit new-child)]
     [else
      (let ([bits (fxior child-bit
                         HAMT-COUNT+EQTYPE-BIT)])
        (stencil-vector-update node
                               bits
                               bits
                               (fx+ (stencil-vector-ref node HAMT-COUNT+EQTYPE-INDEX)
                                    delta)
                               new-child))])))

(define HASHCODE-BITS (fxbit-count (most-positive-fixnum)))

(define (node-merge eqtype k1 v1 h1 k2 v2 h2 shift)
  (cond
   [(and (fx< HASHCODE-BITS shift)
         (fx= h1 h2))
    (pariah
     ;; hash collision: make a cnode
     (make-cnode h1
                 (list (cons k1 v1)
                       (cons k2 v2))))]

   [else
    (let ([m1 (bnode-mask h1 shift)]
          [m2 (bnode-mask h2 shift)])
      (cond
       [(fx= m1 m2)
        ;; partial collision: descend
        (let* ([child (node-merge eqtype k1 v1 h1 k2 v2 h2 (down shift))]
               [cm (bnode-bit-pos h1 shift)])
          (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                 (fxsll 1 (fx+ cm HAMT-CHILD-OFFSET)))
                          (count+eqtype 2 eqtype)
                          child))]

       [else
        ;; no collision, make a bnode
        (let ([bit1 (bnode-bit-pos h1 shift)]
              [bit2 (bnode-bit-pos h2 shift)])
          (let ([finish
                 (lambda (k1 v1 bit1 k2 v2 bit2)
                   (let ([key-bits (fxior (fxsll 1 (fx+ bit1 HAMT-KEY-OFFSET))
                                          (fxsll 1 (fx+ bit2 HAMT-KEY-OFFSET)))])
                     (cond
                      [(eq? v1 #t)
                       (cond
                        [(eq? v2 #t)
                         (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                key-bits)
                                         (count+eqtype 2 eqtype)
                                         k1 k2)]
                        [else
                         (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                key-bits
                                                (fxsll 1 (fx+ bit2 HAMT-VAL-OFFSET)))
                                         (count+eqtype 2 eqtype)
                                         k1 k2
                                         v2)])]
                      [else
                       (cond
                        [(eq? v2 #t)
                         (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                key-bits
                                                (fxsll 1 (fx+ bit1 HAMT-VAL-OFFSET)))
                                         (count+eqtype 2 eqtype)
                                         k1 k2
                                         v1)]
                        [else
                         (stencil-vector (fxior HAMT-COUNT+EQTYPE-BIT
                                                key-bits
                                                (fxsll 1 (fx+ bit1 HAMT-VAL-OFFSET))
                                                (fxsll 1 (fx+ bit2 HAMT-VAL-OFFSET)))
                                         (count+eqtype 2 eqtype)
                                         k1 k2
                                         v1 v2)])])))])
            (if (fx<= bit1 bit2)
                (finish k1 v1 bit1 k2 v2 bit2)
                (finish k2 v2 bit2 k1 v1 bit1))))]))]))

(define (bnode-entry-at-position n pos)
  (let* ([mask (stencil-vector-mask n)]
         [child-count (hamt-mask->child-count mask)]
         [key-count (hamt-mask->key-count mask)])
    (let loop ([i 0] [pos pos])
      (cond
       [(fx= i child-count)
        (cons (key-index-ref n (fx+ pos child-count))
              (val-index-ref n (fx+ pos child-count)))]
       [else
        (let ([c (child-index-ref n i)])
          (cond
           [(bnode? c)
            (let ([sz (hamt-count c)])
              (if (fx>= pos sz)
                  (loop (fx+ i 1) (fx- pos sz))
                  (bnode-entry-at-position c pos)))]
           [else
            (let* ([alist (cnode-content c)]
                   [len (length alist)])
              (if (fx>= pos len)
                  (loop (fx+ i 1) (fx- pos len))
                  (list-ref alist pos)))]))]))))

(define (intmap-for-each h proc)
  (let ([h (unwrap-shell h)])
    (bnode-fold h (lambda (k v _) (proc k v) (void)) (void))))

(define (intmap-map h proc)
  (let ([h (unwrap-shell h)])
    (#%reverse (bnode-fold h (lambda (k v xs) (cons (proc k v) xs)) '()))))

(define (bnode-fold n f nil)
  (let* ([mask (stencil-vector-mask n)]
         [child-count (hamt-mask->child-count mask)]
         [key-count (hamt-mask->key-count mask)])
    (let loop ([i 0] [nil nil])
      (cond
       [(fx= i child-count)
        (let loop ([i 0] [nil nil])
          (cond
           [(fx= i key-count)
            nil]
           [else
            (loop (fx+ i 1)
                  (f (key-index-ref n (fx+ i child-count))
                     (val-index-ref n (fx+ i child-count))
                     nil))]))]
       [else
        (let ([c (child-index-ref n i)])
          (cond
           [(bnode? c)
            (loop (fx+ i 1)
                  (bnode-fold c f nil))]
           [else
            (let aloop ([alist (cnode-content c)] [nil nil])
              (cond
               [(null? alist) (loop (fx+ i 1) nil)]
               [else
                (let ([rest-alist (cdr alist)])
                  (aloop rest-alist
                         (f (caar alist)
                            (cdar alist)
                            nil)))]))]))]))))

(define (cnode-ref node bnode key keyhash default)
  (cond
   [(fx= keyhash (cnode-hash node))
    (let ([p (cnode-assoc (cnode-content node) key bnode)])
      (if p
          (cdr p)
          default))]
   [else default]))

(define (cnode-ref-key node bnode key keyhash default)
  (cond
   [(fx= keyhash (cnode-hash node))
    (let ([p (cnode-assoc (cnode-content node) key bnode)])
      (if p
          (car p)
          default))]
   [else default]))

(define (cnode-has-key? n bnode key keyhash)
  (and (fx= keyhash (cnode-hash n))
       (cnode-assoc (cnode-content n) key bnode)
       #t))

(define (cnode-assoc alist key bnode)
  (cond
   [(null? alist) #f]
   [(key=? bnode key (caar alist))
    (car alist)]
   [else (cnode-assoc (cdr alist) key bnode)]))

(define (cnode-set node bnode key val keyhash)
  (make-cnode (cnode-hash node)
              (cnode-assoc-update (cnode-content node) key bnode (cons key val))))

(define (cnode-remove node bnode key keyhash)
  (make-cnode (cnode-hash node)
              (cnode-assoc-update (cnode-content node) key bnode #f)))

(define (cnode-assoc-update alist key bnode new-p)
  (cond
   [(null? alist) (if new-p
                      (list new-p)
                      null)]
   [(key=? bnode key (caar alist))
    (if new-p
        (cons new-p (cdr alist))
        (cdr alist))]
   [else
    (cons (car alist)
          (cnode-assoc-update (cdr alist) key bnode new-p))]))

(define (cnode=? a b anode eql?)
  (or
   (eq? a b)
   (and
    (cnode? b)
    (cnode-keys-subset/equal? a b anode eql?)
    (fx= (length (cnode-content a))
         (length (cnode-content b))))))

(define (cnode-keys-subset/equal? a b bnode eql?)
  (or
   (eq? a b)
   (and
    (cnode? b)
    (fx= (cnode-hash a) (cnode-hash b))
    (let ([ac (cnode-content a)]
          [bc (cnode-content b)])
      (let loop ([ac ac])
        (cond
         [(null? ac) #t]
         [else
          (let ([p (cnode-assoc bc (caar ac) bnode)])
            (and p
                 (or (not eql?) (eql? (cdar ac) (cdr p)))
                 (loop (cdr ac))))]))))))
