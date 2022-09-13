(define-record-type/orig re:rtd
  (fields base-rtd name parent uid fields count sealed? opaque? extras)
  (nongenerative #{re:rtd bxw8uzjdge5u6o5xp0kovyiun-0}))

(define-record-type/orig re:record
  (fields rtd vec)
  (nongenerative #{re:record bxw8uzjdge5u6o5xp0kovyiun-1}))

(define-record-type/orig re:rcd
  (fields rtd parent-rcd protocol)
  (nongenerative #{re:rcd bxw8uzjdge5u6o5xp0kovyiun-3}))

(define-primitive $record? #%$record?)

(define-primitive record?
  (case-lambda
   [(v) (and (re:record? v)
             (not (re:rtd-opaque? (re:record-rtd v))))]
   [(v rtd) (and (re:record? v)
                 (let loop ([v-rtd (re:record-rtd v)])
                   (or (eq? v-rtd rtd)
                       (let ([p (re:rtd-parent v-rtd)])
                         (and p
                              (loop p))))))]))

(define-primitive ($sealed-record? v rtd)
  (and (re:record? v)
       (eq? rtd (re:record-rtd v))))

(define-primitive ($record-type-descriptor v)
  (cond
    [(eq? v #!base-rtd)
     #!base-rtd]
    [(re:rtd? v)
     (re:rtd-base-rtd v)]
    [else
     (re:record-rtd v)]))

(define-primitive record-rtd re:record-rtd)

(define-primitive record-type-uid re:rtd-uid)
(define-primitive record-type-name re:rtd-name)
(define-primitive record-type-sealed? re:rtd-sealed?)
(define-primitive record-type-opaque? re:rtd-opaque?)
(define-primitive record-type-parent re:rtd-parent)

(define (parent-rtd-count parent)
  (cond
    [(not parent) 0]
    [(eq? parent #!base-rtd) 'something]
    [else (re:rtd-count parent)]))

(define all-rtds (make-eq-hashtable))

(define-primitive ($make-record-type-descriptor base-rtd name parent uid sealed? opaque? fields . extras)
  (unless (or (not parent) (re:rtd? parent)) (error '$make-record-type-descriptor "bad parent ~s" parent))
  (let ([uid (or uid (gensym))]
        [fields (vector->list fields)])
    (or (hashtable-ref all-rtds uid #f)
        (let ([rtd (make-re:rtd base-rtd name parent uid
                                (map (lambda (f) (list (cadr f) (car f) 'scheme-object)) fields)
                                (+ (length fields) (parent-rtd-count parent))
                                sealed? opaque? extras)])
          (hashtable-set! all-rtds uid rtd)
          rtd))))

(define-primitive ($make-record-type base-rtd parent name fields sealed? opaque? . extras)
  (apply $make-record-type-descriptor base-rtd name parent (gensym) sealed? opaque? (list->vector fields) extras))

(define-primitive (make-record-type-descriptor name parent uid sealed? opaque? fields)
  ($make-record-type-descriptor #!base-rtd name parent uid sealed? opaque? fields))

(define-primitive ($make-record-type-descriptor* . args)
  (error '$make-record-type-descriptor* "not yet ready"))

(define-primitive record-type-descriptor? re:rtd?)

(define-primitive make-record-type
  (case-lambda
   [(name fields) (make-record-type #f name fields)]
   [(parent name fields)
    ($make-record-type #!base-rtd parent name
                       (map (lambda (f)
                              (if (symbol? f)
                                  (list f 'mutable 'scheme-object)
                                  (list (caddr f) (car f) (cadr f))))
                            fields)
                       (+ (length fields) (parent-rtd-count parent))
                       #f #f)]))

(define-primitive ($remake-rtd rtd compute-field-offsets)
  (error '$remake-rtd "not yet ready"))

(define-primitive ($record rtd . args)
  (if (eq? rtd #!base-rtd)
      (error 'base-rtd "fixme")
      (make-re:record rtd (list->vector args))))

(define-primitive (make-record-constructor-descriptor rtd parent-rcd protocol)
  (make-re:rcd rtd parent-rcd protocol))

(define-primitive ($make-record-constructor-descriptor rtd parent-rcd protocol who)
  (make-record-constructor-descriptor rtd parent-rcd protocol))

(define-primitive record-constructor-descriptor? re:rcd?)

(define-primitive (record-constructor rcd)
  (cond
    [(re:rtd? rcd)
     (lambda fields
       (make-re:record rcd (list->vector fields)))]
    [else
     (let loop ([rcd rcd]
                [rc (lambda fields
                      (make-re:record (re:rcd-rtd rcd) (list->vector fields)))])
       (let ([protocol (re:rcd-protocol rcd)])
         (if protocol
             (protocol
              (cond
                [(re:rcd-parent-rcd rcd)
                 => (lambda (p-rcd)
                      (loop p-rcd
                            (lambda parent-fields
                              (lambda child-fields
                                (apply rc (append parent-fields child-fields))))))]
                [else rc]))
             rc)))]))

(define-primitive r6rs:record-constructor record-constructor)

(define-primitive (record-predicate rtd)
  (lambda (v) (record? v rtd)))

(define-primitive (record-accessor rtd idx)
  (let ([idx (+ idx (parent-rtd-count (re:rtd-parent rtd)))])
    (lambda (v)
      (vector-ref (re:record-vec v) idx))))

(define-primitive (record-mutator rtd idx)
  (let ([idx (+ idx (parent-rtd-count (re:rtd-parent rtd)))])
    (lambda (v val)
      (vector-set! (re:record-vec v) idx val))))

(define-primitive (field-name->index rtd name)
  (+ (let loop ([fs (re:rtd-fields rtd)] [idx 0])
       (cond
         [(null? fs) (error 'csv7-record "field not found ~s" name)]
         [(eq? (caar fs) name) idx]
         [else (loop (cdr fs) (add1 idx))]))
     (parent-rtd-count (re:rtd-parent rtd))))

(define-primitive (csv7:record-field-accessor rtd name/idx)
  (let ([idx (if (symbol? name/idx)
                 (field-name->index rtd name/idx)
                 name/idx)])
    (lambda (v)
      (vector-ref (re:record-vec v) idx))))

(define-primitive (csv7:record-field-mutator rtd name/idx)
  (let ([idx (if (symbol? name/idx)
                 (field-name->index rtd name/idx)
                 name/idx)])
    (lambda (v val)
      (vector-set! (re:record-vec v) idx val))))

(define-primitive (csv7:record-field-mutable? rtd name/idx)
  (let ([idx (if (symbol? name/idx)
                 (field-name->index rtd name/idx)
                 name/idx)])
    (let loop ([rtd rtd])
      (let ([c (parent-rtd-count (re:rtd-parent rtd))])
        (if (< idx c)
            (loop (re:rtd-parent rtd))
            (eq? (cadr (list-ref (re:rtd-fields rtd) (- idx c))) 'immutable))))))

(define-primitive (csv7:record-field-accessible? rtd name/idx)
  #t)

(define-primitive (record-type-field-names rtd)
  (list->vector (map car (re:rtd-fields rtd))))

(define-primitive (record-type-field-indices rtd)
  (list->vector (iota (- (re:rtd-count rtd) (parent-rtd-count (re:rtd-parent rtd))))))

(define-primitive (csv7:record-type-field-names rtd)
  (let loop ([rtd rtd] [accum '()])
    (let ([accum (append (map car (re:rtd-fields rtd))
                         accum)]
          [p (re:rtd-parent rtd)])
      (if (not p)
          accum
          (loop p accum)))))

(define-primitive ($record-type-field-indices rtd)
  (iota (re:rtd-count rtd)))

(define-primitive (csv7:record-type-field-decls rtd)
  (let loop ([rtd rtd] [accum '()])
    (let ([accum (append (map (lambda (f)
                                (list (cadr f) (caddr f) (cadr f)))
                              (re:rtd-fields rtd))
                         accum)]
          [p (re:rtd-parent rtd)])
      (if (not p)
          accum
          (loop p accum)))))

(define-primitive (record-writer rtd proc) (void))

;; assumes that records has only pointer-sized fields
(define-primitive ($object-ref type v offset)
  (cond
    [(flonum? v)
     (error '$object-ref "flonum")
     #;
     (case type
     [(unsigned-64)
     (integer-bytes->integer (real->floating-point-bytes v 8) #f)]
     [(integer-64)
     (integer-bytes->integer (real->floating-point-bytes v 8) #t)]
     [(integer-32)
     (let ([bstr (real->floating-point-bytes v 8)])
     (case offset
     [(6) (integer-bytes->integer bstr #t (system-big-endian?) 0 4)]
     [(10) (integer-bytes->integer bstr #t (system-big-endian?) 4 8)]
     [else
     (error "unrecognized floating-point access" type offse)]))]
     [else (error "unrecognized floating-point access" type offset)])]
    [else
     (unless (or (eq? type 'scheme-object)
                 (eq? type 'ptr))
       (error '$object-ref "unrecognized type: ~e" type))
     (error '$object-ref "object")
     #;
     (let ([i (quotient (- offset (+ record-ptr-offset ptr-bytes)) ptr-bytes)])
     (cond
     [(struct-type? v)
     (cond
     [(< i (length base-rtd-fields))
     ((csv7:record-field-accessor/mutator base-rtd i #f) v)]
     [else
     (error '$object-ref "not yet supported for base-rtd subtypes")])]
     [(base-rtd? v)
     ((csv7:record-field-accessor/mutator base-rtd i #f) v)]
     [else (unsafe-struct-ref v i)]))]))

(meta define record-type-info list)
(meta define record-type-info-rtd car)
(meta define record-type-info-rcd cadr)

(define-syntax type-descriptor
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name) (record-type-info-rtd (r #'name))]))))

(define-syntax record-type-descriptor
  (lambda (x)
    (syntax-case x ()
      [(_ name) #'(type-descriptor name)])))

(define-syntax record-constructor-descriptor
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name)
         (record-type-info-rcd (r #'name))]))))

(define-syntax define-record
  (let ()
    (lambda (x)
      (error 'define-record "please don't use `define-record` to implement the expander"))))

(define-syntax define-record-type
  (lambda (x)
    (lambda (r)
      (syntax-case x ()
        [(_ name/s spec ...)
         (let ([build (lambda (name . pieces)
                        (datum->syntax name
                                       (string->symbol
                                        (apply string-append
                                               (map (lambda (piece)
                                                      (cond
                                                        [(identifier? piece) (symbol->string (#%syntax->datum piece))]
                                                        [(string? piece) piece]
                                                        [(symbol? piece) (symbol->string piece)]
                                                        [else (error 'build-name "oops")]))
                                                    pieces)))))])
           (let-values ([(name maker pred)
                         (syntax-case #'name/s ()
                           [(name maker pred) (values #'name #'maker #'pred)]
                           [name (values #'name (build #'name "make-" #'name) (build #'name #'name "?"))])])
             (define (find key default)
               (or (ormap (lambda (spec)
                            (syntax-case spec ()
                              [(spec-key . _)
                               (eq? key (#%syntax->datum #'spec-key))
                               spec]
                              [_ #f]))
                          #'(spec ...))
                   default))
             (let ([parent (syntax-case (find 'parent #'(_ #f)) ()
                             [(_ #f) #f]
                             [(_ id) #'id])]
                   [fields (syntax-case (find 'fields #'(fields)) ()
                             [(fields field ...) #'(field ...)])]
                   [uid (syntax-case (find 'nongenerative #'(_ #f)) ()
                          [(_) (datum->syntax name (gensym))]
                          [(_ #f) #f]
                          [(_ id) #'id])]
                   [sealed? (syntax-case (find 'sealed #'(_ #f)) ()
                              [(_ s?) #'s?])]
                   [opaque? (syntax-case (find 'opaque #'(_ #f)) ()
                              [(_ o?) #'o?])]
                   [protocol (syntax-case (find 'protocol #'(_ #f)) ()
                               [(_ p) #'p])]
                   [rtd (build name "RTD:" name)]
                   [rcd (build name "RCD:" name)])
               (let ([field-names (map (lambda (field)
                                         (syntax-case field ()
                                           [(class field-name . _) #'(class field-name)]
                                           [field-name
                                            (identifier? #'field-name)
                                            #'(immutable field-name)]))
                                       fields)])
                 (with-syntax ()
                   (define (show v) #;(pretty-print (syntax->datum v)) v)
                   (show
                    #`(begin
                        (define-syntax #,name
                          (make-compile-time-value (record-type-info (syntax #,rtd) (syntax #,rcd))))
                        (indirect-export #,name #,rtd #,rcd)
                        (define #,rtd
                          (make-record-type-descriptor '#,name
                                                       #,(if parent
                                                             (record-type-info-rtd (r parent))
                                                             #f)
                                                       '#,uid
                                                       '#,sealed?
                                                       '#,opaque?
                                                       '#,(list->vector field-names)))
                        (define #,rcd
                          (make-record-constructor-descriptor #,rtd
                                                              #,(if parent
                                                                    (record-type-info-rcd (r parent))
                                                                    #f)
                                                              #,protocol))
                        (define #,pred (lambda (v) (record? v #,rtd)))
                        (define #,maker (record-constructor #,rcd))
                        #,@(map (lambda (field idx)
                                  #`(define #,(syntax-case field ()
                                                [(_ _ acc . _) #'acc]
                                                [(_ field) (build name name "-" #'field)]
                                                [field
                                                 (identifier? #'field)
                                                 (build name name "-" #'field)])
                                      (record-accessor #,rtd #,idx)))
                                fields
                                (iota (length field-names)))
                        #,@(filter (lambda (v) v)
                                   (map (lambda (field idx)
                                          (syntax-case field (mutable)
                                            [(mutable . _)
                                             #`(define #,(syntax-case field ()
                                                           [(_ _ _ mut) #'mut]
                                                           [(_ field) (build #'field name "-" #'field "-set!")])
                                                 (record-mutator #,rtd #,idx))]
                                            [_ #f]))
                                        fields
                                        (iota (length field-names)))))))))))]))))

