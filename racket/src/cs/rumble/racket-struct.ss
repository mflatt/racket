
;; Every Racket struct type should be a record subtype of `|#%racket-base-rtd|`

(define NUMBER-OF-BASE-RTD-FIELDS 9)
(define NUMBER-OF-RACKET-BASE-RTD-FIELDS (+ NUMBER-OF-BASE-RTD-FIELDS 4))

(define |#%racket-base-rtd|
  (make-record-type-descriptor
   'struct
   #!base-rtd
   #f ; parent
   #f ; sealed?
   #t ; opaque?
   '#((mutable procedure)
      (mutable arity)
      (mutable props)
      (mutable insp))))

(define (racket-rtd? rtd)
  (#%record? rtd |#%racket-base-rtd|))

(define racket-rtd-procedure (record-accessor |#%racket-base-rtd| 0))
(define racket-rtd-arity (record-accessor |#%racket-base-rtd| 1))
(define racket-rtd-props (record-accessor |#%racket-base-rtd| 2))
(define racket-rtd-insp (record-accessor |#%racket-base-rtd| 3))

(define set-racket-rtd-procedure! (record-mutator |#%racket-base-rtd| 0))
(define set-racket-rtd-arity! (record-mutator |#%racket-base-rtd| 1))
(define set-racket-rtd-props! (record-mutator |#%racket-base-rtd| 2))
(define set-racket-rtd-insp! (record-mutator |#%racket-base-rtd| 3))

(define-syntax (define-racket-record-type stx)
  (syntax-case stx (fields nongenerate sealed constructor predicate)
    [(_ name parent
        [fields (kind field-name) ...]
        [nongenerative . uid]
        [sealed sealed?]
        [constructor make-name]
        [predicate name?])
     (let ([mk (lambda args
                 (#%datum->syntax
                  (let loop ([args args])
                    (if (string? (car args))
                        (loop (cdr args))
                        (car args)))
                  (string->symbol
                   (#%apply #%string-append
                            (let loop ([args args])
                              (if (null? args)
                                  '()
                                  (cons
                                   (if (string? (car args))
                                       (car args)
                                       (#%symbol->string (#%syntax->datum (car args))))
                                   (loop (cdr args)))))))))])
       (with-syntax ([(name-field ...)
                      (#%map (lambda (fld) (mk #'name "-" fld)) #'(field-name ...))]
                     [(field-idx ...) (let loop ([fields #'(field-name ...)] [i 0])
                                        (if (null? fields)
                                            '()
                                            (cons i (loop (cdr fields) (add1 i)))))]
                     [(set-name-field! ...)
                      (#%filter
                       #%values
                       (#%map (lambda (fld kind)
                                (and (eq? kind 'mutable)
                                     (mk "set-" #'name "-" fld "!")))
                              #'(field-name ...)
                              (datum (kind ...))))]
                     [(set-field-idx ...) (let loop ([fields (datum (kind ...))] [i 0])
                                            (if (null? fields)
                                                '()
                                                (let ([r (loop (cdr fields) (add1 i))])
                                                  (if (eq? (car fields) 'mutable)
                                                      (cons i r)
                                                      r))))]
                     [rtd:name (mk "rtd:" #'name)]
                     [rcd:name (mk "rcd:" #'name)])
         #`(begin
             (define rtd:name
               (#%$make-record-type-descriptor
                |#%racket-base-rtd|
                'name
                #,(if (datum parent)
                      (mk "rtd:" #'parent)
                      #''#f)
                '#,(cond
                     [(null? (#%syntax->datum #'uid))
                      (#%datum->syntax #'name ((current-generate-id) (datum name)))]
                     [else (car (#%syntax->datum #'uid))])
                sealed?
                #f     ; opaque?
                '#((kind field-name)
                   ...)
                'define-racket-record-type
                #f    ; procedure
                #f    ; arity
                '()   ; props
                #f))  ; insp
             (define rcd:name (make-record-constructor-descriptor rtd:name
                                                                  #,(if (datum parent)
                                                                        (mk "rcd:" #'parent)
                                                                        #''#f)
                                                                  #f))
             (define #,(if (datum make-name)
                           #'make-name
                           (mk "make-" #'name))
               (record-constructor rcd:name))
             (define #,(if (datum name?) #'name? (mk #'name "?")) (record-predicate rtd:name))
             (define name-field (record-accessor rtd:name field-idx))
             ...
             (define set-name-field! (record-mutator rtd:name set-field-idx))
             ...)))]
    [(_ name
        [fields . flds]
        . more)
     #'(define-racket-record-type name #f
         [fields . flds]
         . more)]
    [(_ name parent
        [fields . flds])
     #'(define-racket-record-type name parent
         [fields . flds]
         [nongenerative]
         [sealed #f]
         [constructor #f]
         [predicate #f])]
    [(_ name parent
        [fields . flds]
        [sealed sealed?]
        . more)
     #'(define-racket-record-type name parent
         [fields . flds]
         [nongenerative]
         [sealed sealed?]
         . more)]
    [(_ name parent
        [fields . flds]
        [nongenerative . uid]
        [sealed sealed?])
     #'(define-racket-record-type name parent
         [fields . flds]
         [nongenerative . uid]
         [sealed sealed?]
         [constructor #f]
         [predicate #f])]))
    
