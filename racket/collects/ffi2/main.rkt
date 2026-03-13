#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (rename-in '#%foreign
                    [ffi2-lib-ref ffi2-lib-ref*]
                    [ffi2-free ffi2-free*]
                    [ffi2-memcpy ffi2-memcpy*]
                    [ffi2-memmove ffi2-memmove*]
                    [ffi2-memset ffi2-memset*])
         (submod ffi/unsafe internal)
         racket/fixnum
         racket/symbol
         setup/dirs)

(provide (protect-out
          ffi2-lib
          ffi2-lib?
          ffi2-lib-ref
          define-ffi2-pointer-type
          ffi2-struct
          ffi2-procedure
          ffi2-ptr-ref
          ffi2-ptr-set!
          ffi2-malloc
          ffi2-free
          ffi2-sizeof
          ffi2-memcpy
          ffi2-memmove
          ffi2-memset))

(define (ffi2-lib name [version/s ""]
                  #:fail [fail #f]
                  #:get-lib-dirs [get-lib-dirs get-lib-search-dirs]
                  #:global? [global? (eq? (system-type 'so-mode) 'global)]
                  #:custodian [custodian #f])
  (get-ffi-lib* name version/s
                #:who 'ffi2-lib
                #:fail fail
                #:get-lib-dirs get-lib-dirs
                #:global? global?
                #:custodian custodian))

(define (ffi2-lib? v)
  (ffi-lib? v))

(define (ffi2-lib-ref lib name-in)
  (define who 'ffi2-lib-ref)
  (unless (ffi-lib? lib) (raise-argument-error who "ffi2-lib?" lib))
  (define name
    (cond
      [(bytes? name-in) name-in]
      [(string? name-in) (string->bytes/utf-8 name-in)]
      [(symbol? name-in) (string->bytes/utf-8 (symbol->immutable-string name-in))]
      [else (raise-argument-error who "(or/c bytes? string? symbol?)" name-in)]))
  (ffi2-lib-ref* lib name))

(define (int8? v) (and (fixnum? v) (fx<= -128 v 127)))
(define (uint8? v) (byte? v))
(define (int16? v) (and (fixnum? v) (fx<= #x-8000 v #x7FFF)))
(define (uint16? v) (and (fixnum? v) (fx<= 0 v #xFFFF)))
(define (int32? v) (and (exact-integer? v) (<= #x-80000000 v #x7FFFFFFF)))
(define (uint32? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFF)))
(define (int64? v) (and (exact-integer? v) (<= #x-8000000000000000 v #x7FFFFFFFFFFFFFFF)))
(define (uint64? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFFFFFFFFFF)))
(define (any? v) #t)
(define (string-or-false? s) (or (not s) (string? s)))
(define (long? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long)))
                      (int32? v)
                      (int64? v)))
(define (ulong? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long)))
                       (uint32? v)
                       (uint64? v)))
(define (size_t? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof size_t)))
                        (int32? v)
                        (int64? v)))

(begin-for-syntax
  (struct ffi2-type (name vm-type predicate-id racket->c-id c->racket-id
                          pointer-type-id
                          return-only? compound?))
  (define (make-ffi2-type name vm-type predicate-id
                          #:racket->c [racket->c-id #'values]
                          #:c->racket [c->racket-id #'values]
                          #:pointer-type [pointer-type-id #f]
                          #:return-only? [return-only? #f]
                          #:compound? [compound? #f])
    (ffi2-type name vm-type predicate-id racket->c-id c->racket-id
               pointer-type-id
               return-only? compound?))

  (define (lookup-type stx t-id #:for-return? [for-return? #f])
    (define v (syntax-local-value t-id (lambda () #f)))
    (unless (ffi2-type? v)
      (raise-syntax-error #f "not an ffi2 type" stx t-id))
    (unless (or for-return? (not (ffi2-type-return-only? v)))
      (raise-syntax-error #f "ffi2 type allowed only as a procedure return" stx t-id))
    v))

(define-syntax (define-ffi2-type stx)
  (syntax-parse stx
    [(_ name arg ...)
     #'(begin
         (define-syntax name (make-ffi2-type 'name arg ...))
         (provide name))]))

(define-ffi2-type void_t 'void #'void? #:return-only? #t)
(define-ffi2-type int8_t 'integer-8 #'int8?)
(define-ffi2-type uint8_t 'unsigned-8 #'uint8?)
(define-ffi2-type int16_t 'integer-8 #'int16?)
(define-ffi2-type uint16_t 'unsigned-8 #'uint16?)
(define-ffi2-type int32_t 'integer-32 #'int32?)
(define-ffi2-type uint32_t 'unsigned-32 #'uint32?)
(define-ffi2-type int64_t 'integer-64 #'int64?)
(define-ffi2-type uint64_t 'unsigned-64 #'uint64?)
(define-ffi2-type int_t 'int #'int32?)
(define-ffi2-type uint_t 'unsigned #'uint32?)
(define-ffi2-type long_t 'long #'long?)
(define-ffi2-type ulon_t 'unsigned-long #'ulong?)
(define-ffi2-type size_t 'size_t #'size_t?)
(define-ffi2-type wchar_t 'wchar #'char?)
(define-ffi2-type fixnum_t 'fixnum #'fixnum?)
(define-ffi2-type float_t 'float #'flonum?)
(define-ffi2-type double_t 'double #'flonum?)
(define-ffi2-type bool_t 'stdbool #'any?)
(define-ffi2-type intbool_t 'bool #'any?)
(define-ffi2-type void_t* 'pointer #'ffi2-ptr?)
(define-ffi2-type void_t+ 'pointer/gc #'ffi2-ptr?)
(define-ffi2-type racket_t 'scheme-object #'any?)
(define-ffi2-type string_t 'string #'string-or-false?)

(define-syntax (define-ffi2-pointer-type stx)
  (syntax-parse stx
    [(_ name:id)
     (syntax/loc stx (define-ffi2-pointer-type name name))]
    [(_ name:id tag:id)
     (with-syntax ([tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a?" (syntax-e #'name)))
                                            #'name)])
       #'(begin
           (define (tag-ptr? v) ((#%foreign-inline (ffi2-ptr?-maker tag)) v))
           (define-syntax name (make-ffi2-type 'name '(pointer tag) #'tag-ptr?))))]))

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #t t f) #'t]
    [(_ #f t f) #'f]))

(define-syntax (ffi2-struct stx)
  (syntax-parse stx
    [(_ name:id ([field-name:id field-type:id]
                 ...))
     (with-syntax ([name* (datum->syntax #'name
                                         (string->symbol (format "~a*" (syntax-e #'name)))
                                         #'name)]
                   [tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a*?" (syntax-e #'name)))
                                            #'name)]
                   [tag-ptr?-str (format "~a*?" (syntax-e #'name))]
                   [([field-vm-type field-c->racket field-racket->c field-ok? field-type-name field-compound? field-ptr-vm-type]
                     ...)
                    (map (lambda (field-type)
                           (define t (lookup-type stx field-type))
                           (list (ffi2-type-vm-type t)
                                 (ffi2-type-c->racket-id t)
                                 (ffi2-type-racket->c-id t)
                                 (ffi2-type-predicate-id t)
                                 (ffi2-type-name t)
                                 (ffi2-type-compound? t)
                                 (and (ffi2-type-pointer-type-id t)
                                      (ffi2-type-vm-type (lookup-type stx (ffi2-type-pointer-type-id t))))))
                         (attribute field-type))]
                   [(name-field ...) (map (lambda (field-name)
                                            (datum->syntax field-name
                                                           (string->symbol (format "~a-~a" (syntax-e #'name) (syntax-e field-name)))
                                                           field-name))
                                          (attribute field-name))]
                   [(set-name-field! ...) (map (lambda (field-name)
                                                 (datum->syntax field-name
                                                                (string->symbol (format "set-~a-~a!" (syntax-e #'name) (syntax-e field-name)))
                                                                field-name))
                                               (attribute field-name))])
                                            
       #'(begin
           (define (tag-ptr? v) ((#%foreign-inline (ffi2-ptr?-maker name*)) v))
           (define-syntax name* (make-ffi2-type 'name* '(pointer name*) #'tag-ptr?))
           (define-syntax name (make-ffi2-type 'name '(struct tag (field-name field-vm-type) ...) #'tag-ptr?
                                               #:pointer-type #'name*
                                               #:compound? #t))
           (define (name-field v)
             (unless (tag-ptr? v) (raise-argument-error 'name-field tag-ptr?-str v))
             (static-if field-compound?                        
                        ((#%foreign-inline (ffi2-ptr-cast-maker field-ptr-vm-type))
                         v
                         (#%foreign-inline (ffi2-offsetof (struct tag (field-name field-vm-type) ...) field-name)))
                        (field-c->racket
                         ((#%foreign-inline (ffi2-ptr-ref-maker field-vm-type))
                          v
                          (#%foreign-inline (ffi2-offsetof (struct tag (field-name field-vm-type) ...) field-name))))))
           ...
           (define (set-name-field! v val)
             (unless (tag-ptr? v) (raise-argument-error 'set-name-field! tag-ptr?-str v))
             (unless (field-ok? val) (bad-assign-value 'set-name-field! 'field-type-name val))
             (static-if field-compound?
                        (ffi2-memcpy* v 0 val 0 (#%foreign-inline (ffi2-sizeof field-vm-type)))
                        ((#%foreign-inline (ffi2-ptr-set!-maker field-vm-type))
                         v
                         (#%foreign-inline (ffi2-offsetof (struct tag (field-name field-vm-type) ...) field-name))
                         (field-racket->c val))))
           ...))]))

(define-syntax (ffi2-ptr-ref stx)
  (syntax-parse stx
    [(form-id ptr-expr type:id (~optional (~seq offset-expr (~optional (~and abs #:bytes)))))
     (define t (lookup-type stx #'type))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)])
         (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
         (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
         (#,(ffi2-type-c->racket-id t)
          ((#%foreign-inline (ffi2-ptr-ref-maker #,(ffi2-type-vm-type t)))
           ptr
           #,(if (attribute abs)
                 #'offset
                 #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t))))))))]))

(define-syntax (ffi2-ptr-set! stx)
  (syntax-parse stx
    [(form-id ptr-expr type:id (~optional (~seq offset-expr (~optional (~and abs #:bytes)))) val-expr)
     (define t (lookup-type stx #'type))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)]
             [val val-expr])
         (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
         (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
         (unless (#,(ffi2-type-predicate-id t) val) (bad-assign-value 'form-id '#,(ffi2-type-name t) val))
         ((#%foreign-inline (ffi2-ptr-set!-maker #,(ffi2-type-vm-type t)))
          ptr
          #,(if (attribute abs)
                #'offset
                #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)))))
          (#,(ffi2-type-racket->c-id t) val)))]))

(define-syntax (ffi2-malloc stx)
  (syntax-parse stx
    [(form-id type:id)
     (define t (lookup-type stx #'type))
     (define ptr-vm-type (if (ffi2-type-pointer-type-id t)
                             (ffi2-type-vm-type (lookup-type stx (ffi2-type-pointer-type-id t)))
                             #'pointer))
     #`(let ([n 1])
         ((#%foreign-inline (ffi2-malloc-maker #,(ffi2-type-vm-type t) #,ptr-vm-type)) n))]))

(define (ffi2-free v)
  (unless (ffi2-ptr? v) (raise-argument-error 'ffi2-free "ffi2-ptr?" v))
  (ffi2-free* v))

(define (ffi2-memcpy dest src len
                     #:dest-offset [dest-offset 0]
                     #:src-offset [src-offset 0])
  (define who 'ffi2-memcpy)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ffi2-ptr?" dest))
  (unless (ffi2-ptr? src) (raise-argument-error who "ffi2-ptr?" src))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (unless (exact-integer? src-offset) (raise-argument-error who "exact-integer?" src-offset))
  (ffi2-memcpy* dest dest-offset src src-offset len))

(define (ffi2-memmove dest src len
                      #:dest-offset [dest-offset 0]
                      #:src-offset [src-offset 0])
  (define who 'ffi2-memmove)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ffi2-ptr?" dest))
  (unless (ffi2-ptr? src) (raise-argument-error who "ffi2-ptr?" src))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (unless (exact-integer? src-offset) (raise-argument-error who "exact-integer?" src-offset))
  (ffi2-memmove* dest dest-offset src src-offset len))

(define (ffi2-memset dest byte len
                     #:dest-offset [dest-offset 0])
  (define who 'ffi2-memset)
  (unless (ffi2-ptr? dest) (raise-argument-error who "ffi2-ptr?" dest))
  (unless (byte? byte) (raise-argument-error who "byte?" byte))
  (unless (exact-integer? len) (raise-argument-error who "exact-integer?" len))
  (unless (exact-integer? dest-offset) (raise-argument-error who "exact-integer?" dest-offset))
  (ffi2-memset* dest dest-offset byte len))

(define-syntax (ffi2-sizeof stx)
  (syntax-parse stx
    [(form-id type:id)
     (define t (lookup-type stx #'type))
     #`(#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)))]))

(define-syntax (ffi2-procedure stx)
  (syntax-parse stx
    [(form-id ptr-expr
              (-> in-type:id ...
                  (~optional (~seq #:varargs var-in-type:id ...))
                  out-type))
     (define in-ts (map (lambda (type) (lookup-type stx type)) (append (attribute in-type)
                                                                       (or (attribute var-in-type)
                                                                           null))))
     (define out-t (lookup-type stx #'out-type #:for-return? #t))
     (with-syntax ([(in ...) (generate-temporaries in-ts)]
                   [(in-ok? ...) (map ffi2-type-predicate-id in-ts)]
                   [(in_t-name ...) (map ffi2-type-name in-ts)]
                   [(in-racket->c ...) (map ffi2-type-racket->c-id in-ts)]
                   [(conv ...) (append
                                (if (attribute var-in-type)
                                    (list (list '__varargs_after (length (attribute in-type))))
                                    null))])
       #`(let ([ptr ptr-expr])
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
           (let ([proc ((#%foreign-inline (ffi2-procedure-maker (conv ...)
                                                                #,(map ffi2-type-vm-type in-ts)
                                                                #,(ffi2-type-vm-type out-t)))
                        ptr)])
             (lambda (in ...)
               (unless (in-ok? in) (bad-argument 'in_t-name in))
               ...
               (#,(ffi2-type-c->racket-id out-t)
                (proc (in-racket->c in) ...))))))]))

(define (bad-assign-value who what val)
  (raise-arguments-error who "value does not match type"
                         "value" val
                         "ffi2 type" (unquoted-printing-string (format "~a" what))))

(define (bad-argument what val)
  (raise-arguments-error 'ffi2 "foreign-procedure argument does not match type"
                         "argument" val
                         "argument ffi2 type" (unquoted-printing-string (format "~a" what))))
