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
          define-ffi2-procedure
          ffi2-ptr-ref
          ffi2-ptr-set!
          ffi2-malloc
          ffi2-free
          ffi2-sizeof
          ffi2-memcpy
          ffi2-memmove
          ffi2-memset
          ->)
         ffi2-ptr?
         ffi2-ptr/gcable?)

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
  (struct ffi2-type (name vm-type predicate racket->c c->racket))
  (struct ffi2-type/proc ffi2-type (proc)
    #:property prop:procedure 0)
  (define (make-ffi2-type name vm-type predicate
                          #:procedure [proc #f]
                          #:racket->c [racket->c #'values]
                          #:c->racket [c->racket #'values])
    (if proc
        (ffi2-type/proc name vm-type predicate racket->c c->racket proc)
        (ffi2-type name vm-type predicate racket->c c->racket)))

  (define (ffi2-type-compound? t)
    (define vm-type (ffi2-type-vm-type t))
    (and (pair? vm-type)
         (memq (car vm-type) '(struct union))
         #t))

  (define (ffi2-type-pointer-vm-type t #:gcable? [gcable? #f])
    (define vm-type (ffi2-type-vm-type t))
    (and (pair? vm-type)
         (list (if gcable? 'pointer/gc 'pointer)
               (cadr vm-type))))

  (define (lookup-type stx t-id #:for-return? [for-return? #f])
    (define v (syntax-local-value t-id (lambda () #f)))
    (unless (ffi2-type? v)
      (raise-syntax-error #f "not an ffi2 type" stx t-id))
    (unless (or for-return? (ffi2-type-racket->c v))
      (raise-syntax-error #f "ffi2 type allowed only as a procedure return" stx t-id))
    v)

  (define-syntax-class :malloc-kind
    (pattern (~or #:manual #:gcable #:gcable-traced #:gcable-immobile #:gcable-traced-immobile))))

(define-syntax (define-ffi2-type stx)
  (syntax-parse stx
    [(_ name arg ...)
     #'(begin
         (define-syntax name (make-ffi2-type 'name arg ...))
         (provide name))]))

(define-ffi2-type void_t 'void #'void? #:racket->c #f)
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
(define-ffi2-type void_t*/gcable 'pointer/gc #'ffi2-ptr?)
(define-ffi2-type racket_t 'scheme-object #'any?)
(define-ffi2-type string_t 'string #'string-or-false?)

(define-syntax (-> stx)
  (raise-syntax-error #f "allowed only in an ffi2-type context" stx))

(begin-for-syntax
  (define-syntax-class :maybe-type
    #:description "an ffi2 type"
    #:literals (->)
    (pattern _:id)
    (pattern (-> _ ...)))

  (define-syntax-class (:type stx [for-return? #f])
    #:description "an ffi2 type"
    #:attributes (t)
    #:literals (->)
    (pattern type-name:id
             #:attr t (lookup-type stx #'type-name #:for-return? for-return?))
    (pattern (~and arrow-type
                   (-> in-maybe-type::maybe-type ...
                       (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                       out-maybe-type::maybe-type))
             #:cut
             #:with ((~var in-type (:type stx)) ...) #'(in-maybe-type ...)
             #:with ((~var var-in-type (:type stx)) ...) (if (attribute var-in-maybe-type)
                                                             #'(var-in-maybe-type ...)
                                                             #'())
             #:with (~var out-type (:type stx #t)) #'out-maybe-type
             #:attr t (make-ffi2-type '-> 'pointer procedure?
                                      #:racket->c #`(lambda (proc)
                                                      (ffi2-callback proc arrow-type))
                                      #:c->racket #`(lambda (ptr)
                                                      (ffi2-procedure ptr arrow-type))))))

(define-syntax (define-ffi2-pointer-type stx)
  (syntax-parse stx
    [(_ name:id)
     (syntax/loc stx (define-ffi2-pointer-type name name))]
    [(_ name:id tag:id)
     (with-syntax ([tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a?" (syntax-e #'name)))
                                            #'name)]
                   [name/gcable (datum->syntax #'name
                                               (string->symbol (format "~a/gcable" (syntax-e #'name)))
                                               #'name)])
       #'(begin
           (define (tag-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag)) v)
                                    ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag)) v)))
           (define-syntax name (make-ffi2-type 'name '(pointer tag) #'tag-ptr?))
           (define-syntax name/gcable (make-ffi2-type 'name/gcable '(pointer/gc tag) #'tag-ptr?))))]))

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #t t f) #'t]
    [(_ #f t f) #'f]))

(define-syntax (ffi2-struct stx)
  (syntax-parse stx
    [(_ name:id ([field-name:id (~var field-type (:type stx))]
                 ...))
     (with-syntax ([name* (datum->syntax #'name
                                         (string->symbol (format "~a*" (syntax-e #'name)))
                                         #'name)]
                   [name*/gcable (datum->syntax #'name
                                                (string->symbol (format "~a*/gcable" (syntax-e #'name)))
                                                #'name)]
                   [fill-name (car (generate-temporaries (list (format "fill-~a" (syntax-e #'name)))))]
                   [tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a*?" (syntax-e #'name)))
                                            #'name)]
                   [tag-ptr?-str (format "~a*?" (syntax-e #'name))]
                   [([field-vm-type field-c->racket field-racket->c field-ok? field-type-name
                                    field-compound? field-ptr-vm-type field-ptr/gcable-vm-type]
                     ...)
                    (map (lambda (t)
                           (list (ffi2-type-vm-type t)
                                 (ffi2-type-c->racket t)
                                 (ffi2-type-racket->c t)
                                 (ffi2-type-predicate t)
                                 (ffi2-type-name t)
                                 (ffi2-type-compound? t)
                                 (ffi2-type-pointer-vm-type t)
                                 (ffi2-type-pointer-vm-type t #:gcable? #t)))
                         (attribute field-type.t))]
                   [(name-field ...) (map (lambda (field-name)
                                            (datum->syntax field-name
                                                           (string->symbol (format "~a-~a" (syntax-e #'name) (syntax-e field-name)))
                                                           field-name))
                                          (attribute field-name))]
                   [(set-name-field! ...) (map (lambda (field-name)
                                                 (datum->syntax field-name
                                                                (string->symbol (format "set-~a-~a!" (syntax-e #'name) (syntax-e field-name)))
                                                                field-name))
                                               (attribute field-name))]
                   [(set-name-field!/unchecked ...) (generate-temporaries #'(field-name ...))])
       #'(begin
           (define (tag-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer name*)) v)
                                    ((#%foreign-inline (ffi2-ptr?-maker pointer/gc name*)) v)))
           (define-syntax name* (make-ffi2-type 'name* '(pointer name*) #'tag-ptr?))
           (define-syntax name*/gcable (make-ffi2-type 'name*/gcable '(pointer/gc name*) #'tag-ptr?))
           (define-syntax name
             (make-ffi2-type 'name '(struct name* (field-name field-vm-type) ...) #'tag-ptr?
                             #:procedure
                             (lambda (stx)
                               (syntax-parse stx
                                 [(_ (~optional kind::malloc-kind) field-name ...)
                                  (with-syntax ([kind (or (attribute kind) #'#:manual)])
                                    #'(fill-name (ffi2-malloc kind name)
                                                 field-name ...))]))))
           (define (name-field v)
             (unless (tag-ptr? v) (raise-argument-error 'name-field tag-ptr?-str v))
             (static-if field-compound?
                        ((#%foreign-inline (ffi2-ptr-cast-maker field-ptr-vm-type field-ptr/gcable-vm-type))
                         v
                         (#%foreign-inline (ffi2-offsetof (struct tag (field-name field-vm-type) ...) field-name)))
                        (field-c->racket
                         ((#%foreign-inline (ffi2-ptr-ref-maker field-vm-type))
                          v
                          (#%foreign-inline (ffi2-offsetof (struct tag (field-name field-vm-type) ...) field-name))))))
           ...
           (define (set-name-field!/unchecked v val)
             (static-if field-compound?
                        (ffi2-memcpy* v 0 val 0 (#%foreign-inline (ffi2-sizeof field-vm-type)))
                        ((#%foreign-inline (ffi2-ptr-set!-maker field-vm-type))
                         v
                         (#%foreign-inline (ffi2-offsetof (struct name* (field-name field-vm-type) ...) field-name))
                         (field-racket->c val))))
           ...
           (define (set-name-field! v val)
             (unless (tag-ptr? v) (raise-argument-error 'set-name-field! tag-ptr?-str v))
             (unless (field-ok? val) (bad-assign-value 'set-name-field! 'field-type-name val))
             (set-name-field!/unchecked v val))
           ...
           (define (fill-name p field-name ...)
             (unless (field-ok? field-name) (bad-assign-value 'name 'field-type-name field-name))
             ...
             (set-name-field!/unchecked p field-name)
             ...
             p)))]))

(define-syntax (ffi2-ptr-ref stx)
  (syntax-parse stx
    [(form-id ptr-expr (~var type (:type stx)) (~optional (~seq offset-expr (~optional (~and abs #:bytes)))))
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)])
         (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
         (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
         (#,(ffi2-type-c->racket t)
          ((#%foreign-inline (ffi2-ptr-ref-maker #,(ffi2-type-vm-type t)))
           ptr
           #,(if (attribute abs)
                 #'offset
                 #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t))))))))]))

(define-syntax (ffi2-ptr-set! stx)
  (syntax-parse stx
    [(form-id ptr-expr (~var type (:type stx))
              (~optional (~seq offset-expr (~optional (~and abs #:bytes))))
              val-expr)
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)]
             [val val-expr])
         (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
         (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
         (unless (#,(ffi2-type-predicate t) val) (bad-assign-value 'form-id '#,(ffi2-type-name t) val))
         ((#%foreign-inline (ffi2-ptr-set!-maker #,(ffi2-type-vm-type t)))
          ptr
          #,(if (attribute abs)
                #'offset
                #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)))))
          (#,(ffi2-type-racket->c t) val)))]))

(define-syntax (ffi2-malloc stx)
  (syntax-parse stx
    [(form-id (~optional kind::malloc-kind)
              (~or #:bytes (~var type (:type stx)))
              (~optional n-expr))
     (define kind-sym (string->symbol (keyword->string (syntax-e #'(~? kind #:manual)))))
     (define t (or (attribute type.t) (lookup-type stx #'int8_t)))
     (define ptr-vm-type (cond
                           [(ffi2-type-pointer-vm-type t #:gcable? (not (eq? kind-sym 'manual)))
                            => (lambda (vm-type) vm-type)]
                           [(eq? kind-sym 'manual) #'pointer]
                           [else #'pointer/gc]))
     #`(let ([n (~? n-expr 1)])
         (unless (exact-nonnegative-integer? n) (raise-argument-error 'form-id "exact-nonnegative-integer?" n))
         ((#%foreign-inline (ffi2-malloc-maker #,(ffi2-type-vm-type t) #,ptr-vm-type #,kind-sym)) n))]))

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
    [(form-id (~var type (:type stx)))     
     (define t (attribute type.t))
     #`(#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)))]))

(define-syntax (ffi2-procedure stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id ptr-expr
              (-> in-maybe-type::maybe-type ...
                  (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                  out-maybe-type::maybe-type))
     #:cut
     #:with ((~var in-type (:type stx)) ...) #'(in-maybe-type ...)
     #:with ((~var var-in-type (:type stx)) ...) (if (attribute var-in-maybe-type)
                                                     #'(var-in-maybe-type ...)
                                                     #'())
     #:with (~var out-type (:type stx #t)) #'out-maybe-type
     (define in-ts (append (attribute in-type.t) (attribute var-in-type.t)))
     (define out-t (attribute out-type.t))
     (with-syntax ([(in ...) (generate-temporaries in-ts)]
                   [(in-ok? ...) (map ffi2-type-predicate in-ts)]
                   [(in_t-name ...) (map ffi2-type-name in-ts)]
                   [(in-racket->c ...) (map ffi2-type-racket->c in-ts)]
                   [(conv ...) (append
                                (if (attribute var-in-maybe-type)
                                    (list (list '__varargs_after (length (attribute in-type))))
                                    null))])
       (with-syntax ([adjust-proc (cond
                                    [(ffi2-type-compound? out-t)
                                     (define kind-sym 'gcable-immobile)
                                     (define ptr-vm-type (ffi2-type-pointer-vm-type out-t #:gcable? (not (eq? kind-sym 'manual))))
                                     #`(lambda (in ...)
                                         (define r
                                           ((#%foreign-inline (ffi2-malloc-maker #,(ffi2-type-vm-type out-t) #,ptr-vm-type #,kind-sym)) 1))
                                         (proc r in ...)
                                         r)]
                                    [else #'proc])])
         #`(let ([ptr ptr-expr])
             (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
             (let ([proc ((#%foreign-inline (ffi2-procedure-maker (conv ...)
                                                                  #,(map ffi2-type-vm-type in-ts)
                                                                  #,(ffi2-type-vm-type out-t)))
                          ptr)])
               (let ([proc adjust-proc])
                 (lambda (in ...)
                   (unless (in-ok? in) (bad-argument 'in_t-name in))
                   ...
                   (#,(ffi2-type-c->racket out-t)
                    (proc (in-racket->c in) ...))))))))]))

(define-syntax (define-ffi2-procedure stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id name:id lib-expr
              (~and type
                    (-> in-type::maybe-type ...
                        (~optional (~seq #:varargs var-in-type::maybe-type ...))
                        out-type::maybe-type)))
     (with-syntax ([name-bstr (string->bytes/utf-8 (symbol->string (syntax-e #'name)))])
       #'(define name (ffi2-procedure (ffi2-lib-ref lib-expr name-bstr)
                                      type)))]))

(define-syntax (ffi2-callback stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id proc-expr
              (-> in-maybe-type::maybe-type ...
                  (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                  out-maybe-type::maybe-type))
     #:cut
     #:with ((~var in-type (:type stx)) ...) #'(in-maybe-type ...)
     #:with ((~var var-in-type (:type stx)) ...) (if (attribute var-in-maybe-type)
                                                     #'(var-in-maybe-type ...)
                                                     #'())
     #:with (~var out-type (:type stx #t)) #'out-maybe-type
     (define in-ts (append (attribute in-type.t) (attribute var-in-type.t)))
     (define out-t (attribute out-type.t))
     (with-syntax ([(in ...) (generate-temporaries in-ts)]
                   [(in-c->racket ...) (map ffi2-type-c->racket in-ts)]
                   [(conv ...) (append
                                (if (attribute var-in-maybe-type)
                                    (list (list '__varargs_after (length (attribute in-type))))
                                    null))])
       (with-syntax ([adjust-proc (cond
                                    [(ffi2-type-compound? out-t)
                                     #`(lambda (r in ...)
                                         (define out (proc in ...))
                                         (ffi2-memcpy r out (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type out-t))))
                                         r)]
                                    [else #'proc])])
         #`(let ([proc proc-expr]
                 [async-apply #f])
             (let ([proc (lambda (in ...)
                           (define out (proc (in-c->racket in) ...))
                           (unless (#,(ffi2-type-predicate out-t) out)
                             (bad-result '#,(ffi2-type-name out-t)) out)
                           (#,(ffi2-type-racket->c out-t) out))])
               (let ([proc adjust-proc])
                 ((#%foreign-inline (ffi2-callback-maker (__disable_interrupts conv ...)
                                                         #,(map ffi2-type-vm-type in-ts)
                                                         #,(ffi2-type-vm-type out-t)))
                  proc
                  async-apply))))))]))

(define (bad-assign-value who what val)
  (raise-arguments-error who "value does not match type"
                         "value" val
                         "ffi2 type" (unquoted-printing-string (format "~a" what))))

(define (bad-argument what val)
  (raise-arguments-error 'ffi2 "foreign-procedure argument does not match type"
                         "argument" val
                         "argument ffi2 type" (unquoted-printing-string (format "~a" what))))

(define (bad-result what val)
  (raise-arguments-error 'ffi2 "foreign-callback result does not match type"
                         "result" val
                         "result ffi2 type" (unquoted-printing-string (format "~a" what))))
