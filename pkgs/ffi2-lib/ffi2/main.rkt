#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (rename-in '#%foreign
                    [ffi2-lib-ref ffi2-lib-ref*]
                    [ffi2-free ffi2-free*]
                    [ffi2-memcpy ffi2-memcpy*]
                    [ffi2-memmove ffi2-memmove*]
                    [ffi2-memset ffi2-memset*]
                    [ffi2-ptr->cpointer ffi2-ptr->cpointer*]
                    [cpointer->ffi2-ptr cpointer->ffi2-ptr*])
         ffi/unsafe/private/ffi-lib
         ffi/unsafe/private/not-available
         racket/fixnum
         racket/symbol
         setup/dirs)

(provide (protect-out
          ffi2-lib
          ffi2-lib?
          ffi2-lib-ref
          define-ffi2-type
          define-ffi2-abi
          ffi2-procedure
          define-ffi2-procedure
          define-ffi2-definer
          ffi2-ptr-ref
          ffi2-ptr-set!
          ffi2-ptr-cast
          ffi2-bytes-cast
          ffi2-ptr-add
          ffi2-malloc
          ffi2-free
          ffi2-sizeof
          ffi2-offsetof
          ffi2-memcpy
          ffi2-memmove
          ffi2-memset
          ffi2-ptr->cpointer
          cpointer->ffi2-ptr
          ->
          struct
          union
          array
          system-type-case
          default_abi
          cdecl_abi
          stdcall_abi)
         ffi2-ptr?
         ffi2-ptr/gcable?
         make-not-available)

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

(define (ffi2-lib-ref lib name-in #:fail [failure #f])
  (define who 'ffi2-lib-ref)
  (unless (ffi-lib? lib) (raise-argument-error who "ffi2-lib?" lib))
  (when (and failure (not (and (procedure? failure) (procedure-arity-includes? failure 0))))
    (raise-argument-error who "(procedure-arity-includes/c 0)" failure))
  (define name
    (cond
      [(bytes? name-in) name-in]
      [(string? name-in) (string->bytes/utf-8 name-in)]
      [(symbol? name-in) (string->bytes/utf-8 (symbol->immutable-string name-in))]
      [else (raise-argument-error who "(or/c bytes? string? symbol?)" name-in)]))
  (if failure
      (with-handlers ([exn:fail:filesystem? (lambda (e) (failure))])
        (ffi2-lib-ref* lib name))
      (ffi2-lib-ref* lib name)))

(define-syntax (define-predicate stx)
  (syntax-parse stx
    [(_ (? arg) body)
     #'(define-syntax-rule (? arg-expr)
         (let ([arg arg-expr]) body))]))

(define-predicate (int8? v) (and (fixnum? v) (fx<= -128 v 127)))
(define-predicate (uint8? v) (byte? v))
(define-predicate (int16? v) (and (fixnum? v) (fx<= #x-8000 v #x7FFF)))
(define-predicate (uint16? v) (and (fixnum? v) (fx<= 0 v #xFFFF)))
(define-predicate (int32? v) (and (exact-integer? v) (<= #x-80000000 v #x7FFFFFFF)))
(define-predicate (uint32? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFF)))
(define-predicate (int64? v) (and (exact-integer? v) (<= #x-8000000000000000 v #x7FFFFFFFFFFFFFFF)))
(define-predicate (uint64? v) (and (exact-integer? v) (<= 0 v #xFFFFFFFFFFFFFFFF)))
(define-predicate (any? v) #t)
(define-predicate (string-or-false? s) (or (not s) (string? s)))
(define-predicate (bytes-or-false? s) (or (not s) (bytes? s)))
(define-predicate (long? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long) #:copy))
                                (int32? v)
                                (int64? v)))
(define-predicate (ulong? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof long) #:copy))
                                 (uint32? v)
                                 (uint64? v)))
(define-predicate (size_t? v) (if (fx= 4 (#%foreign-inline (ffi2-sizeof size_t) #:copy))
                                  (int32? v)
                                  (int64? v)))

(define (bytes-add-terminator bstr)
  (define len (bytes-length bstr))
  (define new-bstr (make-bytes (add1 len)))
  (memcpy new-bstr bstr len)
  new-bstr)

(define (string->bytes/utf-8/add-terminator str)
  (bytes-add-terminator (string->bytes/utf-8 str)))

(define-syntax (drop stx) #'(void))

(begin-for-syntax
  (struct ffi2-type (name vm-type category predicate racket->c c->racket release))
  (struct ffi2-type/proc ffi2-type (proc)
    #:property prop:procedure 0)
  (define (make-ffi2-type name vm-type predicate
                          #:category [category #f]
                          #:procedure [proc #f]
                          #:racket->c [racket->c #'values]
                          #:c->racket [c->racket #'values]
                          #:release [release #'drop])
    (if proc
        (ffi2-type/proc name vm-type category predicate racket->c c->racket release proc)
        (ffi2-type name vm-type category predicate racket->c c->racket release)))

  (define (ffi2-type-compound? t)
    (define vm-type (ffi2-type-vm-type t))
    (and (pair? vm-type)
         (memq (car vm-type) '(struct union))
         #t))

  (define (ffi2-type-pointer? t)
    (define vm-type (ffi2-type-vm-type t))
    (or (and (pair? vm-type)
             (memq (car vm-type) '(pointer pointer/gc))
             #t)
        (eq? vm-type 'pointer)
        (eq? vm-type 'pointer/gc)))

  (define (ffi2-type-pointer-vm-type t #:gcable? [gcable? #f])
    (define vm-type (ffi2-type-vm-type t))
    (or (and (pair? vm-type)
             (if (pair? (cadr vm-type))
                 (list (if gcable? 'pointer/gc 'pointer)
                       (cadr vm-type))
                 (if gcable? 'pointer/gc 'pointer)))
        (and (or (eq? vm-type 'pointer)
                 (eq? vm-type 'pointer/gc))
             (if gcable? 'pointer/gc 'pointer))))

  (define (ffi2-type-immediate-pointer? t)
    (eq? (ffi2-type-category t) 'ptr))

  (define (ffi2-type-scalar? t)
    (eq? (ffi2-type-category t) 'scalar))

  (define (lookup-type stx t-id #:for-return? [for-return? #f])
    (define v (syntax-local-value t-id (lambda () #f)))
    (unless (ffi2-type? v)
      (raise-syntax-error #f "not an ffi2 type" stx t-id))
    (unless (or for-return? (ffi2-type-racket->c v))
      (raise-syntax-error #f "ffi2 type allowed only as a procedure return" stx t-id))
    v)

  (define-syntax-class :malloc-kind
    (pattern (~or #:manual #:gcable #:gcable-traced #:gcable-immobile #:gcable-traced-immobile)))

  (struct procedure-abi (vm-abi))
  
  (define-syntax-class (:abi stx)
    #:attributes (a)
    (pattern name:id
             #:do [(define abi (syntax-local-value #'name (lambda () #f)))
                   (unless (procedure-abi? abi)
                     (raise-syntax-error #f "expected an ffi2 abi" stx #'name))]
             #:attr a (procedure-abi-vm-abi abi))
    (pattern (~and all ((~datum system-type-case) . _))
             #:attr a (parse-system-type-case/abi #'all))))

(define-syntax (define-ffi2-base-type stx)
  (syntax-parse stx
    [(_ name arg ...)
     #'(begin
         (define-syntax name (make-ffi2-type 'name arg ...))
         (provide name))]))

(define-ffi2-base-type void_t 'void #'void? #:racket->c #f)
(define-ffi2-base-type int8_t 'integer-8 #'int8? #:category 'scalar)
(define-ffi2-base-type uint8_t 'unsigned-8 #'uint8? #:category 'scalar)
(define-ffi2-base-type byte_t 'unsigned-8 #'byte? #:category 'scalar)
(define-ffi2-base-type int16_t 'integer-16 #'int16? #:category 'scalar)
(define-ffi2-base-type uint16_t 'unsigned-16 #'uint16? #:category 'scalar)
(define-ffi2-base-type int32_t 'integer-32 #'int32? #:category 'scalar)
(define-ffi2-base-type uint32_t 'unsigned-32 #'uint32? #:category 'scalar)
(define-ffi2-base-type int64_t 'integer-64 #'int64? #:category 'scalar)
(define-ffi2-base-type uint64_t 'unsigned-64 #'uint64? #:category 'scalar)
(define-ffi2-base-type int_t 'int #'int32? #:category 'scalar)
(define-ffi2-base-type uint_t 'unsigned #'uint32? #:category 'scalar)
(define-ffi2-base-type long_t 'long #'long? #:category 'scalar)
(define-ffi2-base-type ulon_t 'unsigned-long #'ulong? #:category 'scalar)
(define-ffi2-base-type size_t 'size_t #'size_t? #:category 'scalar)
(define-ffi2-base-type wchar_t 'wchar #'char? #:category 'scalar)
(define-ffi2-base-type fixnum_t 'fixnum #'fixnum? #:category 'scalar)
(define-ffi2-base-type float_t 'float #'flonum? #:category 'scalar)
(define-ffi2-base-type double_t 'double #'flonum? #:category 'scalar)
(define-ffi2-base-type bool_t 'stdbool #'any? #:category 'scalar)
(define-ffi2-base-type intbool_t 'bool #'any? #:category 'scalar)
(define-ffi2-base-type void_t* 'pointer #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type void_t*/gcable 'pointer/gc #'ffi2-ptr? #:release #'black-box #:category 'ptr)
(define-ffi2-base-type racket_t 'scheme-object #'any? #:release #'black-box)
(define-ffi2-base-type string_t 'u8* #'string-or-false? #:release #'black-box
  #:racket->c #'string->bytes/utf-8/add-terminator
  #:c->racket #'bytes->string/utf-8)
(define-ffi2-base-type bytes_t 'u8* #'bytes-or-false? #:release #'black-box
  #:racket->c #'bytes-add-terminator)
(define-ffi2-base-type path_t 'u8* #'string-or-false? #:release #'black-box
  #:racket->c #'path->bytes
  #:c->racket #'bytes->path)
(define-ffi2-base-type bytes_ptr_t 'u8* #'bytes-or-false? #:release #'black-box)

(define-for-syntax (raise-only-as-ffi-type stx)
  (raise-syntax-error #f "allowed only in an ffi2 type context" stx))

(define-syntax (-> stx) (raise-only-as-ffi-type stx))
(define-syntax (struct stx) (raise-only-as-ffi-type stx))
(define-syntax (union stx) (raise-only-as-ffi-type stx))
(define-syntax (array stx) (raise-only-as-ffi-type stx))
(define-syntax (system-type-case stx)
  (raise-syntax-error #f "allowed only in an ffi2 type or abi context" stx))

(define-syntax default_abi (procedure-abi #f))
(define-syntax stdcall_abi (procedure-abi '(__select os (windows) __stdcall #f)))
(define-syntax cdecl_abi (procedure-abi '(__select os (windows) __cdecl #f)))
    
(begin-for-syntax
  (define-syntax-class :maybe-type
    #:description "an ffi2 type"
    #:literals (-> struct union array)
    (pattern t:id
             #:when (ffi2-type? (syntax-local-value #'t (lambda () #f))))
    (pattern (-> _ ...))
    (pattern (struct _ ...))
    (pattern (union _ ...))
    (pattern (array _ ...)))

  (define-syntax-class (:type stx [for-return? #f])
    #:description "an ffi2 type"
    #:attributes (t)
    #:literals (-> struct union array system-type-case)
    (pattern type-name:id
             #:attr t (lookup-type stx #'type-name #:for-return? for-return?))
    (pattern (~and arrow-type
                   (-> in-maybe-type::maybe-type ...
                       (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                       out-maybe-type::maybe-type
                       (~optional (~seq #:abi (~var abi (:abi stx))))))
             #:cut
             #:with ((~var in-type (:type stx)) ...) #'(in-maybe-type ...)
             #:with ((~var var-in-type (:type stx)) ...) (if (attribute var-in-maybe-type)
                                                             #'(var-in-maybe-type ...)
                                                             #'())
             #:with (~var out-type (:type stx #t)) #'out-maybe-type
             #:with arity #`#,(length (syntax->list #'(in-type ... var-in-type ...)))
             #:attr t (make-ffi2-type '-> 'pointer #'(lambda (proc)
                                                       (and (procedure? proc)
                                                            (procedure-arity-includes? proc arity)))
                                      #:racket->c #`(lambda (proc)
                                                      (ffi2-callback proc arrow-type))
                                      #:c->racket #`(lambda (ptr)
                                                      (ffi2-procedure ptr arrow-type))
                                      #:release #'black-box
                                      #:category 'arrow))
    (pattern ((~and compound (~or struct union)) (~optional tag:id)
                                                 [field-name:id (~var field-type (:type stx))]
                                                 ...)
             #:with (field-vm-type ...) (map ffi2-type-vm-type (attribute field-type.t))
             #:with tag*s (if (attribute tag)
                              #`(#,(string->symbol (format "~a*" (syntax-e #'tag))))
                              #'())
             #:attr t (make-ffi2-type (syntax-e #'(~? tag compound)) (syntax->datum #'(compound tag*s (field-name field-vm-type) ...))
                                      (if (attribute tag)
                                          #'(lambda (v)
                                              (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy) v)
                                                  ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy) v)))
                                          #'ffi2-ptr?)
                                      #:release #'black-box))
    (pattern (array (~var elem-type (:type stx)) n:exact-nonnegative-integer)
             #:with tag*s (list (string->symbol (format "~a*" (ffi2-type-name (attribute elem-type.t)))))
             #:attr t (make-ffi2-type 'array `(array tag*s n ,(ffi2-type-vm-type (attribute elem-type.t)))
                                      #'(lambda (v)
                                          (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy) v)
                                              ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy) v)))
                                      #:release #'black-box))
    (pattern (~and all (system-type-case . _))
             #:attr t (parse-system-type-case/type #'all))))

(define-syntax (static-if stx)
  (syntax-parse stx
    [(_ #t t f) #'t]
    [(_ #f t f) #'f]))

(define-syntax (define-ffi2-type stx)
  (syntax-parse stx
    #:literals (struct union array)
    [(_ name:id ((~and compound (~or (~and is-s? struct) (~and is-u? union)))
                 (~optional tag:id)
                 [field-name:id (~var field-type (:type stx))]
                 ...))
     (with-syntax ([name* (datum->syntax #'name
                                         (string->symbol (format "~a*" (syntax-e #'name)))
                                         #'name)]
                   [name*/gcable (datum->syntax #'name
                                                (string->symbol (format "~a*/gcable" (syntax-e #'name)))
                                                #'name)]
                   [tag* (string->symbol (format "~a*" (syntax-e #'(~? tag name))))]
                   [fill-name (if (attribute is-s?)
                                  (car (generate-temporaries (list (format "fill-~a" (syntax-e #'name)))))
                                  #f)]
                   [(fill-field-name ...) (if (attribute is-u?)
                                              (generate-temporaries (for/list ([field-name (in-list (attribute field-name))])
                                                                      (format "fill-~a-~a" (syntax-e #'name) (syntax-e field-name))))
                                              '())]
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
           (define (tag-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer (tag*)) #:copy) v)
                                    ((#%foreign-inline (ffi2-ptr?-maker pointer/gc (tag*)) #:copy) v)))
           (define-syntax name* (make-ffi2-type 'name* '(pointer (tag*)) #'tag-ptr?
                                                #:release #'black-box
                                                #:category 'ptr))
           (define-syntax name*/gcable (make-ffi2-type 'name*/gcable '(pointer/gc (tag*)) #'tag-ptr?
                                                       #:release #'black-box
                                                       #:category 'ptr))
           (define-syntax name
             (make-ffi2-type 'name '(compound (tag*) (field-name field-vm-type) ...) #'tag-ptr?
                             #:release #'black-box
                             #:procedure
                             (lambda (stx)
                               (~? (syntax-parse stx
                                     [(_ (~optional kind::malloc-kind) field-name ...)
                                      'is-s?
                                      (with-syntax ([kind (or (attribute kind) #'#:gcable)])
                                        #'(fill-name (ffi2-malloc kind name)
                                                     field-name ...))])
                                   (syntax-parse stx
                                     [(_ (~datum field-name) (~optional kind::malloc-kind) expr)
                                      (with-syntax ([kind (or (attribute kind) #'#:gcable)])
                                        #'(fill-field-name (ffi2-malloc kind name)
                                                           expr))]
                                     ...)))))
           (define (name-field v)
             (unless (tag-ptr? v) (raise-argument-error 'name-field tag-ptr?-str v))
             (static-if field-compound?
                        ((#%foreign-inline (ffi2-ptr-cast-maker field-ptr-vm-type field-ptr/gcable-vm-type) #:copy)
                         v
                         (~? (begin 'is-u? 0)
                             (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name) #:copy)))
                        (field-c->racket
                         ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker field-vm-type)) #:copy)
                          v
                          (~? (begin 'is-u? 0)
                              (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name) #:copy))))))
           ...
           (define (set-name-field!/unchecked v val)
             (static-if field-compound?
                        (ffi2-memcpy* v (~? (begin 'is-u? 0)
                                            (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name)))
                                      val 0
                                      (#%foreign-inline (ffi2-sizeof field-vm-type) #:copy))
                        ((#%foreign-inline (begin-unsafe (ffi2-ptr-set!-maker field-vm-type)) #:copy)
                         v
                         (~? (begin 'is-u? 0)
                             (#%foreign-inline (ffi2-offsetof (compound (tag*) (field-name field-vm-type) ...) field-name) #:copy))
                         (field-racket->c val))))
           ...
           (define (set-name-field! v val)
             (unless (tag-ptr? v) (raise-argument-error 'set-name-field! tag-ptr?-str v))
             (unless (field-ok? val) (bad-assign-value 'set-name-field! 'field-type-name val))
             (set-name-field!/unchecked v val))
           ...
           (~? (define (fill-name p field-name ...)
                 'is-s?
                 (unless (field-ok? field-name) (bad-assign-value 'name 'field-type-name field-name))
                 ...
                 (set-name-field!/unchecked p field-name)
                 ...
                 p)
               (begin
                 (define (fill-field-name p v)
                   (unless (field-ok? v) (bad-assign-value 'name 'field-type-name v))
                   (set-name-field!/unchecked p v)
                   p)
                 ...))))]
    [(_ name:id (array (~var elem-type (:type stx)) n:exact-nonnegative-integer)
        (~optional (~seq #:tag tag:id)))
     (define elem-t (attribute elem-type.t))
     (with-syntax ([tag*s (list (or (attribute tag)
                                    (string->symbol (format "~a*" (ffi2-type-name elem-t)))))]
                   [tag-ptr? (datum->syntax #'name
                                            (string->symbol (format "~a?" (syntax-e #'name)))
                                            #'name)]
                   [tag-ptr?-str (format "~a*?" (syntax-e #'name))]
                   [name-set! (datum->syntax #'name
                                             (string->symbol (format "~a-set!" (syntax-e #'name)))
                                             #'name)]
                   [name-ref (datum->syntax #'name
                                            (string->symbol (format "~a-ref" (syntax-e #'name)))
                                            #'name)]
                   [range-str (format "(integer-in 0 ~a)" (sub1 (syntax-e #'n)))])
       #`(begin
           (define (tag-ptr? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer tag*s) #:copy) v)
                                    ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tag*s) #:copy) v)))           
           (define-syntax name
             (make-ffi2-type 'name `(array tag*s n #,(ffi2-type-vm-type elem-t)) #'tag-ptr?
                             #:release #'black-box
                             #:category 'ptr))
           (define (name-ref ptr idx)
             (unless (tag-ptr? ptr) (raise-argument-error 'name-ref tag-ptr?-str ptr))
             (unless (and (fixnum? idx) (fx<= 0 idx (sub1 n))) (raise-argument-error 'name-ref 'range-str idx))
             (#,(ffi2-type-c->racket elem-t)
              ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker #,(ffi2-type-vm-type elem-t))) #:copy)
               ptr
               (* idx (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type elem-t)) #:copy)))))
           (define (name-set! ptr idx val)
             (unless (tag-ptr? ptr) (raise-argument-error 'name-set! tag-ptr?-str ptr))
             (unless (and (fixnum? idx) (fx<= 0 idx (sub1 n))) (raise-argument-error 'name-set! 'range-str idx))
             (unless (#,(ffi2-type-predicate elem-t) val) (bad-assign-value 'name-set! '#,(ffi2-type-name elem-t) val))
             ((#%foreign-inline (begin-unsafe (ffi2-ptr-set!-maker #,(ffi2-type-vm-type elem-t))) #:copy)
              ptr
              (* idx (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type elem-t)) #:copy))
              (#,(ffi2-type-racket->c elem-t) val)))))]
    [(_ name:id
        (~var parent (:type stx))
        (~optional (~seq #:tag tag:id)))
     (define parent-t (attribute parent.t))
     (with-syntax ([name? (datum->syntax #'name
                                         (string->symbol (format "~a?" (syntax-e #'name)))
                                         #'name)])
       (cond
         [(ffi2-type-immediate-pointer? parent-t)
          (with-syntax ([name/gcable (datum->syntax #'name
                                                    (string->symbol (format "~a/gcable" (syntax-e #'name)))
                                                    #'name)]
                        [tags (cons #'(~? tag name)
                                    (if parent-t
                                        (let ([vm-type (ffi2-type-vm-type parent-t)])
                                          (if (pair? vm-type)
                                              (cadr vm-type)
                                              null))
                                        null))])
            #'(begin
                (define (name? v) (or ((#%foreign-inline (ffi2-ptr?-maker pointer tags) #:copy) v)
                                      ((#%foreign-inline (ffi2-ptr?-maker pointer/gc tags) #:copy) v)))
                (define-syntax name (make-ffi2-type 'name '(pointer tags) #'name?
                                                    #:release #'black-box
                                                    #:category 'ptr))
                (define-syntax name/gcable (make-ffi2-type 'name/gcable '(pointer/gc tags) #'name?
                                                           #:release #'black-box
                                                           #:category 'ptr))))]
         [else
          (when (attribute tag)
            (raise-syntax-error #f "base type for new tag is not an immediate pointer type" stx #'parent))
          #`(begin
              (define (name? v) (#,(ffi2-type-predicate parent-t) v))
              (define (racket->c v) (#,(ffi2-type-racket->c parent-t) v))
              (define (c->racket v) (#,(ffi2-type-c->racket parent-t) v))
              (define (release v) (#,(ffi2-type-release parent-t) v))
              (define-syntax name (make-ffi2-type 'name '#,(ffi2-type-vm-type parent-t) #'name?
                                                  #:category '#,(ffi2-type-category parent-t)
                                                  #:racket->c #'racket->c
                                                  #:c->racket #'c->racket
                                                  #:release #'release)))]))]))

(define-syntax (define-ffi2-abi stx)
  (syntax-parse stx
    [(_ name:id (~var abi (:abi stx)))
     #`(define-syntax name (procedure-abi '#,(attribute abi.a)))]))

(define-syntax (ffi2-ptr-ref stx)
  (syntax-parse stx
    [(form-id ptr-expr (~var type (:type stx)) (~optional (~seq offset-expr (~optional (~and abs #:bytes)))))
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         (#,(ffi2-type-c->racket t)
          ((#%foreign-inline (begin-unsafe (ffi2-ptr-ref-maker #,(ffi2-type-vm-type t))) #:copy)
           ptr
           #,(if (attribute abs)
                 #'offset
                 #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy))))))]))

(define-syntax (ffi2-ptr-set! stx)
  (syntax-parse stx
    [(form-id ptr-expr (~var type (:type stx))
              (~optional (~seq offset-expr (~optional (~and abs #:bytes))))
              val-expr)
     (define t (attribute type.t))
     #`(let ([ptr ptr-expr]
             [offset (~? offset-expr 0)]
             [val val-expr])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset))
           (unless (#,(ffi2-type-predicate t) val) (bad-assign-value 'form-id '#,(ffi2-type-name t) val)))
         ((#%foreign-inline (begin-unsafe (ffi2-ptr-set!-maker #,(ffi2-type-vm-type t))) #:copy)
          ptr
          #,(if (attribute abs)
                #'offset
                #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)))
          (#,(ffi2-type-racket->c t) val)))]))

(define-syntax (ffi2-malloc stx)
  (define (build kind-stx t n-expr-stx abs? as-t as-t-stx)
    (define kind-sym (string->symbol (keyword->string (syntax-e kind-stx))))
    (define size-vm-type (if abs?
                             'integer-8
                             (ffi2-type-vm-type t)))
    (define ptr-vm-type (cond
                          [as-t
                           (unless (ffi2-type-pointer? as-t)
                             (raise-syntax-error #f "result type is not a pointer type" stx as-t-stx))
                           (ffi2-type-pointer-vm-type as-t #:gcable? (not (eq? kind-sym 'manual)))]
                          [(and t (ffi2-type-pointer-vm-type t #:gcable? (not (eq? kind-sym 'manual))))
                           => (lambda (vm-type) vm-type)]
                          [(eq? kind-sym 'manual) #'pointer]
                          [else #'pointer/gc]))
    #`(let ([n #,n-expr-stx])
        (unless (exact-nonnegative-integer? n) (raise-argument-error 'form-id "exact-nonnegative-integer?" n))
        ((#%foreign-inline (ffi2-malloc-maker #,size-vm-type #,ptr-vm-type #,kind-sym) #:copy) n)))
  (syntax-parse stx
    [(form-id (~optional kind::malloc-kind)
              maybe-type::maybe-type
              (~optional (~seq n-expr:expr (~optional (~and abs #:bytes))))
              (~optional (~seq #:as (~var as-type (:type stx)))))
     #:with (~var type (:type stx)) #'maybe-type
     (build #'(~? kind #:gcable)
            (attribute type.t)
            #'(~? n-expr 1)
            (attribute abs)
            (attribute as-type.t)
            (attribute as-type))]
    [(form-id (~optional kind::malloc-kind)
              n-expr:expr (~optional #:bytes)
              (~optional (~seq #:as (~var as-type (:type stx)))))
     (build #'(~? kind #:gcable)
            #f
            #'n-expr
            #t
            (attribute as-type.t)
            (attribute as-type))]))

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
     #`(#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)]))

(define-syntax (ffi2-offsetof stx)
  (syntax-parse stx
    [(form-id (~var type (:type stx)) field-name:id)
     (define t (attribute type.t))
     (define vm-type (ffi2-type-vm-type t))
     (unless (for/or ([field (in-list (if (pair? vm-type) (cddr vm-type) null))])
               (eq? (car field) (syntax-e #'field-name)))
       (raise-syntax-error #f "field name not found in type" stx #'field-name))
     (cond
       [(and (pair? vm-type) (eq? (car vm-type) 'union))
        #'0]
       [else
        #`(#%foreign-inline (ffi2-offsetof #,vm-type field-name) #:copy)])]))

(define-syntax (ffi2-procedure stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id ptr-expr
              (-> in-maybe-type::maybe-type ...
                  (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                  out-maybe-type::maybe-type
                  (~optional (~seq #:abi (~var abi (:abi stx))))))
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
                   [(in-release ...) (map ffi2-type-release in-ts)]
                   [(conv ...) (append
                                (if (attribute abi)
                                    (list (attribute abi.a))
                                    null)
                                (if (attribute var-in-maybe-type)
                                    (list (list '__varargs_after (length (attribute in-type))))
                                    null))])
       (with-syntax ([adjust-proc (cond
                                    [(ffi2-type-compound? out-t)
                                     (define kind-sym 'gcable-immobile)
                                     (define ptr-vm-type (ffi2-type-pointer-vm-type out-t #:gcable? (not (eq? kind-sym 'manual))))
                                     #`(lambda (in ...)
                                         (define r
                                           ((#%foreign-inline (ffi2-malloc-maker #,(ffi2-type-vm-type out-t) #,ptr-vm-type #,kind-sym) #:copy) 1))
                                         (proc r in ...)
                                         r)]
                                    [else #'proc])])
         #`(let ([ptr ptr-expr])
             (unless (variable-reference-from-unsafe? (#%variable-reference))
               (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr)))
             (let ([proc ((#%foreign-inline (begin-unsafe
                                              (ffi2-procedure-maker (conv ...)
                                                                    #,(map ffi2-type-vm-type in-ts)
                                                                    #,(ffi2-type-vm-type out-t)))
                                            #:copy)
                          ptr)])
               (let ([proc adjust-proc])
                 (lambda (in ...)
                   (unless (in-ok? in) (bad-argument 'in_t-name in))
                   ...
                   (#,(ffi2-type-c->racket out-t)
                    (let ([in (in-racket->c in)] ...)
                      (let ([out (proc in ...)])
                        ;; the `release` function can usefully be something like `black-box` to
                        ;; retain a converted argument until the foreign procedure returns
                        (in-release in) ...
                        out)))))))))]
    [(form-id ptr-expr (~var type (:type stx)))
     (define t (attribute type.t))
     (unless (eq? (ffi2-type-category t) 'arrow)
       (raise-syntax-error #f "not a procedure type" stx #'type))
     #`(let ([ptr ptr-expr])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr)))
         (#,(ffi2-type-c->racket t) ptr))]))

(define-for-syntax (parse-define-ffi2-procedure stx use-lib-expr
                                                #:default-fail [default-fail #f]
                                                #:default-wrap [default-wrap #f]
                                                #:provide? [provide? #f])
  (syntax-parse stx
    #:literals (->)
    [(form-id name:id maybe-type::maybe-type
              (~alt (~optional (~seq #:lib lib-expr))
                    (~optional (~seq #:c-id c-name:id))
                    (~optional (~seq #:fail fail-expr))
                    (~optional (~seq #:wrap wrap-expr)))
              ...)
     (cond
       [(not use-lib-expr)
        (unless (attribute lib-expr)
          (raise-syntax-error #f "missing a `#:lib` clause to specify the source foreign library" stx))]
       [else
        (when (attribute lib-expr)
          (raise-syntax-error #f "redundant or conflicting `#:lib`" stx #'lib-expr))])
     (with-syntax ([lib-expr (or (attribute lib-expr) use-lib-expr)]
                   [c-name #'(~? c-name name)])
       (with-syntax ([name-bstr (string->bytes/utf-8 (symbol->string (syntax-e #'c-name)))]
                     [wrapper (if (attribute wrap-expr) #'wrap (or default-wrap #'begin))]
                     [build-default-fail (if default-fail
                                             #`(lambda () (failure-result (#,default-fail 'c-name)))
                                             #'#f)]
                     [(name-provide ...) (if provide?
                                             #'((provide (protect-out name)))
                                             #'())])
         #`(begin
             name-provide ...
             (~? (define wrap (check-wrap-proc 'form-id wrap-expr)))
             (define name-ptr (ffi2-lib-ref lib-expr name-bstr
                                            #:fail (~? (build-fail 'form-id fail-expr 'c-name)
                                                       build-default-fail)))             
             (define name (wrapper
                           #,(if (or (attribute fail-expr) default-fail)
                                 #'(if (failure-result? name-ptr)
                                       (failure-result-v name-ptr)
                                       (ffi2-procedure name-ptr maybe-type))
                                 #'(ffi2-procedure name-ptr maybe-type)))))))]))

(define (check-wrap-proc who wrap)
  (unless (and (procedure? wrap) (procedure-arity-includes? wrap 1))
    (raise-argument-error who "(procedure-arity-includes/c 1)" wrap))
  wrap)

(define (check-fail-proc who fail)
  (unless (or (not fail) (and (procedure? fail) (procedure-arity-includes? fail 1)))
    (raise-argument-error who "(procedure-arity-includes/c 1)" fail))
  fail)

(define (build-fail who fail name)
  (check-fail-proc who fail)
  (and fail (lambda () (failure-result (fail name)))))

(define-struct failure-result (v))

(define-syntax (define-ffi2-procedure stx)
  (parse-define-ffi2-procedure stx #f))

(define-syntax (define-ffi2-definer stx)
  (syntax-parse stx
    [(form-id name:id
              (~alt (~optional (~seq #:lib lib-expr))
                    (~optional (~seq #:default-fail fail-expr))
                    (~optional (~seq #:default-wrap wrap-expr))
                    (~optional (~and provide? #:provide)
                               #:defaults ([provide? #'#f])))
              ...)
     (unless (attribute lib-expr)
       (raise-syntax-error #f "missing a `#:lib` clause" stx))
     (with-syntax ([fail-id (if (attribute fail-expr) #'(quote-syntax fail) #'#f)]
                   [wrap-id (if (attribute wrap-expr) #'(quote-syntax wrap) #'#f)])
       #'(begin
           (define lib lib-expr)
           (~? (define wrap (check-wrap-proc 'form-id wrap-expr)))
           (~? (define fail (check-fail-proc 'form-id fail-expr)))
           (define-syntax name
             (lambda (stx)
               (parse-define-ffi2-procedure stx #'lib
                                            #:default-fail fail-id
                                            #:default-wrap wrap-id
                                            #:provide? 'provide?)))))]))

(define-syntax (ffi2-callback stx)
  (syntax-parse stx
    #:literals (->)
    [(form-id proc-expr
              (-> in-maybe-type::maybe-type ...
                  (~optional (~seq #:varargs var-in-maybe-type::maybe-type ...))
                  out-maybe-type::maybe-type
                  (~optional (~seq #:abi (~var abi (:abi stx))))))
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
                   [(in-release ...) (map ffi2-type-release in-ts)]
                   [(conv ...) (append
                                (if (attribute abi)
                                    (list (attribute abi.a))
                                    null)
                                (if (attribute var-in-maybe-type)
                                    (list (list '__varargs_after (length (attribute in-type))))
                                    null))])
       (with-syntax ([adjust-proc (cond
                                    [(ffi2-type-compound? out-t)
                                     #`(lambda (r in ...)
                                         (define out (proc in ...))
                                         (ffi2-memcpy r out (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type out-t))
                                                                               #:copy))
                                         r)]
                                    [else #'proc])])
         #`(let ([proc proc-expr]
                 [async-apply #f])
             (let ([proc (lambda (in ...)
                           (define out (proc (in-c->racket in) ...))
                           (unless (#,(ffi2-type-predicate out-t) out)
                             (bad-result '#,(ffi2-type-name out-t)) out)
                           (in-release in) ...
                           (#,(ffi2-type-racket->c out-t) out))])
               (let ([proc adjust-proc])
                 ((#%foreign-inline (ffi2-callback-maker (__disable_interrupts conv ...)
                                                         #,(map ffi2-type-vm-type in-ts)
                                                         #,(ffi2-type-vm-type out-t))
                                    #:copy)
                  proc
                  async-apply))))))]))

(define-for-syntax (parse-ffi2-ptr-cast stx from-bytes?)
  (syntax-parse stx
    [(form-id expr (~var to (:type stx))
              (~optional (~seq offset-expr #:bytes)))
     (define t (attribute to.t))
     (unless (ffi2-type-pointer? t)
       (raise-syntax-error #f "target type is not a pointer type" stx #'to))
     (define vm-type (and (not from-bytes?)
                          (ffi2-type-pointer-vm-type t #:gcable? #f)))
     (define gcable-vm-type (ffi2-type-pointer-vm-type t #:gcable? #t))
     #`(let ([ptr expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           #,(if from-bytes?
                 #`(unless (bytes? ptr) (raise-argument-error 'form-id "bytes?" ptr))
                 #`(unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr)))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         (#,(if from-bytes?
                #`(#%foreign-inline (ffi2-ptr-cast-maker #f #,gcable-vm-type) #:copy)
                #`(#%foreign-inline (ffi2-ptr-cast-maker #,vm-type #,gcable-vm-type) #:copy))
          #,(if from-bytes?
                #`(cpointer->ffi2-ptr* #f ptr)
                #`ptr)
          offset))]))

(define-syntax (ffi2-ptr-cast stx)
  (parse-ffi2-ptr-cast stx #f))

(define-syntax (ffi2-bytes-cast stx)
  (parse-ffi2-ptr-cast stx #t))

(define-syntax (ffi2-ptr-add stx)
  (syntax-parse stx
    [(form-id expr offset-expr #:bytes)
     #`(let ([ptr expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         ((#%foreign-inline (ffi2-ptr-cast-maker pointer pointer/gc) #:copy)
          ptr
          offset))]
    [(form-id expr (~var to (:type stx))
              offset-expr (~optional (~and abs #:bytes)))
     (define t (attribute to.t))
     (define vm-type (if (ffi2-type-compound? t)
                         (ffi2-type-pointer-vm-type t #:gcable? #f)
                         'pointer))
     (define gcable-vm-type (if (ffi2-type-compound? t)
                                (ffi2-type-pointer-vm-type t #:gcable? #t)
                                'pointer/gc))
     #`(let ([ptr expr]
             [offset (~? offset-expr 0)])
         (unless (variable-reference-from-unsafe? (#%variable-reference))
           (unless (ffi2-ptr? ptr) (raise-argument-error 'form-id "ffi2-ptr?" ptr))
           (unless (exact-integer? offset) (raise-argument-error 'form-id "exact-integer?" offset)))
         ((#%foreign-inline (ffi2-ptr-cast-maker #,vm-type #,gcable-vm-type) #:copy)
          ptr
          #,(if (attribute abs)
                #'offset
                #`(* offset (#%foreign-inline (ffi2-sizeof #,(ffi2-type-vm-type t)) #:copy)))))]))

(define (ffi2-ptr->cpointer ptr)
  (unless (ffi2-ptr? ptr)
    (raise-argument-error 'ffi2-ptr->cpointer "ffi2-ptr?" ptr))
  (ffi2-ptr->cpointer* ptr))

(define (cpointer->ffi2-ptr ptr)
  (cpointer->ffi2-ptr* 'cpointer->ffi2-ptr ptr))

(define-syntax-rule (discourage-inline)
  (#%foreign-inline (void)))

(define (bad-assign-value who what val)
  (discourage-inline)
  (raise-arguments-error who "value does not match type"
                         "value" val
                         "ffi2 type" (unquoted-printing-string (format "~a" what))))

(define (bad-argument what val)
  (discourage-inline)
  (raise-arguments-error 'ffi2 "foreign-procedure argument does not match type"
                         "argument" val
                         "argument ffi2 type" (unquoted-printing-string (format "~a" what))))

(define (bad-result what val)
  (discourage-inline)
  (raise-arguments-error 'ffi2 "foreign-callback result does not match type"
                         "result" val
                         "result ffi2 type" (unquoted-printing-string (format "~a" what))))

(define-for-syntax (parse-system-type-case stx
                                           parse-one
                                           make-one make-combined)
  (syntax-parse stx
    [(_
      (~and key (~or (~datum os) (~datum os*) (~datum arch) (~datum word)))
      [(val ...) rhs]
      ...
      [(~datum else) else-rhs])

     (for ([val-stx (in-list (syntax->list #'(val ... ...)))])
       (if (eq? (syntax-e #'key) 'word)
           (unless (memv (syntax-e val-stx) '(32 64))
             (raise-syntax-error #f "expected 32 or 64" stx val-stx))
           (unless (symbol? (syntax-e val-stx))
             (raise-syntax-error #f "expected an identifier" stx val-stx))))

     (define rhs-stxs (append (attribute rhs) (list #'else-rhs)))
     (define rhs-ts (for/list ([rhs-stx (in-list rhs-stxs)])
                      (parse-one stx rhs-stx)))
     
     (let loop ([rhs-ts rhs-ts] [valss (syntax->list #'((val ...) ...))])
       (cond
         [(null? valss) (make-one (car rhs-ts))]
         [else
          (define combined (loop (cdr rhs-ts) (cdr valss)))
          (make-combined (syntax-e #'key)
                         (car valss)
                         (make-one (car rhs-ts))
                         combined)]))]))

(define-for-syntax (parse-system-type-case/type stx)
  (define p
    (parse-system-type-case stx
                            (lambda (stx rhs-stx)
                              (syntax-parse rhs-stx
                                [(~var rhs (:type stx))
                                 (unless (ffi2-type-scalar? (attribute rhs.t))
                                   (raise-syntax-error #f "expected a scalar type" stx rhs-stx))
                                 (attribute rhs.t)]))
                            (lambda (rhs-t)
                              (cons (ffi2-type-vm-type rhs-t)
                                    (ffi2-type-predicate rhs-t)))
                            (lambda (key vals left right)
                              (cons (list 'select key vals (car left) (car right))
                                    #`(#%foreign-inline
                                       (ffi2-system-type--select #,key #,vals #,(cdr left) #,(cdr right)))))))

    (make-ffi2-type 'system-type-case (car p) (cdr p)
                    #:category 'scalar))

(define-for-syntax (parse-system-type-case/abi stx)
  (parse-system-type-case stx
                          (lambda (stx rhs-stx)
                            (syntax-parse rhs-stx
                              [(~var abi (:abi stx))
                               (attribute abi.a)]))
                          (lambda (rhs-a) rhs-a)
                          (lambda (key vals left right)
                            (list '(__select key vals left right)))))
