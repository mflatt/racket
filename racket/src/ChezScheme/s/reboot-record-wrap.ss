(define-syntax define-record-type/orig
  (syntax-rules ()
    [(_ . rest) (define-record-type . rest)]))

(include "reboot-record.ss")

(library (reboot-records)
  (export record?
          record-rtd
          record-type-uid
          record-type-name
          record-type-sealed?
          record-type-opaque?
          record-type-parent
          make-record-type-descriptor
          record-type-descriptor?
          
          make-record-constructor-descriptor
          record-constructor-descriptor?
          record-constructor
          r6rs:record-constructor
          record-predicate
          record-accessor
          record-mutator
          record-type-field-names
          record-type-descriptor
          record-constructor-descriptor
          define-record
          define-record-type

          $record?
          $sealed-record?
          $record-type-descriptor
          $make-record-type
          $make-record-type-descriptor
          $make-record-type-descriptor*
          $make-record-constructor-descriptor
          make-record-type
          $remake-rtd
          $record
          type-descriptor
          csv7:record-field-accessor
          csv7:record-field-mutator
          csv7:record-field-mutable?
          csv7:record-field-accessible?
          record-type-field-indices
          csv7:record-type-field-names
          $record-type-field-indices
          csv7:record-type-field-decls
          record-writer
          $object-ref)
  (import (except (rename (chezscheme)
                          [define-record-type define-record-type/orig])
                  record?
                  record-rtd
                  record-type-uid
                  record-type-name
                  record-type-sealed?
                  record-type-opaque?
                  record-type-parent
                  make-record-type-descriptor
                  record-type-descriptor?
                  define-record
                  
                  make-record-constructor-descriptor
                  record-constructor-descriptor?
                  record-constructor
                  r6rs:record-constructor
                  record-predicate
                  record-accessor
                  record-mutator
                  record-type-field-names
                  record-type-descriptor
                  record-constructor-descriptor

                  csv7:record-field-accessor
                  csv7:record-field-mutator
                  csv7:record-field-mutable?
                  csv7:record-field-accessible?
                  csv7:record-type-field-names
                  csv7:record-type-field-decls

                  record-writer
                  make-record-type
                  type-descriptor))
  (define-syntax define-primitive
    (syntax-rules ()
      [(_ . rest) (define . rest)]))
  (include "reboot-record.ss"))

(library (rnrs-no-records)
  (export)
  (import (rnrs)
          (only (chezscheme)
                export))
  (export (import (except (rnrs)
                          record?
                          record-rtd
                          record-type-uid
                          record-type-name
                          record-type-sealed?
                          record-type-opaque?
                          record-type-parent
                          make-record-type-descriptor
                          record-type-descriptor?

                          make-record-constructor-descriptor
                          record-constructor
                          record-predicate
                          record-accessor
                          record-mutator
                          record-type-field-names
                          record-type-descriptor
                          record-constructor-descriptor
                          define-record-type))))

(library (chezscheme-no-records)
  (export)
  (import (chezscheme))
  (export (import (except (chezscheme)
                          record?
                          record-rtd
                          record-type-uid
                          record-type-name
                          record-type-sealed?
                          record-type-opaque?
                          record-type-parent
                          make-record-type-descriptor
                          record-type-descriptor?

                          make-record-constructor-descriptor
                          record-constructor-descriptor?
                          record-constructor
                          r6rs:record-constructor
                          record-predicate
                          record-accessor
                          record-mutator
                          record-type-field-names
                          record-type-descriptor
                          record-constructor-descriptor
                          define-record-type
                          define-record

                          csv7:record-field-accessor
                          csv7:record-field-mutator
                          csv7:record-field-mutable?
                          csv7:record-field-accessible?
                          csv7:record-type-field-names
                          csv7:record-type-field-decls

                          record-writer
                          make-record-type
                          type-descriptor))))

(define-syntax orig-library (top-level-syntax 'library))
(define-syntax (library stx)
  (syntax-case stx ()
    [(_ id outs (import in ...) . rest)
     (with-syntax ([(new-in ...)
                    (apply
                     append
                     (map (lambda (in)
                            (syntax-case in (rnrs chezscheme)
                              [(rnrs)
                               (with-syntax ([(id) in])
                                 (let ([same (lambda (sym) (datum->syntax #'id sym))])
                                   (list #`(#,(same 'rnrs-no-records))
                                         #`(#,(same 'reboot-records)))))]
                              [(chezscheme)
                               (with-syntax ([(id) in])
                                 (let ([same (lambda (sym) (datum->syntax #'id sym))])
                                   (list #`(#,(same 'chezscheme-no-records))
                                         #`(#,(same 'reboot-records)))))]
                              [_ (list in)]))
                          #'(in ...)))])
       #'(orig-library id outs (import new-in ...) . rest))]))

(define compile-time-value? #%compile-time-value?)
(define compile-time-value-value #%compile-time-value-value)

(define (translate-compile-time-value v)
  (if (compile-time-value? v)
      (orig-make-compile-time-value (compile-time-value-value v))
      v))

(define-syntax orig-define-syntax (top-level-syntax 'define-syntax))

(orig-define-syntax
 define-syntax
 (lambda (stx)
   (syntax-case stx ()
     [(_ (id . args) . body)
      #`(orig-define-syntax (id . args) . body)]
     [(_ id rhs)
      #`(orig-define-syntax id (translate-compile-time-value rhs))])))
