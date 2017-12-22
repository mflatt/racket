#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "struct-type-info.rkt"
         "simple.rkt"
         "find-definition.rkt"
         "mutated.rkt"
         "mutated-state.rkt"
         "left-to-right.rkt"
         "serialize.rkt"
         "let.rkt"
         "equal.rkt")

(provide schemify-linklet
         schemify-body)

;; Convert a linklet to a Scheme `lambda`, dealing with several
;; issues:
;;
;;   - imports and exports are represented by `variable` objects that
;;     are passed to the function; to avoid obscuring the program to
;;     the optimizer, though, refer to the definitions of exported
;;     variables instead of going through the `variable`, whenever
;;     possible, and accept values instead of `variable`s for constant
;;     imports;
;;
;;   - wrap expressions in a sequence of definitions plus expressions
;;     so that the result body is a sequence of definitions followed
;;     by a single expression;
;;
;;   - convert function calls and `let` forms to enforce left-to-right
;;     evaluation;
;;
;;   - convert function calls to support applicable structs, using
;;     `#%app` whenever a call might go through something other than a
;;     plain function;
;;
;;   - convert `make-struct-type` bindings to a pattern that Chez can
;;     recognize;
;;
;;   - optimize away `variable-reference-constant?` uses, which is
;;     important to make keyword-argument function calls work directly
;;     without keywords;
;;
;;   - simplify `define-values` and `let-values` to `define` and
;;     `let`, when possible, and generally avoid `let-values`.

;; The given linklet can have parts wrapped as `syntax?` objects.
;; When called from the Racket expander, those syntax objects
;; will be "correlated" objects that just support source locations.
;; The job of `annotate` is to take an original term (which might
;; be `syntax?`) and a schemified term and potentially make it
;; an annotatation. The `unannotate` function should return a
;; stripped S-expression back; it's used for post-Schemify checks,
;; and `unannotate` is expected to be constant time.

;; Returns (values schemified-linklet import-abi export-info)
;; An import ABI is a list of list of booleans, parallel to the
;; linklet imports, where #t to means that a value is expected, and #f
;; means that a variable (which boxes a value) is expected
(define (schemify-linklet lk serializable? for-jit?
                          annotate unannotate prim-knowns get-import-knowns)
  (define (im-int-id id) (unwrap (if (pair? id) (cadr id) id)))
  (define (im-ext-id id) (unwrap (if (pair? id) (car id) id)))
  (define (ex-int-id id) (unwrap (if (pair? id) (car id) id)))
  (define (ex-ext-id id) (unwrap (if (pair? id) (cadr id) id)))
  ;; Assume no wraps unless the level of an id or expression
  (match lk
    [`(linklet ,im-idss ,ex-ids . ,bodys)
     ;; For imports, map symbols to gensymed `variable` argument names:
     (define imports
       (for/fold ([imports (hasheq)]) ([im-ids (in-list im-idss)]
                                       [index (in-naturals)])
         (define grp (import-group (lambda () (get-import-knowns index)) #f))
         (for/fold ([imports imports]) ([im-id (in-list im-ids)])
           (define id (im-int-id im-id))
           (define ext-id (im-ext-id im-id))
           (hash-set imports id (import grp (gensym (symbol->string id)) ext-id)))))
     ;; Ditto for exports:
     (define exports
       (for/fold ([exports (hasheq)]) ([ex-id (in-list ex-ids)])
         (define id (ex-int-id ex-id))
         (hash-set exports id (gensym (symbol->string id)))))
     ;; Lift any quoted constants that can't be serialized
     (define-values (bodys/constants-lifted lifted-constants)
       (if serializable?
           (convert-for-serialize bodys)
           (values bodys null)))
     ;; Schemify the body, collecting information about defined names:
     (define-values (new-body defn-info mutated)
       (schemify-body* bodys/constants-lifted annotate unannotate prim-knowns imports exports
                       for-jit?))
     (values
      ;; Build `lambda` with schemified body:
      (make-let*
       lifted-constants
       `(lambda (instance-variable-reference
                 ,@(for*/list ([im-ids (in-list im-idss)]
                               [im-id (in-list im-ids)])
                     (import-id (hash-ref imports (im-int-id im-id))))
                 ,@(for/list ([ex-id (in-list ex-ids)])
                     (hash-ref exports (ex-int-id ex-id))))
          ,@new-body))
      ;; Import ABI: request values for constants, `variable`s otherwise
      (for/list ([im-ids (in-list im-idss)])
        (define grp (and (pair? (unwrap im-ids))
                         (import-grp (hash-ref imports (im-int-id (wrap-car im-ids))))))
        (define im-ready? (and grp (import-group-lookup-ready? grp)))
        (for/list ([im-id (in-list im-ids)])
          (and im-ready?
               (known-constant? (import-group-lookup grp (im-ext-id im-id))))))
      ;; Convert internal to external identifiers
      (for/fold ([knowns (hasheq)]) ([ex-id (in-list ex-ids)])
        (define id (ex-int-id ex-id))
        (define v (hash-ref defn-info id #f))
        (cond
         [(and v
               (not (set!ed-mutated-state? (hash-ref mutated id #f))))
          (define ext-id (ex-ext-id ex-id))
          (hash-set knowns ext-id v)]
         [else knowns])))]))

;; ----------------------------------------

(define (schemify-body l annotate unannotate prim-knowns imports exports)
  (define-values (new-body defn-info mutated)
    (schemify-body* l annotate unannotate prim-knowns imports exports #f))
  new-body)

(define (schemify-body* l annotate unannotate prim-knowns imports exports for-jit?)
  ;; Various conversion steps need information about mutated variables,
  ;; where "mutated" here includes visible implicit mutation, such as
  ;; a variable that might be used before it is defined:
  (define mutated (mutated-in-body l exports prim-knowns (hasheq) imports))
  ;; Make another pass to gather known-binding information:
  (define knowns
    (for/fold ([knowns (hasheq)]) ([form (in-list l)])
      (define-values (new-knowns info)
        (find-definitions form prim-knowns knowns imports mutated))
      new-knowns))
  ;; While schemifying, add calls to install exported values in to the
  ;; corresponding exported `variable` records, but delay those
  ;; installs to the end, if possible
  (define schemified
    (let loop ([l l] [accum-exprs null] [accum-ids null])
      (cond
       [(null? l)
        (define set-vars
          (for/list ([id (in-list accum-ids)]
                     #:when (hash-ref exports id #f))
            (make-set-variable id exports knowns mutated)))
        (cond
         [(null? set-vars)
          (cond
           [(null? accum-exprs) '((void))]
           [else (reverse accum-exprs)])]
         [else (append
                accum-exprs
                set-vars)])]
       [else
        (define form (car l))
        (define schemified (schemify form annotate unannotate
                                     prim-knowns knowns mutated imports exports))
        (match form
          [`(define-values ,ids ,_)
           (append
            (if for-jit?
                accum-exprs
                (make-expr-defns accum-exprs))
            (cons
             schemified 
             (let id-loop ([ids ids] [accum-exprs null] [accum-ids accum-ids])
               (cond
                [(wrap-null? ids) (loop (wrap-cdr l) accum-exprs accum-ids)]
                [(or for-jit?
                     (via-variable-mutated-state? (hash-ref mutated (unwrap (wrap-car ids)) #f)))
                 (define id (unwrap (wrap-car ids)))
                 (cond
                  [(hash-ref exports id #f)
                   (id-loop (wrap-cdr ids)
                            (cons (make-set-variable id exports knowns mutated)
                                  accum-exprs)
                            accum-ids)]
                  [else
                   (id-loop (wrap-cdr ids) accum-exprs accum-ids)])]
                [else
                 (id-loop (wrap-cdr ids) accum-exprs (cons (unwrap (wrap-car ids)) accum-ids))]))))]
          [`,_
           (loop (wrap-cdr l) (cons schemified accum-exprs) accum-ids)])])))
  ;; Return both schemified and known-binding information, where
  ;; the later is used for cross-linklet optimization
  (values schemified knowns mutated))

(define (make-set-variable id exports knowns mutated)
  (define int-id (unwrap id))
  (define ex-var (hash-ref exports int-id))
  `(variable-set! ,ex-var ,id ',(variable-constance int-id knowns mutated)))

(define (make-expr-defns accum-exprs)
  (for/list ([expr (in-list (reverse accum-exprs))])
    `(define ,(gensym) (begin ,expr (void)))))

(define (variable-constance id knowns mutated)
  (cond
    [(set!ed-mutated-state? (hash-ref mutated id #f))
     #f]
    [(known-constant? (hash-ref knowns id #f))
     'consistent]
    [else
     'constant]))

;; ----------------------------------------

;; Schemify `let-values` to `let`, etc., and
;; reorganize struct bindings.
(define (schemify v annotate unannotate prim-knowns knowns mutated imports exports)
  (let schemify/knowns ([knowns knowns] [v v])
    (let schemify ([v v])
      (annotate
       v
       (match v
         [`(lambda ,formals ,body ...)
          `(lambda ,formals ,@(map schemify body))]
         [`(case-lambda [,formalss ,bodys ...] ...)
          `(case-lambda ,@(for/list ([formals (in-list formalss)]
                                [body (in-list bodys)])
                       `[,formals ,@(map schemify body)]))]
         [`(define-values (,struct:s ,make-s ,s? ,acc/muts ...)
            (let-values (((,struct: ,make ,?1 ,-ref ,-set!) ,mk))
              (values ,struct:2
                      ,make2
                      ,?2
                      ,make-acc/muts ...)))
          ;; Convert a `make-struct-type` binding into a 
          ;; set of bindings that Chez's cp0 recognizes,
          ;; and push the struct-specific extra work into
          ;; `struct-type-install-properties!`
          (define sti (and (wrap-eq? struct: struct:2)
                           (wrap-eq? make make2)
                           (wrap-eq? ?1 ?2)
                           (make-struct-type-info mk prim-knowns knowns imports mutated)))
          (cond
           [(and sti
                 ;; make sure `struct:` isn't used too early, since we're
                 ;; reordering it's definition with respect to some arguments
                 ;; of `make-struct-type`:
                 (simple-mutated-state? (hash-ref mutated (unwrap struct:) #f)))
            (define can-impersonate? (not (struct-type-info-authentic? sti)))
            (define raw-s? (if can-impersonate? (gensym s?) s?))
            `(begin
              (define ,struct:s (make-record-type-descriptor ',(struct-type-info-name sti)
                                                             ,(schemify (struct-type-info-parent sti))
                                                             ,(if (not (struct-type-info-prefab-immutables sti))
                                                                  #f
                                                                  `(structure-type-lookup-prefab-uid
                                                                    ',(struct-type-info-name sti)
                                                                    ,(schemify (struct-type-info-parent sti))
                                                                    ,(struct-type-info-immediate-field-count sti)
                                                                    0 #f
                                                                    ',(struct-type-info-prefab-immutables sti)))
                                                             #f
                                                             #f
                                                             ',(for/vector ([i (in-range (struct-type-info-immediate-field-count sti))])
                                                                 `(mutable ,(string->symbol (format "f~a" i))))))
              ,@(if (null? (struct-type-info-rest sti))
                    null
                    `((define ,(gensym)
                        (struct-type-install-properties! ,struct:s
                                                         ',(struct-type-info-name sti)
                                                         ,(struct-type-info-immediate-field-count sti)
                                                         0
                                                         ,(schemify (struct-type-info-parent sti))
                                                         ,@(map schemify (struct-type-info-rest sti))))))
              (define ,make-s ,(let ([ctr `(record-constructor
                                            (make-record-constructor-descriptor ,struct:s #f #f))])
                                 (if (struct-type-info-pure-constructor? sti)
                                     ctr
                                     `(struct-type-constructor-add-guards ,ctr ,struct:s ',(struct-type-info-name sti)))))
              (define ,raw-s? (record-predicate ,struct:s))
              ,@(if can-impersonate?
                    `((define ,s? (lambda (v) (if (,raw-s? v) #t (pariah (if (impersonator? v) (,raw-s? (impersonator-val v)) #f))))))
                    null)
              ,@(for/list ([acc/mut (in-list acc/muts)]
                           [make-acc/mut (in-list make-acc/muts)])
                  (define raw-acc/mut (if can-impersonate? (gensym acc/mut) acc/mut))
                  (match make-acc/mut
                    [`(make-struct-field-accessor ,(? (lambda (v) (wrap-eq? v -ref))) ,pos ,_)
                     (define raw-def `(define ,raw-acc/mut (record-accessor ,struct:s ,pos)))
                     (if can-impersonate?
                         `(begin
                            ,raw-def
                            (define ,acc/mut
                              (lambda (s) (if (,raw-s? s)
                                              (,raw-acc/mut s)
                                              (pariah (impersonate-ref ,raw-acc/mut ,struct:s ,pos s))))))
                         raw-def)]
                    [`(make-struct-field-mutator ,(? (lambda (v) (wrap-eq? v -set!))) ,pos ,_)
                     (define raw-def `(define ,raw-acc/mut (record-mutator ,struct:s ,pos)))
                     (define abs-pos (+ pos (- (struct-type-info-field-count sti)
                                               (struct-type-info-immediate-field-count sti))))
                     (if can-impersonate?
                         `(begin
                            ,raw-def
                            (define ,acc/mut
                              (lambda (s v) (if (,raw-s? s)
                                                (,raw-acc/mut s v)
                                                (pariah (impersonate-set! ,raw-acc/mut ,struct:s ,pos ,abs-pos s v))))))
                         raw-def)]
                    [`,_ (error "oops")]))
              (define ,(gensym)
                (begin
                  (register-struct-constructor! ,make-s)
                  (register-struct-predicate! ,s?)
                  ,@(for/list ([acc/mut (in-list acc/muts)]
                               [make-acc/mut (in-list make-acc/muts)])
                      (match make-acc/mut
                        [`(make-struct-field-accessor ,_ ,pos ,_)
                         `(register-struct-field-accessor! ,acc/mut ,struct:s ,pos)]
                        [`(make-struct-field-mutator ,_ ,pos ,_)
                         `(register-struct-field-mutator! ,acc/mut ,struct:s ,pos)]
                        [`,_ (error "oops")]))
                  (void))))]
           [else
            (match v
              [`(,_ ,ids ,rhs)
               `(define-values ,ids ,(schemify rhs))])])]
         [`(define-values (,id) ,rhs)
          `(define ,id ,(schemify rhs))]
         [`(define-values ,ids ,rhs)
          `(define-values ,ids ,(schemify rhs))]
         [`(quote ,_) v]
         [`(let-values () ,body)
          (schemify body)]
         [`(let-values () ,bodys ...)
          `(begin ,@(map schemify bodys))]
         [`(let-values ([(,ids) ,rhss] ...) ,bodys ...)
          (define new-knowns
            (for/fold ([knowns knowns]) ([id (in-list ids)]
                                         [rhs (in-list rhss)])
              (if (lambda? rhs)
                  (hash-set knowns (unwrap id) a-known-procedure)
                  knowns)))
          (left-to-right/let ids
                             (for/list ([rhs (in-list rhss)])
                               (schemify rhs))
                             (for/list ([body (in-list bodys)])
                               (schemify/knowns new-knowns body))
                             unannotate prim-knowns knowns imports mutated)]
         [`(let-values ([() (begin ,rhss ... (values))]) ,bodys ...)
          `(begin ,@(map schemify rhss) ,@(map schemify bodys))]
         [`(let-values ([,idss ,rhss] ...) ,bodys ...)
          (left-to-right/let-values idss
                                    (for/list ([rhs (in-list rhss)])
                                      (schemify rhs))
                                    (map schemify bodys)
                                    mutated)]
         [`(letrec-values ([(,ids) ,rhss] ...) ,bodys ...)
          (define new-knowns
            (for/fold ([knowns knowns]) ([id (in-list ids)]
                                         [rhs (in-list rhss)])
              (if (lambda? rhs)
                  (hash-set knowns (unwrap id) a-known-procedure)
                  knowns)))
          `(letrec* ,(for/list ([id (in-list ids)]
                                [rhs (in-list rhss)])
                       `[,id ,(schemify/knowns new-knowns rhs)])
            ,@(for/list ([body (in-list bodys)])
                (schemify/knowns new-knowns body)))]
         [`(letrec-values ([,idss ,rhss] ...) ,bodys ...)
          ;; Convert
          ;;  (letrec*-values ([(id ...) rhs] ...) ....)
          ;; to
          ;;  (letrec* ([vec (call-with-values rhs vector)]
          ;;            [id (vector-ref vec 0)]
          ;;            ... ...)
          ;;    ....)
          `(letrec* ,(apply
                      append
                      (for/list ([ids (in-wrap-list idss)]
                                 [rhs (in-list rhss)])
                        (let ([rhs (schemify rhs)])
                          (cond
                           [(null? ids)
                            `([,(gensym "lr")
                               ,(make-let-values null rhs '(void))])]
                           [(and (pair? ids) (null? (cdr ids)))
                            `([,(car ids) ,rhs])]
                           [else
                            (define lr (gensym "lr"))
                            `([,lr ,(make-let-values ids rhs `(vector . ,ids))]
                              ,@(for/list ([id (in-list ids)]
                                           [pos (in-naturals)])
                                  `[,id (vector-ref ,lr ,pos)]))]))))
            ,@(map schemify bodys))]
         [`(if ,tst ,thn ,els)
          `(if ,(schemify tst) ,(schemify thn) ,(schemify els))]
         [`(with-continuation-mark ,key ,val ,body)
          `(with-continuation-mark ,(schemify key) ,(schemify val) ,(schemify body))]
         [`(begin ,exps ...)
          `(begin . ,(map schemify exps))]
         [`(begin0 ,exps ...)
          `(begin0 . ,(map schemify exps))]
         [`(set! ,id ,rhs)
          (define int-id (unwrap id))
          (define ex-id (hash-ref exports int-id #f))
          (if ex-id
              `(variable-set! ,ex-id ,(schemify rhs) '#f)
              `(set! ,id ,(schemify rhs)))]
         [`(variable-reference-constant? (#%variable-reference ,id))
          (let ([id (unwrap id)])
            (and (not (hash-ref mutated id #f))
                 (let ([im (hash-ref imports id #f)])
                   (or (not im)
                       (known-constant? (import-lookup im))))))]
         [`(#%variable-reference)
          'instance-variable-reference]
         [`(#%variable-reference ,id)
          (define e (hash-ref exports (unwrap id) #f))
          (if e
              `(make-instance-variable-reference 
                instance-variable-reference
                ,e)
              `(make-instance-variable-reference 
                instance-variable-reference
                ',(if (hash-ref mutated (unwrap id) #f)
                      'mutable
                      'immutable)))]
         [`(equal? ,exp1 ,exp2)
          (let ([exp1 (schemify exp1)]
                [exp2 (schemify exp2)])
            (cond
              [(or (equal-implies-eq? exp1) (equal-implies-eq? exp2))
               `(eq? ,exp1 ,exp2)]
              [(or (equal-implies-eqv? exp1) (equal-implies-eqv? exp2))
               `(eqv? ,exp1 ,exp2)]
              [else
               (left-to-right/app 'equal?
                                  (list exp1 exp2)
                                  #t
                                  unannotate prim-knowns knowns imports mutated)]))]
         [`(call-with-values ,generator ,receiver)
          (cond
            [(and (lambda? generator)
                  (lambda? receiver))
             `(call-with-values ,(schemify generator) ,(schemify receiver))]
            [else
             (left-to-right/app '#%call-with-values
                                (list (schemify generator) (schemify receiver))
                                #t
                                unannotate prim-knowns knowns imports mutated)])]
         [`(,rator ,exps ...)
          (let ([s-rator (schemify rator)]
                [args (map schemify exps)]
                [u-rator (unwrap rator)])
            (let ([plain-app?
                   (or (and (known-procedure? (hash-ref-either knowns imports u-rator))
                            (not (hash-ref mutated u-rator #f)))
                       (known-procedure? (hash-ref prim-knowns u-rator #f))
                       (lambda? rator))])
              (left-to-right/app s-rator
                                 args
                                 plain-app?
                                 unannotate prim-knowns knowns imports mutated)))]
         [`,_
          (let ([u-v (unwrap v)])
            (cond
             [(and (symbol? u-v)
                   (via-variable-mutated-state? (hash-ref mutated u-v #f))
                   (hash-ref exports u-v #f))
              => (lambda (ex-id) `(variable-ref ,ex-id))]
             [(and (symbol? u-v)
                   (hash-ref imports u-v #f))
              => (lambda (im)
                   (if (known-constant? (import-lookup im))
                       ;; Not boxed:
                       (import-id im)
                       ;; Will be boxed, but won't be undefined (because the
                       ;; module system won't link to an instance whose
                       ;; definitions didn't complete):
                       `(variable-ref/no-check ,(import-id im))))]
             [else v]))])))))
