#lang racket/base
(require racket/unsafe/undefined
         racket/unsafe/ops
         "match.rkt"
         "wrap.rkt"
         "interp-match.rkt")

;; Interpreter for the output of "jitify". This little interpreter is
;; useful to avoid going through a more heavyweight `eval` or
;; `interpret`, mainly because we don't need to go through a macro
;; expander. Also, because it's tailored to the shape of a linklet
;; outer layer, it can implement that layer more efficiently and
;; compactly.

(provide interpretable-jitified-linklet
         interpret-linklet)

(struct indirect (stack element))
(struct boxed (stack))
(struct boxed/check boxed ())

(struct stack-info ([max-depth #:mutable]
                    capture-depth
                    closure-map))

(define (interpretable-jitified-linklet linklet-e strip-annotations)
  ;; Return a compiled linklet in two parts: a vector expression for
  ;; constants to be run once, and a expression for the linklet body.
  ;; A compiled expression uses a list as a stack for local variables,
  ;; where the coldest element is is a vector of constants, and the
  ;; 1th slot is a vector of linklet arguments for imports and
  ;; exports, and the 2nd slot is a vector for top-level variables. We
  ;; don't have to worry about continuations, because linklet bodies
  ;; are constrained.
  ;;
  ;; Bindings in the environment are represented as positions that
  ;; count from the coldest end of the stack; that position relative
  ;; to the hottest end can be computed from the current stack depth.

  (define (stack->pos i stk-i stack-depth)
    (define capture-depth (stack-info-capture-depth stk-i))
    (cond
      [(not capture-depth) i]
      [(i . >= . capture-depth)
       (- i capture-depth)]
      [(hash-ref (stack-info-closure-map stk-i) i #f)
       => (lambda (pos) pos)]
      [else
       ;; Count backwards from -1 to represent closure elements
       (define cmap (stack-info-closure-map stk-i))
       (define pos (- -1 (hash-count cmap)))
       (hash-set! cmap i pos)
       pos]))

  (define (start linklet-e)
    (match linklet-e
      [`(lambda . ,_)
       ;; No constants:
       (define-values (compiled-body num-body-vars max-depth)
         (compile-linklet-body linklet-e '#hasheq() 0))
       (vector #f
               max-depth
               num-body-vars
               compiled-body)]
      [`(let* ,bindings ,body)
       (define bindings-stk-i (stack-info 1 #f '#hasheq()))
       (let loop ([bindings bindings] [pos 0] [env '#hasheq()] [accum '()])
         (cond
           [(null? bindings)
            (define-values (compiled-body num-body-vars max-depth)
              (compile-linklet-body body env 1))
            (vector (list->vector (reverse accum))
                    (max max-depth (stack-info-max-depth bindings-stk-i))
                    num-body-vars
                    compiled-body)]
           [else
            (let ([binding (car bindings)])
              (loop (cdr bindings)
                    (add1 pos)
                    (hash-set env (car binding) (indirect 0 pos))
                    (cons (compile-expr (cadr binding) env 1 bindings-stk-i)
                          accum)))]))]))

  (define (compile-linklet-body v env stack-depth)
    (match v
      [`(lambda ,args . ,body)
       (define num-args (length args))
       (define args-env
         (for/fold ([env env]) ([arg (in-list args)]
                                [i (in-naturals)])
           (hash-set env arg (+ stack-depth i))))
       (define body-vars-index (+ num-args stack-depth))
       (define-values (body-env num-body-vars)
         (for/fold ([env args-env] [num-body-vars 0]) ([e (in-wrap-list body)])
           (let loop ([e e] [env env] [num-body-vars num-body-vars])
             (match e
               [`(define ,id . ,_)
                (values (hash-set env (unwrap id) (boxed (+ body-vars-index num-body-vars)))
                        (add1 num-body-vars))]
               [`(define-values ,ids . ,_)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([id (in-wrap-list ids)])
                  (values (hash-set env (unwrap id) (boxed (+ body-vars-index num-body-vars)))
                          (add1 num-body-vars)))]
               [`(begin . ,body)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([e (in-wrap-list body)])
                  (loop e env num-body-vars))]
               [`,_ (values env num-body-vars)]))))
       (define body-stack-depth (+ num-body-vars num-args stack-depth))
       (define stk-i (stack-info body-stack-depth #f #hasheq()))
       (define new-body
         (compile-top-body body body-env body-stack-depth stk-i))
       (values new-body
               num-body-vars
               (stack-info-max-depth stk-i))]))

  ;; Like `compile-body`, but flatten top-level `begin`s
  (define (compile-top-body body env stack-depth stk-i)
    (define bs (let loop ([body body])
                 (match body
                   [`() '()]
                   [`((begin ,subs ...) . ,rest)
                    (loop (append subs rest))]
                   [`(,e . ,rest)
                    (cons (compile-expr e env stack-depth stk-i)
                          (loop rest))])))
    (cond
      [(null? bs) '#(void)]
      [(and (pair? bs) (null? (cdr bs)))
       (car bs)]
      [else
       (list->vector (cons 'begin bs))]))

  (define (compile-body body env stack-depth stk-i)
    (match body
      [`(,e) (compile-expr e env stack-depth stk-i)]
      [`,_
       (list->vector
        (cons 'begin
              (for/list ([e (in-wrap-list body)])
                (compile-expr e env stack-depth stk-i))))]))

  (define (compile-expr e env stack-depth stk-i)
    (match e
      [`(lambda ,ids . ,body)
       (define-values (body-env count rest?)
         (args->env ids env stack-depth))
       (define cmap (make-hasheq))
       (define body-stack-depth (+ stack-depth count))
       (define body-stk-i (stack-info body-stack-depth stack-depth cmap))
       (define new-body (compile-body body body-env body-stack-depth body-stk-i))
       (define rev-cmap (for/hasheq ([(i pos) (in-hash cmap)]) (values (- -1 pos) i)))
       (vector 'lambda
               (count->mask count rest?)
               (for/vector #:length (hash-count cmap) ([i (in-range (hash-count cmap))])
                 (stack->pos (hash-ref rev-cmap i) stk-i stack-depth))
               (- (stack-info-max-depth body-stk-i) stack-depth)
               new-body)]
      [`(case-lambda [,idss . ,bodys] ...)
       (define lams (for/list ([ids (in-list idss)]
                               [body (in-list bodys)])
                      (compile-expr `(lambda ,ids . ,body) env stack-depth stk-i)))
       (define mask (for/fold ([mask 0]) ([lam (in-list lams)])
                      (bitwise-ior mask (interp-match lam [#(lambda ,mask) mask]))))
       (list->vector (list* 'case-lambda mask lams))]
      [`(let ([,ids ,rhss] ...) . ,body)
       (define len (length ids))
       (define body-env
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (+ stack-depth i))))
       (define body-stack-depth (+ stack-depth len))
       (max-depth! stk-i body-stack-depth)
       (vector 'let
               (stack->pos stack-depth stk-i stack-depth)
               (for/vector #:length len ([rhs (in-list rhss)])
                 (compile-expr rhs env stack-depth stk-i))
               (compile-body body body-env body-stack-depth stk-i))]
      [`(letrec . ,_) (compile-letrec e env stack-depth stk-i)]
      [`(letrec* . ,_) (compile-letrec e env stack-depth stk-i)]
      [`(begin . ,vs)
       (compile-body vs env stack-depth stk-i)]
      [`(begin0 ,e . ,vs)
       (vector 'begin0
               (compile-expr e env stack-depth stk-i)
               (compile-body vs env stack-depth stk-i))]
      [`(pariah ,e)
       (compile-expr e env stack-depth stk-i)]
      [`(if ,tst ,thn ,els)
       (vector 'if
               (compile-expr tst env stack-depth stk-i)
               (compile-expr thn env stack-depth stk-i)
               (compile-expr els env stack-depth stk-i))]
      [`(with-continuation-mark ,key ,val ,body)
       (vector 'wcm
               (compile-expr key env stack-depth stk-i)
               (compile-expr val env stack-depth stk-i)
               (compile-expr body env stack-depth stk-i))]
      [`(quote ,v)
       (let ([v (strip-annotations v)])
         ;; Protect with `quote` any value that looks like an
         ;; interpreter instruction:
         (if (or (vector? v)
                 (pair? v)
                 (symbol? v)
                 (number? v))
             (vector 'quote v)
             v))]
      [`(set! ,id ,rhs)
       (compile-assignment id rhs env stack-depth stk-i)]
      [`(define ,id ,rhs)
       (compile-assignment id rhs env stack-depth stk-i)]
      [`(define-values ,ids ,rhs)
       (define gen-ids (for/list ([id (in-list ids)])
                         (gensym (unwrap id))))
       (compile-expr `(call-with-values (lambda () ,rhs)
                        (lambda ,gen-ids
                          ,@(if (null? ids)
                                '((void))
                                (for/list ([id (in-list ids)]
                                           [gen-id (in-list gen-ids)])
                                  `(set! ,id ,gen-id)))))
                     env
                     stack-depth
                     stk-i)]
      [`(call-with-values ,proc1 (lambda ,ids . ,body))
       (compile-expr `(call-with-values ,proc1 (case-lambda
                                                 [,ids . ,body]))
                     env
                     stack-depth
                     stk-i)]
      [`(call-with-values (lambda () . ,body) (case-lambda [,idss . ,bodys] ...))
       (vector 'cwv
               (compile-body body env stack-depth stk-i)
               (stack->pos stack-depth stk-i stack-depth)
               (for/list ([ids (in-list idss)]
                          [body (in-list bodys)])
                 (define-values (new-env count rest?)
                   (args->env ids env stack-depth))
                 (define new-stack-depth (+ stack-depth count))
                 (max-depth! stk-i new-stack-depth)
                 (vector (count->mask count rest?)
                         (compile-body body new-env new-stack-depth stk-i))))]
      [`(call-with-module-prompt (lambda () . ,body))
       (vector 'cwmp0 (compile-body body env stack-depth stk-i))]
      [`(call-with-module-prompt (lambda () . ,body) ',ids ',constances ,vars ...)
       (vector 'cwmp
               (compile-body body env stack-depth stk-i)
               ids
               constances
               (for/list ([var (in-list vars)])
                 (compile-expr var env stack-depth stk-i)))]
      [`(variable-set! ,dest-id ,e ',constance)
       (define dest-var (hash-ref env (unwrap dest-id)))
       (vector 'set-variable!
               (stack->pos dest-var stk-i stack-depth)
               (compile-expr e env stack-depth stk-i)
               constance)]
      [`(variable-ref ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable/checked (stack->pos var stk-i stack-depth))]
      [`(variable-ref/no-check ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable (stack->pos var stk-i stack-depth))]
      [`(#%app ,_ ...) (compile-apply (wrap-cdr e) env stack-depth stk-i)]
      [`(,rator ,_ ...)  (compile-apply e env stack-depth stk-i)]
      [`,id
       (define u (unwrap id))
       (define var (hash-ref env u #f))
       (cond
         [(not var)
          (if (number? u)
              (vector 'quote u)
              u)]
         [(indirect? var)
          (define pos (stack->pos (indirect-stack var) stk-i stack-depth))
          (define elem (indirect-element var))
          (cons pos elem)]
         [(boxed? var)
          (define pos (stack->pos (boxed-stack var) stk-i stack-depth))
          (if (boxed/check? var)
              (vector 'unbox/checked pos u)
              (box pos))]
         [else
          (stack->pos var stk-i stack-depth)])]))

  (define (compile-letrec e env stack-depth stk-i)
    (match e
      [`(,_ ([,ids ,rhss] ...) . ,body)
       (define count (length ids))
       (define (make-env boxed)
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (boxed (+ (- count i 1) stack-depth)))))
       (define rhs-env (make-env boxed/check))
       (define body-env (make-env boxed))
       (define body-stack-depth (+ stack-depth count))
       (max-depth! stk-i body-stack-depth)
       (vector 'letrec
               (stack->pos stack-depth stk-i stack-depth)
               (for/vector #:length (length ids) ([rhs (in-list rhss)])
                 (compile-expr rhs rhs-env body-stack-depth stk-i))
               (compile-body body body-env body-stack-depth stk-i))]))

  (define (compile-apply es env stack-depth stk-i)
    (list->vector (cons 'app
                        (for/list ([e (in-wrap-list es)])
                          (compile-expr e env stack-depth stk-i)))))
  
  (define (compile-assignment id rhs env stack-depth stk-i)
    (define compiled-rhs (compile-expr rhs env stack-depth stk-i))
    (define u (unwrap id))
    (define var (hash-ref env u))
    (cond
      [(indirect? var)
       (define s (stack->pos (indirect-stack var) stk-i stack-depth))
       (define e (indirect-element var))
       (vector 'set!-indirect s e compiled-rhs)]
      [(boxed? var)
       (define s (stack->pos (boxed-stack var) stk-i stack-depth))
       (if (boxed/check? var)
           (vector 'set!-boxed/checked s compiled-rhs u)
           (vector 'set!-boxed s compiled-rhs))]
      [else (error 'compile "unexpected set!")]))

  (define (args->env ids env stack-depth)
    (let loop ([ids ids] [env env] [count 0])
      (cond
        [(wrap-null? ids) (values env count #f)]
        [(wrap-pair? ids) (loop (wrap-cdr ids)
                                (hash-set env (unwrap (wrap-car ids)) (+ stack-depth count))
                                (add1 count))]
        [else
         (values (hash-set env (unwrap ids) (+ stack-depth count))
                 (add1 count)
                 #t)])))

  (define (max-depth! stk-i new-stack-depth)
    (set-stack-info-max-depth! stk-i (max (stack-info-max-depth stk-i) new-stack-depth)))

  (start linklet-e))

;; ----------------------------------------

(define (interpret-linklet b primitives variable-ref variable-ref/no-check variable-set!
                           make-arity-wrapper-procedure)
  (interp-match
   b
   [#(,consts ,max-depth ,num-body-vars ,b)
    (let ([consts (and consts
                       (let ([vec (make-vector (vector-length consts))])
                         (define stack (list vec))
                         (for ([b (in-vector consts)]
                               [i (in-naturals)])
                           (vector-set! vec i (interpret-expr b stack primitives void void void void))
                           vec)
                         vec))])
      (lambda args
        (define start-stack (if consts
                                (hasheq 0 consts)
                                #hasheq()))
        (define args-stack (for/fold ([stack start-stack]) ([arg (in-list args)]
                                                            [i (in-naturals (if consts 1 0))])
                             (hash-set stack i arg)))
        (define post-args-pos (hash-count args-stack))
        (define stack (for/fold ([stack args-stack]) ([i (in-range num-body-vars)])
                        (hash-set stack (+ i post-args-pos) (box unsafe-undefined))))
        (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                        make-arity-wrapper-procedure)))]))

(define (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                        make-arity-wrapper-procedure)

  (define (stack-ref stack i)
    (hash-ref stack i))

  (define (stack-set stack i v)
    (hash-set stack i v))
  
  (define (interpret b stack)
    (cond
      [(integer? b) (stack-ref stack b)]
      [(box? b) (unbox* (stack-ref stack (unbox* b)))]
      [(pair? b) (vector*-ref (stack-ref stack (car b)) (cdr b))]
      [(symbol? b) (hash-ref primitives b)]
      [(vector? b)
       (interp-match
        b
        [#(app ,rator-b)
         (define len (vector-length b))
         (define rator (interpret rator-b stack))
         (cond
           [(eq? len 2)
            (rator)]
           [(eq? len 3)
            (rator
             (interpret (vector*-ref b 2) stack))]
           [(eq? len 4)
            (rator
             (interpret (vector*-ref b 2) stack)
             (interpret (vector*-ref b 3) stack))]
           [else
            (apply (interpret rator-b stack)
                   (for/list ([b (in-vector b 2)])
                     (interpret b stack)))])]
        [#(quote ,v) v]
        [#(unbox/checked ,s ,name)
         (define v (unbox* (stack-ref stack s)))
         (check-not-unsafe-undefined v name)]
        [#(ref-variable ,s)
         (variable-ref/no-check (stack-ref stack s))]
        [#(ref-variable/checked ,s)
         (variable-ref (stack-ref stack s))]
        [#(let ,pos ,rhss ,b)
         (define len (vector-length rhss))
         (define body-stack
           (let loop ([i 0] [body-stack stack])
             (cond
               [(= i len) body-stack]
               [else
                (define val (interpret (vector*-ref rhss i) stack))
                (loop (add1 i)
                      (stack-set body-stack (+ i pos) val))])))
         (interpret b body-stack)]
        [#(letrec ,pos ,rhss ,b)
         (define len (vector-length rhss))
         (define body-stack
           (for/fold ([stack stack]) ([i (in-range len)])
             (stack-set stack (+ i pos) (box unsafe-undefined))))
         (let loop ([i 0])
           (if (= i len)
               (interpret b body-stack)
               (begin
                 (set-box! (stack-ref body-stack (+ pos i))
                           (interpret (vector-ref rhss i) stack))
                 (loop (add1 i)))))]
        [#(begin)
         (define last (sub1 (vector-length b)))
         (let loop ([i 1])
           (if (= i last)
               (interpret (vector*-ref b i) stack)
               (begin
                 (interpret (vector*-ref b i) stack)
                 (loop (add1 i)))))]
        [#(begin0 ,b0)
         (define last (sub1 (vector-length b)))
         (begin0
           (interpret b0 stack)
           (let loop ([i 2])
             (interpret (vector*-ref b i) stack)
             (unless (= i last)
               (loop (add1 i)))))]
        [#(if ,tst ,thn ,els)
         (if (interpret tst stack)
             (interpret thn stack)
             (interpret els stack))]
        [#(wcm ,key ,val ,body)
         (with-continuation-mark
          (interpret key stack)
          (interpret val stack)
          (interpret body stack))]
        [#(cwv ,b ,pos ,clauses)
         (define vs (call-with-values (lambda () (interpret b stack)) list))
         (define len (length vs))
         (let loop ([clauses clauses])
           (cond
             [(null? clauses) (error 'call-with-values "arity error")]
             [else
              (interp-match
               (car clauses)
               [#(,mask ,b)
                (if (matching-argument-count? mask len)
                    (interpret b (push-stack stack pos vs mask))
                    (loop (cdr clauses)))])]))]
        [#(cwmp0 ,b)
         ((hash-ref primitives 'call-with-module-prompt)
          (lambda () (interpret b stack)))]
        [#(cwmp ,b ,ids ,constances ,var-es)
         (apply (hash-ref primitives 'call-with-module-prompt)
                (lambda () (interpret b stack))
                ids
                constances
                (for/list ([e (in-list var-es)])
                  (interpret e stack)))]
        [#(lambda ,mask ,close-vec ,max-stack-depth ,_)
         (define captured (capture-closure close-vec stack))
         (make-arity-wrapper-procedure
          (lambda args
            (cond
              [(matching-argument-count? mask (length args))
               (apply-function b captured args)]
              [else
               (error "arity error")]))
          mask
          #f)]
        [#(case-lambda ,mask)
         (define n (vector-length b))
         (define captureds
           (for/list ([one-b (in-vector b 2)])
             (interp-match
              one-b
              [#(lambda ,_ ,close-vec) (capture-closure close-vec stack)])))
         (make-arity-wrapper-procedure
          (lambda args
            (define len (length args))
            (let loop ([i 2] [captureds captureds])
              (cond
                [(= i n) (error "arity error")]
                [else
                 (define one-b (vector*-ref b i))
                 (interp-match
                  one-b
                  [#(lambda ,mask ,b)
                   (if (matching-argument-count? mask len)
                       (apply-function one-b (car captureds) args)
                       (loop (add1 i) (cdr captureds)))])])))
          mask
          #f)]
        [#(set-variable! ,s ,b ,c)
         (variable-set! (stack-ref stack s)
                        (interpret b stack)
                        c)]
        [#(set!-indirect ,s ,e ,b)
         (vector*-set! (stack-ref stack s) e (interpret b stack))]
        [#(set!-boxed ,s ,b ,name)
         (define v (interpret b stack))
         (define bx (stack-ref stack s))
         (set-box*! bx v)]
        [#(set!-boxed/checked ,s ,b ,name)
         (define v (interpret b stack))
         (define bx (stack-ref stack s))
         (check-not-unsafe-undefined/assign (unbox* bx) name)
         (set-box*! bx v)])]
      [else b]))

  (define (capture-closure close-vec stack)
    (for/hasheq ([s (in-vector close-vec)]
                 [i (in-naturals)])
      (values (- -1 i) (stack-ref stack s))))

  (define (apply-function b captured args)
    (interp-match
     b
     [#(lambda ,mask ,close-vec ,max-stack-depth ,b)
      (interpret b (push-stack captured 0 args mask))]))

  (define (matching-argument-count? mask len)
    (bitwise-bit-set? mask len))

  (define (push-stack stack pos vals mask)
    (define rest? (negative? mask))
    (define count (if rest?
                      (integer-length mask)
                      (sub1 (integer-length mask))))
    (let loop ([pos pos] [vals vals] [count count] [stack stack])
      (cond
        [(zero? count)
         (if rest?
             (stack-set stack pos vals)
             stack)]
        [else
         (loop (add1 pos) (cdr vals) (sub1 count)
               (stack-set stack pos (car vals)))])))

  (interpret b stack))


(define (count->mask count rest?)
  (if rest?
      (bitwise-xor -1 (sub1 (arithmetic-shift 1 (sub1 count))))
      (arithmetic-shift 1 count)))

;; ----------------------------------------

(module+ main
  (define primitives (hash 'list list
                           'vector vector
                           'add1 add1
                           'values values
                           'continuation-mark-set-first continuation-mark-set-first))
  (struct var ([val #:mutable]) #:transparent)
  (define b
    (interpretable-jitified-linklet '(let* ([s "string"])
                                       (lambda (x two-box)
                                         (define other 5)
                                         (begin
                                           (define f (lambda (y)
                                                       (let ([z y])
                                                         (vector x z))))
                                           (define g (case-lambda
                                                       [() no]
                                                       [ys
                                                        (vector x ys)])))
                                         (define-values (one two) (values 100 200))
                                         (variable-set! two-box two 'constant)
                                         (letrec ([ok 'ok])
                                           (set! other (call-with-values (lambda () (values 71 (begin0 88 ok)))
                                                         (lambda (v q) (list q v))))
                                           (with-continuation-mark
                                            'x 'cm/x
                                            (list (if s s #f) x ok other
                                                  (f 'vec) (g 'also-vec 'more)
                                                  one two (variable-ref two-box)
                                                  (continuation-mark-set-first #f 'x 'no))))))
                                    values))
  (define l (interpret-linklet b primitives var-val var-val (lambda (b v c)
                                                              (set-var-val! b v))
                               (lambda (proc mask name) proc)))
  (l 'the-x (var #f)))
