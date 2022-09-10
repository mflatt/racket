(source-directories '("/Users/mflatt/plt/racket/src/build/cs/c/ChezScheme/tarm64osx/s" "s"))
(library-directories '(("." . ".") ("nanopass" . "nanopass")))

(define out-dir "/tmp/")

(define (status s)
  (printf "~a\n" s)
  (flush-output-port))

(define primitive-environment (interaction-environment))
(define primitives '())
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (id . args) . body)
     (begin
       (set! primitives (cons 'id primitives))
       (define (id . args) . body))]
    [(_ id rhs)
     (begin
       (set! primitives (cons 'id primitives))
       (define id rhs))]))

(define-primitive ($make-record-type base-rtd parent name fields sealed? opaque? . extras)
  (apply #%$make-record-type base-rtd parent name fields sealed? opaque? extras))
(define-primitive ($make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields . extras)
  (apply #%$make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields extras))
(define-primitive ($make-record-constructor-descriptor rts parent protocol name)
  (#%$make-record-constructor-descriptor rts parent protocol name))

(define-primitive ($sputprop sym key val)
  (putprop sym 'reboot (cons (cons key val) (getprop sym 'reboot '()))))

(define-primitive ($sgetprop sym key def-val)
  (let ([a (assq key (getprop sym 'reboot '()))])
    (if a
        (cdr a)
        def-val)))

(define-primitive ($sremprop sym key)
  (let ([a (assq key (getprop sym 'reboot '()))])
    (when a
      (error '$sremprop "remove not imlemented"))))

(define-primitive ($intern3 s x y)
  (#%$intern3 s x y))

(define-primitive ($undefined-violation id msg)
  (error (syntax->datum id) msg))

(define-primitive $enum-set-members
  #%$enum-set-members)

(define-primitive $fasl-target (make-parameter #f))

(define-primitive $unbound-object (gensym "unbound"))
(define-primitive ($unbound-object? x)
  (eq? x $unbound-object))

(define-primitive $top-level-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (if (orig-top-level-bound? s primitive-environment)
          (orig-top-level-value s primitive-environment)
          $unbound-object))))
(define-primitive $set-top-level-value!
  (let ([top-level-bound? top-level-bound?]
        [set-top-level-value! set-top-level-value!]
        [define-top-level-value define-top-level-value])
    (lambda (sym val)
      (if (top-level-bound? sym  primitive-environment)
          (set-top-level-value! sym val  primitive-environment)
          (define-top-level-value sym val primitive-environment)))))

(define $tc-mutex (make-mutex))

(define tc-table (make-eq-hashtable))
(define ($tc) tc-table)
(define $tc-field
  (case-lambda
   [(sym table) (hashtable-ref table sym 0)]
   [(sym table val) (hashtable-set! table sym val)]))

(define-primitive ($profile-source-data?) #f)
(define-primitive ($suppress-primitive-inlining) #f)

(define-primitive $reset-protect #%$reset-protect)
(define-primitive $pass-time
  (lambda (name thunk)
    (thunk)))
(define-primitive $guard #%$guard)

(define-primitive $primitive-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (if (orig-top-level-bound? s primitive-environment)
          (orig-top-level-value s primitive-environment)
          (begin
            (printf "UNBOUND PRIMITIVE ~s\n" s)
            $unbound-object)))))

;; Make `$primitive` access top-level variables:
(define-syntax ($primitive stx)
  (define (top id) #`($primitive-value (quote #,id)))
  (syntax-case stx ()
    [(_ id) (top #'id)]
    [(_ level id) (top #'id)]))

(define-primitive ($oops . args)
  (apply error args))

(define-primitive ($make-source-oops who . args)
  (($top-level-value 'datum->syntax) (or who ($make-interaction-syntax 'unknown))
                                     '(error "oops")))

(define-primitive ($source-warning . args)
  (printf "~s\n" args))

(define-primitive ($compile-profile) #f)

(define-primitive ($open-file-input-port who fn)
  (open-file-input-port fn))

(define-primitive ($source-file-descriptor fn p)
  fn)

(define-primitive ($make-read p . more)
  (lambda ()
    (read p)))

(define-primitive ($map who f . ls)
  (apply map f ls))

(define-primitive subset-mode
  (case-lambda
   [(mode) (unless (eq? mode 'system) (error 'subset-mode "always must be system mode"))]
   [() 'system]))

(define $current-expand current-expand)
(define current-expand (make-parameter #f))

(define (noisy-load s)
  (status (format "Loading ~a" s))
  (load s))

(define (file->exps s)
  (call-with-input-file
   s
   (lambda (i)
     (let loop ()
       (define e (read i))
       (if (eof-object? e)
           '()
           (cons e (loop)))))))

(define (noisy-compile-and-load s)
  (status (format "Loading ~a" s))
  (let-values ([(p get) (open-bytevector-output-port)])
    (compile-to-port (file->exps (path-build "s" s)) p)
    (load-compiled-from-port (open-bytevector-input-port (get)))))

(for-each noisy-compile-and-load
          '("cmacros.ss" "priminfo.ss" "primvars.ss"))

(noisy-load "mkheader.ss")
(status "== Generate headers")
(mkscheme.h (path-build out-dir "scheme.h") (constant machine-type-name))
(mkequates.h (path-build out-dir "equates.h"))

(noisy-load "mkgc.ss")
(status "== Generate GC traversals")
(mkgc-ocd.inc (path-build out-dir "gc-ocd.inc"))
(mkgc-oce.inc (path-build out-dir "gc-oce.inc"))
(mkgc-par.inc (path-build out-dir "gc-par.inc"))
(mkheapcheck.inc (path-build out-dir "heapcheck.inc"))

(status "== Load nanopass")
(load "./nanopass/nanopass.ss")

(status "== Make variables mutable")
(for-each (lambda (sym)
            (when (top-level-bound? sym)
              (unless (top-level-mutable? sym)
                (eval `(define ,sym ',(top-level-value sym))))))
          (environment-symbols (interaction-environment)))

;; These will be refined by the macro expander, but are needed
;; for "setup.ss" to run:
(define ($make-base-modules) (void))
(define ($make-rnrs-libraries) (void))

(for-each noisy-load
          '("setup.ss" "env.ss"))

(status "== Install new expander")
(noisy-load "cprep.ss")

(define ($make-system-syntax datum)
  ($datum->environment-syntax datum ($system-environment)))

(define ($make-interaction-syntax datum)
  (cond
    [(symbol? datum)
     ($datum->environment-syntax datum ($system-environment))]
    [(pair? datum)
     (cons ($make-interaction-syntax (car datum))
           ($make-interaction-syntax (cdr datum)))]
    [(vector? datum)
     (vector-map $make-interaction-syntax datum)]
    [else datum]))

(define syntax-object? (record-predicate (record-rtd (syntax x))))
(define orig-identifier? identifier?)
(define orig-free-identifier=? free-identifier=?)
(define orig-datum->syntax datum->syntax)
(define orig-syntax->datum syntax->datum)

(define (expand-to-non-syntax/system s)
  (define (requote-syntax s)
    (cond
      [(pair? s)
       (if (and (eq? (car s) 'quote)
                (pair? (cdr s))
                (syntax-object? (cadr s)))
           `($make-system-syntax ',(syntax->datum (cadr s)))
           (cons (requote-syntax (car s))
                 (requote-syntax (cdr s))))]
      [else s]))
  (requote-syntax (expand s primitive-environment)))

;; first form is the expander's implementation:
(status "Load expander implementation")
(eval (expand-to-non-syntax/system
       (call-with-input-file
        "s/syntax.ss"
        (lambda (in)
          (read in)))))

(define top-wrapped
  (car (generate-temporaries '(x))))

;; initialize the just-loaded expander
(define (init-syntax-libraries)
  ($make-base-modules)
  ($make-rnrs-libraries))
(init-syntax-libraries)
;; Forward reference of sorts:
(set! $annotation-options (make-enumeration '(debug profile)))
(set! $make-annotation-options (enum-set-constructor $annotation-options))

(define (requote-syntax s)
  (cond
    [(pair? s)
     (cons (requote-syntax (car s))
           (requote-syntax (cdr s)))]
    [(vector? s)
     (vector-map requote-syntax s)]
    [(syntax-object? s)
     ($make-interaction-syntax (orig-syntax->datum s))]
    [else s]))

;; Expander with the new expander, but the interpreter still uses the language
;; of the old expander.
(define (eval-with-expand s primitive?)
  (let ([e (parameterize ([$current-expand
                           (let ([orig ($current-expand)])
                             (lambda (e . args)
                               (let ([e (requote-syntax ($uncprep (cadr e)))])
                                 #;(printf "recur ~s\n" e)
                                 (apply orig e args))))])
             (sc-expand s (if (and primitive?
                                   (pair? s)
                                   (eq? (car s) 'define-syntax))
                              ;; Define macros in the system environment
                              ($system-environment)
                              (interaction-environment))))])
    #;(printf "=> ~s\n" (expand e))
    (let ([r (eval e)])
      #;(printf "= ~s\n" r)
      r)))

(define (evalx s)
  (eval-with-expand s #t))

(for-each (lambda (sym)
            (let ([val ($top-level-value sym)])
              (evalx `(define ,sym ',val))
              val))            
          primitives)

;; Forward reference
(evalx `(define $syntax-match? #f))

;; Expand the implementations of macro using the host Scheme,
;; so that macro implementations can use macros that are not
;; yet defined. This constrains the implementation of macros
;; defined in "syntax.ss" to use only constructs in the host
;; Scheme implementation.
(define (expand-to-non-syntax s)
  #;(pretty-print s)
  (requote-syntax (fluid-let ([identifier? orig-identifier?]
                              [free-identifier=? orig-free-identifier=?]
                              [datum->syntax orig-datum->syntax]
                              [syntax->datum orig-syntax->datum])
                    (expand s primitive-environment))))

(define (evalxm s)
  (let ([rhs (expand-to-non-syntax (caddr s))])
    (evalx `(,(car s) ,(cadr s)
                      ;; `values` wrapper avoids deferring evaluation:
                      (values ,rhs)))))

(define (evale s)
  (let loop ([s s])
    (cond
      [(and (pair? s)
            (eq? 'begin (car s)))
       (for-each loop (cdr s))]
      [(and (pair? s)
            (eq? 'define-syntax (car s)))
       (evalxm s)]
      [(and (pair? s)
            (eq? 'define (car s)))
       (evalxm s)]
      [(and (pair? s)
            (eq? 'when-feature (car s)))
       (when (equal? "yes" (expand-to-non-syntax `(when-feature ,(cadr s) "yes")))
         (evalxm (caddr s)))]
      [else
       (evalx (expand-to-non-syntax s))])))

;; Load the macro implementations from "syntax.ss", which is
;; everything after the expander's implementation
(status "Load expander macros")
(for-each evale
          (cddr (file->exps (path-build "s" "syntax.ss"))))

;; Not defined in "syntax.ss", but needed to load nanopass:
(evale '(define-syntax guard
          (syntax-rules (else)
            [(_ (var clause ... [else e1 e2 ...]) b1 b2 ...)
             ($guard #f (lambda (var) (cond clause ... [else e1 e2 ...]))
                     (lambda () b1 b2 ...))]
            [(_ (var clause1 clause2 ...) b1 b2 ...)
             ($guard #t (lambda (var p) (cond clause1 clause2 ... [else (p)]))
                     (lambda () b1 b2 ...))])))

(define (evaly s)
  (eval-with-expand s #f))

(define (expand-and-load s)
  (status (format "Loading ~a" s))
  (map evaly
       (file->exps s)))

(status "== Load nanopass using expander")
(define (load-nanopass)
  (define (load-nano s)
    (expand-and-load (path-build "nanopass" s)))
  (load-nano "nanopass/implementation-helpers.chezscheme.sls")
  (load-nano "nanopass/helpers.ss")
  (load-nano "nanopass/syntaxconvert.ss")
  (load-nano "nanopass/records.ss")
  (load-nano "nanopass/nano-syntax-dispatch.ss")
  (load-nano "nanopass/parser.ss")
  (load-nano "nanopass/unparser.ss")
  (load-nano "nanopass/meta-syntax-dispatch.ss")
  (load-nano "nanopass/meta-parser.ss")
  (load-nano "nanopass/pass.ss")
  (load-nano "nanopass/language-node-counter.ss")
  (load-nano "nanopass/language-helpers.ss")
  (load-nano "nanopass/language.ss")
  (load-nano "nanopass.ss"))
(load-nanopass)

(status "== Load priminfo and primvars")
(expand-and-load "s/priminfo.ss")
(expand-and-load "s/primvars.ss")

(status "== Load expander using expander")
(expand-and-load "s/syntax.ss")
(init-syntax-libraries)

(status "== Declare nanopass in bootstrapped expander")
(load-nanopass)

(for-each (lambda (s)
            (expand-and-load (path-build "s" s)))
          '("ftype.ss"
            "fasl.ss"
            "reloc.ss"
            "format.ss"
            "cp0.ss"
            "cpvalid.ss"
            "cpcheck.ss"
            "cpletrec.ss"
            "cpcommonize.ss"
            "cpnanopass.ss"
            "cpprim.ss"
            "compile.ss"
            "back.ss"))
