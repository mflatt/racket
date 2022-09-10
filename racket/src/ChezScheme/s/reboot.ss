(define xc-dir
  (let ([l (command-line-arguments)])
    (if (= 1 (length l))
        (car l)
        (error 'reboot "expected <target-dir> as only argument"))))

(let ([machine.def (path-build xc-dir "machine.def")])
  (unless (file-exists? machine.def)
    (error 'reboot "~a not found" machine.def)))

(source-directories (list xc-dir "s" "unicode"))
(library-directories '(("." . ".") ("nanopass" . "nanopass")))

(define (status s)
  (printf "~a\n" s)
  (flush-output-port))

(define-values (base-srcs compiler-srcs)
  (call-with-input-file
   "s/build.zuo"
   (lambda (i)
     (unless (equal? (get-line i) "#lang zuo")
       (error 'srcs "expected `#lang zuo`from build.zuo"))
     (let ([content (let loop ()
                      (let ([v (read i)])
                        (if (eof-object? v)
                            '()
                            (cons v (loop)))))])
       (define (extract-list id)
         (let loop ([c content])
           (cond
             [(and (list? c)
                   (= 3 (length c))
                   (eq? (car c) 'define)
                   (eq? (cadr c) id))
              (let ([v (caddr c)])
                (cond
                  [(and (list? v) (eq? 'list (car v)))
                   (cdr v)]
                  [else (error "definition did not have the expected right-hand side" v)]))]
             [(list? c)
              (ormap loop c)]
             [else #f])))
       (values (extract-list 'base-src-names)
               (extract-list 'compiler-names))))))

(print-graph #t)

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
      (putprop sym 'reboot (filter
                            (lambda (p)
                              (not (eq? (car p) key)))
                            (getprop sym 'reboot '()))))))

(define-primitive ($intern3 s x y)
  (#%$intern3 s x y))

(define-primitive ($undefined-violation id msg)
  (error (syntax->datum id) msg))

(define-primitive $exactnum-real-part real-part)
(define-primitive $exactnum-imag-part imag-part)
(define-primitive $ratio-numerator numerator)
(define-primitive $ratio-denominator denominator)

(define-primitive ($inexactnum? x)
  (and (complex? x)
       (not (real? x))
       (inexact? x)))

(define-primitive ($exactnum? x)
  (and (complex? x)
       (not (real? x))
       (exact? x)))

; (define-primitive $make-thread-parameter make-thread-parameter)

(define-primitive $enum-set-members #%$enum-set-members)
(define-primitive $make-file-options #%$make-file-options)
(define-primitive $file-options #%$file-options)
(define-primitive $eol-style? #%$eol-style?)
(define-primitive $error-handling-mode? #%$error-handling-mode?)
(define-primitive $open-file-output-port #%$open-file-output-port)
(define-primitive $open-file-input-port #%$open-file-input-port)
(define-primitive $open-bytevector-list-output-port #%$open-bytevector-list-output-port)
(define-primitive $format-scheme-version #%$format-scheme-version)
(define-primitive $fasl-strip-options #%$fasl-strip-options)
(define-primitive $make-fasl-strip-options #%$make-fasl-strip-options)

(define-primitive $immediate? #%$immediate?)
(define-primitive $flonum->digits #%$flonum->digits)
(define-primitive $flonum-sign #%$flonum-sign)
(define-primitive $integer-32? #%$integer-32?)
(define-primitive $integer-64? #%$integer-64?)
(define-primitive $fxu< #%$fxu<)
(define-primitive $stencil-vector? #%$stencil-vector?)
(define-primitive $system-stencil-vector? #%$system-stencil-vector?)
(define-primitive $symbol-name #%$symbol-name)

(define-primitive $char-grapheme-other-state #%$char-grapheme-other-state)

(define-primitive $ht-minlen #%$ht-minlen)
(define-primitive $ht-veclen #%$ht-veclen)

(define-primitive $rtd-counts? #%$rtd-counts?)

(define-primitive $record #%$record)
(define-primitive $record? #%$record?)
(define-primitive $record-type-descriptor #%$record-type-descriptor)
(define-primitive $make-record-type-descriptor* #%$make-record-type-descriptor*)
(define-primitive $make-record-constructor-descriptor #%$make-record-constructor-descriptor)
(define-primitive $record-type-field-indices #%$record-type-field-indices)
(define-primitive $object-ref #%$object-ref)
(define-primitive $sealed-record? #%$sealed-record?)

(define-primitive $thread-list #%$thread-list)

(define-primitive $c-bufsiz #%$c-bufsiz)

(define-primitive $separator-character #%$separator-character)

(define-primitive $filter-foreign-type #%$filter-foreign-type)

(define-primitive $set-collect-trip-bytes #%$set-collect-trip-bytes)

  (define-syntax $lambda/lift-barrier
    (syntax-rules ()
      [(_ fmls body ...) (lambda fmls body ...)]))

(define-primitive $fasl-target (make-parameter #f))
(define-primitive $current-mso (make-parameter #f))
(define-primitive $block-counter (make-parameter 0))
(define-primitive $sfd (make-parameter #f))
(define-primitive $target-machine (make-parameter #f))
(define-primitive $compile-profile (make-parameter #f))
(define-primitive $optimize-closures (make-parameter #t))
(define-primitive $track-dynamic-closure-counts (make-parameter #f))

(define-primitive $the-unbound-object (gensym "unbound"))
(define-primitive ($unbound-object) $the-unbound-object)
(define-primitive ($unbound-object? v)
  (eq? v $the-unbound-object))

(define-primitive $top-level-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (if (orig-top-level-bound? s primitive-environment)
          (orig-top-level-value s primitive-environment)
          (begin
            (unless (eq? s '$capture-fasl-target)
              (printf "  [unbound: ~s]\n" s))
            ($unbound-object))))))
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
(define-primitive ($profile-block-data?) #f)
(define-primitive ($suppress-primitive-inlining) #f)

(define-primitive $reset-protect #%$reset-protect)
(define-primitive $pass-time
  (lambda (name thunk)
    (thunk)))
(define-primitive $guard #%$guard)

(define orig-$uncprep #%$uncprep)

(define-primitive $primitive-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (if (orig-top-level-bound? s primitive-environment)
          (orig-top-level-value s primitive-environment)
          (begin
            (printf "UNBOUND PRIMITIVE ~s\n" s)
            ($unbound-object))))))

;; Make `$primitive` access top-level variables:
(define-syntax $primitive
  (let ([orig-top-level-bound? top-level-bound?])
    (lambda (stx)
      (define (top id)
        (if (orig-top-level-bound? (syntax->datum id) primitive-environment)
            ;; This works as long as primitives are never locally shadowed,
            ;; (which won't be the case for expanded code, at least):
            id
            ;; If it's not yet there, defer the lookup, and maybe we
            ;; won't have to fill in the primitive:
            #`($primitive-value (quote #,id))))
      (syntax-case stx ()
        [(_ id) (top #'id)]
        [(_ level id) (top #'id)]))))

(define-syntax ($foreign-procedure stx)
  (syntax-case stx ()
    [(_ _ name . _)
     (printf " [assuming no call to foreign function ~s]\n" (syntax->datum #'name))
     #'name]))

(define-primitive ($oops . args)
  (apply error args))

(define-primitive ($make-source-oops who . args)
  (($top-level-value 'datum->syntax) (or who ($make-interaction-syntax 'unknown))
                                     '(error "oops")))

(define-primitive ($source-warning . args)
  (printf "~s\n" args))

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

($target-machine (constant machine-type-name))
(status (format ">> Target machine: ~a" ($target-machine)))

(define out-dir (path-build "boot" (symbol->string ($target-machine))))
(unless (file-directory? out-dir)
  (mkdir out-dir))

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

(current-eval compile)

(status "== Install new expander")

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

;; Loading after the expander means that `%uncprep` sees the new
;; syntax-object constructors
(noisy-load "cprep.ss")

;; Expander with the new expander, but the interpreter still uses the language
;; of the old expander.
(define (eval-with-expand s mode)
  (let ([e (parameterize ([$current-expand
                           (let ([orig ($current-expand)])
                             (lambda (e . args)
                               (let ([e ($uncprep (cadr e))])
                                 #;(printf "ct ~s\n" e)
                                 (let ([r (apply orig e args)])
                                   #;(printf "ct out ~s\n" (orig-$uncprep r))
                                   r))))])
             (sc-expand s (if (or (eq? mode 'system)
                                  (and (eq? mode 'system-macros)
                                       (pair? s)
                                       (eq? (car s) 'define-syntax)))
                              ;; Define macros in the system environment
                              ($system-environment)
                              (interaction-environment))))])
    #;(printf "=> ~s\n" e)
    (let ([r (eval e)])
      #;(printf "= ~s\n" r)
      r)))

(define (evalx s)
  (eval-with-expand s 'system-macros))

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

(define (expand-and-load s mode)
  (status (format "Loading ~a" s))
  (for-each (lambda (e)
              #;(printf "~s\n" e)
              (eval-with-expand e mode))
            (file->exps s)))

(status "== Setup for using expander")
(expand-and-load "s/cmacros.ss" 'system)
(expand-and-load "s/priminfo.ss" 'system)
(expand-and-load "s/primvars.ss" 'system)

;; Need just `$compiled-file-header?` from "7.ss":
(for-each (lambda (e)
            (let loop ([e e])
              (cond
                [(and (pair? e)
                      (eq? (car e) 'define)
                      (or (eq? (cadr e) '$compiled-file-header?)
                          (and (pair? (cadr e))
                               (eq? (caadr e) '$compiled-file-header?))))
                 (status "Loading part of s/7.ss")
                 ($set-top-level-value! '$compiled-file-header?
                                        (eval-with-expand (if (pair? (ca<dr e))
                                                              `(lambda ,(cdadr e) . ,(cddr e))
                                                              (caddr e))
                                                          'system))]
                [(and (pair? e)
                      (eq? 'begin (car e)))
                 (for-each loop (cdr e))])))
          (file->exps "s/7.ss"))

(status "== Load nanopass using expander")
(define (load-nanopass)
  (define (load-nano s)
    (expand-and-load (path-build "nanopass" s) #f))
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

(status "== Load compiler")
(for-each (lambda (s)
            (expand-and-load (path-build "s" s) 'system))
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

(status "== Compile bootfiles")
(status " [At this point, `compile-file` is from the loaded compiler]")

(fasl-compressed #f)
(enable-type-recovery #f)

(current-expand (lambda args
                  (parameterize ([$current-expand
                                  (let ([orig ($current-expand)])
                                    (lambda (e . args)
                                      (cond
                                        [(and (pair? e)
                                              (equal? "noexpand" (car e)))
                                         (let ([e ($uncprep (cadr e))])
                                           #;(printf "ct ~s\n" e)
                                           (let ([r (apply orig e args)])
                                             #;(printf "ct out ~s\n" (orig-$uncprep r))
                                             r))]
                                        [(equal? e '($target-machine))
                                         (apply orig e args)]
                                        [else
                                         (error 'expand "unexpected nesting: ~s" e)])))])
                  (apply sc-expand args))))

;; Define `expand` to use our `current-expand`
(define expand
  (case-lambda
    [(x) ((current-expand) x)]
    [(x env-spec) ((current-expand) x env-spec)]
    [(x env-spec records?) ((current-expand) x env-spec records?)]
    [(x env-spec records? compiling-a-file) ((current-expand) x env-spec records? compiling-a-file)]
    [(x env-spec records? compiling-a-file outfn) ((current-expand) x env-spec records? compiling-a-file outfn)]))

(define (compile-s-file s)
  (let ([start (cpu-time)])
    (compile-file (path-build "s" s)
                  (path-build xc-dir (string-append (path-root s) ".so")))
    (status (format "  ~a secs cpu time" (/ (- (cpu-time) start) 1000.0)))))

(for-each compile-s-file
          base-srcs)
(for-each compile-s-file
          compiler-srcs)

(let* ([src->so (lambda (src) (path-build xc-dir (string-append (path-root src) ".so")))])
  (status (format "Writing ~a/petite.boot" out-dir))
  (apply $make-boot-file (path-build out-dir "petite.boot")
         ($target-machine) '()
         (map src->so base-srcs))
  (status (format "Writing ~a/scheme.boot" out-dir))
  (apply $make-boot-file (path-build out-dir "scheme.boot")
         ($target-machine) '("petite")
         (map src->so compiler-srcs)))
