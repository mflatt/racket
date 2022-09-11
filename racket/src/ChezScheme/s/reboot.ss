;; This script automates bootstrapping of the latest implementation of
;; Chez Scheme from a substantially incompatible version --- for
;; example, one with different C entries, or fewer foreign procedures
;; exported from the kernel, or a different base-lanuage AST, a
;; different representation of syntax objects, or a different
;; representation of some preimitive datatype, or simply a version
;; number.
;;
;; This script doesn't yet support the possibility differently shaped
;; `#!base-rtd`, but the general strategy here should support that
;; when it becomes needed by defining replacement structure procedures,
;; including `csv7:record-field-accessor`.
;;
;; The script is specific to the implementation being bootstapped. If
;; the implementation changes by, say, moving functionality to
;; different files, then this script likely needs updates. The script
;; will also need updates if the compiler starts using new primitives
;; at compile time. The host Scheme used to bootstrap needs to be new
;; enough to load the current nanopass implementation and to define
;; any macro used in "cmacros.ss" or "syntax.ss"; if something is
;; missing from the host Scheme, then hopefully you can define it here
;; (similar to temporarily adding to "patch.ss").

;; To run:
;;
;;   - create a directory that has a suitable "machine.def", and
;;
;;   - run this script as `scheme --script reboot.ss <dir>` in
;;     the Chez Scheme source directory with `<dir>` as the
;;     directory containing "machine.def".
;;
;; These steps are intended to be performed by a `reboot` makefile
;; target.
;;
;; Output is written to "boot/<machine>", where "<machine>" is
;; determined by the "<dir>/machine.def" file.

;; Implementation:
;;
;; The overall strategy is to load the Chez Scheme macro expander and
;; compiler as user-level programs, where `$primitive` no longer
;; necessarily gets the primitive versions of procedures. Instead,
;; `$primitive` is redefined to look in the top-level environment.
;; This redirect primitives only need to work well enough to run the
;; expander and compiler.
;;
;; Since `$primitive` no longer accesses primitives, we need to define
;; here any `$`-prefixed name that is used by the expander and
;; compiler. Mostly, we just define them to be the primitive functions
;; (before `$primitive` is redefined), but they could instead be
;; implemented using other functions.
;;
;; The trickiest part is loading the macro expander, which defines the
;; layout of syntax objects, but also has literal syntax objects in
;; its implementation. Both the expander and predefined macros are
;; defined using syntax objects and many of the same macros that will
;; be defined. So, it takes a few passes:
;;
;;   - Start by loading "cmacros.ss" and similar so that data about
;;     the target platform, etc., is available. Currently, we assume
;;     that the host Scheme can run "mkheader.ss" and similar, so
;;     we run that first to get ".h" and ".inc" files generated.
;;
;;   - The expander needs nanopass, so load that. The host Scheme
;;     needs to be new enough to run the current nanopass.
;;
;;   - Next, load just the expander implementation, which is defined
;;     as the first big S-expression in "syntax.ss".
;;
;;   - For each subsequent term in "syntax.ss", expand the expression
;;     part with the host Scheme's macro expander. Then, rewrite
;;     syntax-object literals in the expansion into new-expander
;;     literals, using the `$datum->environment-syntax` procedure
;;     exported by the expander for this purpose. Finally, send the
;;     definition through the just-loaded new expander, which
;;     registers the definitions in its system environment.
;;
;;     At this point, we now have the expander and predefined macros
;;     all working with the new expander's representation for syntax
;;     objects and base-language AST. This expander is *not* wired
;;     into `eval` or installed to the real `current-expand` (but it
;;     is installed to a new, user-level `current-expand`). Instead,
;;     the expander it must be called directly as `sc-expand`.
;;
;;     When `sc-expand` is running, it may need to evalute via `eval`.
;;     The new expander's AST form is converted by to an S-expression
;;     using the new expander's `$uncprep`. Syntax-object literals are
;;     not converted back, however; the evaluated/compiled code should
;;     operate on new-expander syntax objects.
;;
;;   - Take it from the top by loading "cmacro.ss", etc., using the
;;     new expander, which defines macros to work with the new
;;     expander.
;;
;;   - Load nanopass again, too. The nanopass implementation shouldn't
;;     be any different this second time around, but now it's defined
;;     and registered in the new expander's table of modules.
;;
;;   - Load the compiler. The compiler implementation is a
;;     hand-crafted list of files that cover everything needed to run
;;     `compile-file` and `$make-boot-file`.
;;
;;   - The the user-level `current-expand` to the new expander, since
;;     the just-loaded `compile-file` will reach it via a new
;;     user-level `expand`. While the new expander is running via
;;     `current-expand`, set the real `current-expand` to perform the
;;     same dance as before to handle the times when the expander
;;     calls `eval`.
;;
;;   - Run `compile-file` on all of the sources. Run `$make-boot-file`
;;     to create "petite.boot" and "scheme.boot".

(define-values (xc-dir host-dir)
  (let ([l (command-line-arguments)])
    (if (= 2 (length l))
        (apply values l)
        (error 'reboot "expected <xc-dir> <host-dir>"))))

(meta-cond
 [(top-level-bound? 'path-build) (begin)]
 [else
  (define path-build
    (lambda (a b)
      (let ([sep (if (eqv? (string-ref a (sub1 (string-length a))) #\/) "" "/")])
        (string-append a sep b))))])

(let ([machine.def (path-build xc-dir "machine.def")])
  (unless (file-exists? machine.def)
    (error 'reboot "~a not found" machine.def)))

(define (select-config config-dir)
  (source-directories (list config-dir "s" "unicode")))
(select-config xc-dir)
(library-directories '(("." . ".") ("nanopass" . "nanopass")))

(define (status s)
  (printf "~a\n" s)
  (flush-output-port))

;; Read "s/build.zuo" to get the set of sources for "petite.boot"
;; and "scheme.boot", so we don't have a separate copy here.
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

;; In case of debugging printfs:
(print-graph #t)

;; We need to keep track of all the user-level "primitives" that we
;; define, so we can carry them over to a new namespace that is
;; created by the new expander.
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

;; Start defining "primitives" here vv ----------------------------------------

(define-primitive ($make-record-type base-rtd parent name fields sealed? opaque? . extras)
  (apply #%$make-record-type base-rtd parent name fields sealed? opaque? extras))
(define-primitive ($make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields . extras)
  (apply #%$make-record-type-descriptor base-rtd parent name uid sealed? opaque? fields extras))
(define-primitive ($make-record-constructor-descriptor rts parent protocol name)
  (#%$make-record-constructor-descriptor rts parent protocol name))

(define (make-$sputprop meta-key)
  (lambda (sym key val)
    (putprop sym meta-key (cons (cons key val) (getprop sym meta-key '())))))

(define (make-$sgetprop meta-key)
  (lambda (sym key def-val)
    (let ([a (assq key (getprop sym meta-key '()))])
      (if a
          (cdr a)
          def-val))))

(define-primitive (make-$sremprop meta-key)
  (lambda (sym key)
    (let ([a (assq key (getprop sym meta-key '()))])
      (when a
        (putprop sym 'reboot (filter
                              (lambda (p)
                                (not (eq? (car p) key)))
                              (getprop sym meta-key '())))))))

(define-primitive $sputprop (make-$sputprop 'reboot-host))
(define-primitive $sgetprop (make-$sgetprop 'reboot-host))
(define-primitive $sremprop (make-$sremprop 'reboot-host))

;; We'll use these when we're ready to compile/expand for the client,
;; and that way information about primitives for host and client are
;; kept separate
(define client-$sputprop (make-$sputprop 'reboot-client))
(define client-$sgetprop (make-$sgetprop 'reboot-client))
(define client-$sremprop (make-$sremprop 'reboot-client))

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

(define-primitive symbol? (lambda (x) (and (#%symbol? x)
                                            (not (eq? x $the-unbound-object)))))
(define-primitive gensym? (lambda (x) (and (#%gensym? x)
                                           (not (eq? x $the-unbound-object)))))

(define-primitive $immediate? (lambda (x)
                                (or (#%$immediate? x)
                                    (eq? x $the-unbound-object))))
(define-primitive $flonum->digits #%$flonum->digits)
(define-primitive $flonum-sign #%$flonum-sign)
(define-primitive $integer-32? #%$integer-32?)
(define-primitive $integer-64? #%$integer-64?)
(define-primitive $fxu< #%$fxu<)
(define-primitive $stencil-vector? (lambda (v) #f))
(define-primitive $system-stencil-vector? (lambda (v) #f))
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
(define-primitive $remake-rtd (lambda (rtd compute-field-offsets)
                                (parameterize ([#%$target-machine ($target-machine)])
                                  (#%$remake-rtd rtd compute-field-offsets))))

(define-primitive $thread-list #%$thread-list)

(define-primitive $c-bufsiz #%$c-bufsiz)

(define-primitive $separator-character (meta-cond
                                        [(#%$top-level-bound? '$separator-character) #%$separator-character]
                                        [else #\/]))

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

(define primitive-substs (make-eq-hashtable))

(define-primitive $top-level-value
  (let ([orig-top-level-bound? top-level-bound?]
        [orig-top-level-value top-level-value])
    (lambda (s)
      (let ([s (hashtable-ref primitive-substs s s)])
        (if (orig-top-level-bound? s primitive-environment)
            (orig-top-level-value s primitive-environment)
            (begin
              (unless (eq? s '$capture-fasl-target)
                (printf "  [unbound: ~s]\n" s))
              ($unbound-object)))))))
(define-primitive $set-top-level-value!
  (let ([top-level-bound? top-level-bound?]
        [set-top-level-value! set-top-level-value!]
        [define-top-level-value define-top-level-value])
    (lambda (sym val)
      (let ([sym (hashtable-ref primitive-substs sym sym)])
        (if (top-level-bound? sym  primitive-environment)
            (set-top-level-value! sym val  primitive-environment)
            (define-top-level-value sym val primitive-environment))))))

(define $tc-mutex (and (threaded?) (make-mutex)))

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
        (let* ([sym (syntax->datum id)]
               [sym (hashtable-ref primitive-substs sym sym)])
          (if (orig-top-level-bound? sym primitive-environment)
              ;; This works as long as primitives are never locally shadowed,
              ;; (which won't be the case for expanded code, at least):
              (datum->syntax id sym)
              ;; If it's not yet there, defer the lookup, and maybe we
              ;; won't have to fill in the primitive:
              #`($primitive-value (quote #,(datum->syntax id sym))))))
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

;; End of "primitives" here ^^ ----------------------------------------

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

(define (load-machine-config)
  (for-each noisy-compile-and-load
            '("cmacros.ss" "priminfo.ss" "primvars.ss")))
(load-machine-config)

;; Loading "cmacros.ss" defined `constant`:
(define (set-target-machine mach)
  ($target-machine mach)
  (status (format "Configured for machine: ~a" ($target-machine))))
(set-target-machine (constant machine-type-name))

(define out-dir (path-build "boot" (symbol->string ($target-machine))))
(unless (file-directory? out-dir)
  (mkdir out-dir))

(status "== Generate headers")
(noisy-load "mkheader.ss")
(mkscheme.h (path-build out-dir "scheme.h") (constant machine-type-name))
(mkequates.h (path-build out-dir "equates.h"))

(status "== Generate GC traversals")
(noisy-load "mkgc.ss")
(mkgc-ocd.inc (path-build out-dir "gc-ocd.inc"))
(mkgc-oce.inc (path-build out-dir "gc-oce.inc"))
(mkgc-par.inc (path-build out-dir "gc-par.inc"))
(mkheapcheck.inc (path-build out-dir "heapcheck.inc"))

(status "== Switching to host mode")
(select-config host-dir)
(load-machine-config)
(set-target-machine (constant machine-type-name))

;; The expander implementation needs nanopass loaded
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

;; Configure as usual for loading implementaton files
(for-each noisy-load
          '("setup.ss" "env.ss"))

;; ... but use the compiler instead of the interpreter
(current-eval compile)

;; Set up a syntax-object bridge between the old and new worlds

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

(status "== Install new expander")

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
;; Forward reference, of sorts:
(set! $annotation-options (make-enumeration '(debug profile)))
(set! $make-annotation-options (enum-set-constructor $annotation-options))

;; Loading after the expander means that `%uncprep` sees the new
;; syntax-object constructors
(noisy-load "cprep.ss")

;; Load rest of expander with the new expander, but the interpreter
;; still uses the language of the old expander
(define (eval-with-expand s mode eval-mode)
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
                              (interaction-environment))
                        #f (eq? eval-mode 'expand)))])
    #;(printf "=> ~s\n" e)
    (if (eq? eval-mode 'expand)
        e
        (let ([r (eval e)])
          #;(printf "= ~s\n" r)
          r))))

(define (evalx s)
  (eval-with-expand s 'system-macros 'eval))

(for-each (lambda (sym)
            (let ([val ($top-level-value sym)])
              (evalx `(define ,sym ',val))
              val))
          primitives)

;; Forward reference
(evalx `(define $syntax-match? #f))

;; Prepare to expand the implementations of macros using the host
;; Scheme, so that macro implementations can use macros that are not
;; yet defined. This constrains the implementation of macros defined
;; in "syntax.ss" to use only constructs in the host Scheme
;; implementation.
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
(define (load-syntax-macro-definitions eval)
  (for-each eval
            (cddr (file->exps (path-build "s" "syntax.ss")))))
(load-syntax-macro-definitions evale)

;; Not defined in "syntax.ss", but needed to load nanopass:
(evale '(define-syntax guard
          (syntax-rules (else)
            [(_ (var clause ... [else e1 e2 ...]) b1 b2 ...)
             ($guard #f (lambda (var) (cond clause ... [else e1 e2 ...]))
                     (lambda () b1 b2 ...))]
            [(_ (var clause1 clause2 ...) b1 b2 ...)
             ($guard #t (lambda (var p) (cond clause1 clause2 ... [else (p)]))
                     (lambda () b1 b2 ...))])))

(define (expand/then-load s mode skip)
  (status (format "Loading ~a" s))
  (let ([vs
         ;; expand in order (so don't use `map`):
         (let loop ([es (skip (file->exps s))])
           (if (null? es)
               '()
               (let* ([v (let ([e (car es)])
                           #;(printf "~s\n" e)
                           (eval-with-expand e mode 'expand))])
                 #;(printf "~s\n" v)
                 (cons v
                       (loop (cdr es))))))])
    (let loop ([v (cons 'begin vs)])
      (cond
        [(and (pair? v) (eq? (car v) 'begin))
         (for-each loop (cdr v))]
        [(and (pair? v) (eq? (car v) 'eval-when))
         (for-each loop (cddr v))]
        [else
         #;(printf "~s\n" v)
         (eval v)]))))

(define (expand-and-load s mode)
  (status (format "Loading ~a" s))
  (for-each (lambda (e)
              #;(printf "~s\n" e)
              (eval-with-expand e mode 'eval))
            (file->exps s)))

(status "== Setup for using expander")
(define (configure-compile-time same-host-and-target?)
  (if same-host-and-target?
      (expand-and-load "s/cmacros.ss" 'system)
      (expand/then-load "s/cmacros.ss" 'system values))
  (expand-and-load "s/priminfo.ss" 'system)
  (expand-and-load "s/primvars.ss" 'system))
(configure-compile-time #t)

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
                                        (eval-with-expand (if (pair? (cadr e))
                                                              `(lambda ,(cdadr e) . ,(cddr e))
                                                              (caddr e))
                                                          'system
                                                          'eval))]
                [(and (pair? e)
                      (eq? 'begin (car e)))
                 (for-each loop (cdr e))])))
          (file->exps "s/7.ss"))

(define-primitive $sputprop (make-$sputprop 'reboot-host))
(define-primitive $sgetprop (make-$sgetprop 'reboot-host))
(define-primitive $sremprop (make-$sremprop 'reboot-host))

(status "== Load nanopass using expander")
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
(load-nano "nanopass.ss")

(status "== Set configuration to target")
(hashtable-set! primitive-substs '$sputprop 'client-$sputprop)
(hashtable-set! primitive-substs '$sgetprop 'client-$sgetprop)
(hashtable-set! primitive-substs '$sremprop 'client-$sremprop)
(select-config xc-dir)
(configure-compile-time #f) ; compile as host, load to set target
(set-target-machine (constant machine-type-name))

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

(status "== Switch to target compilation")

;; "syntax.ss" may define different macros for the taregt platform, so
;; load those now while we have macros defined for the host but
;; configuration for the target; we are assuming that the expander
;; does not itself work differently when the target changes
(expand/then-load "s/syntax.ss" 'system cdr)

(configure-compile-time #t) ; set compile-time macros for target
(init-syntax-libraries) ; target may have different primitives

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
    (parameterize ([optimize-level 3]
                   [debug-level 0])
      (compile-file (path-build "s" s)
                    (path-build xc-dir (string-append (path-root s) ".so"))))
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
