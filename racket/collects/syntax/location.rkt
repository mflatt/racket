#lang racket/base
(require syntax/srcloc
         (for-syntax racket/base syntax/srcloc setup/path-to-relative
                     (only-in '#%unsafe unsafe-assume-shareable)))
(provide (protect-out module-name-fixup)
         quote-srcloc
         quote-srcloc-string
         quote-srcloc-prefix
         quote-source-file
         quote-line-number
         quote-column-number
         quote-character-position
         quote-character-span
         quote-module-path
         quote-module-name
         syntax-source-directory
         syntax-source-file-name)

(begin-for-syntax

  ;; A cache is ok to share across instantiations
  (define cache (unsafe-assume-shareable (make-hash)))
  
  (define (source-location-relative-source loc)
    (define src (source-location-source loc))
    (and (path-string? src)
         (path->relative-string/library src #f #:cache cache)))

  (define (syntax-quote-source stx)
    (cond
      [(source-location-relative-source stx)
       =>
       (lambda (rel) #`(quote #,rel))]
      [else #`(source-location-source
                (quote-syntax
                  #,(identifier-prune-to-source-module
                      (datum->syntax stx 'here stx stx))))]))

  (define (syntax-quote-line stx) #`(quote #,(syntax-line stx)))

  (define (syntax-quote-column stx) #`(quote #,(syntax-column stx)))
  (define (syntax-quote-position stx) #`(quote #,(syntax-position stx)))
  (define (syntax-quote-span stx) #`(quote #,(syntax-span stx)))
  (define (syntax-quote-srcloc stx)
    #`(srcloc
        #,(syntax-quote-source stx)
        #,(syntax-quote-line stx)
        #,(syntax-quote-column stx)
        #,(syntax-quote-position stx)
        #,(syntax-quote-span stx)))

  (define (syntax-quote-string stx)
    (cond
      [(source-location-relative-source stx)
       #`(quote #,(source-location->string stx))]
      [else
       #`(source-location->string
           #,(syntax-quote-srcloc stx))]))

  (define (syntax-quote-prefix stx)
    (cond
      [(source-location-relative-source stx)
       #`(quote #,(source-location->prefix stx))]
      [else
       #`(source-location->prefix
           #,(syntax-quote-srcloc stx))]))

  (define (source-transformer proc)
    (lambda (stx)
      (syntax-case stx ()
        [(_) (proc stx)]
        [(_ here) (proc #'here)]))))

(define-syntax quote-srcloc
  (source-transformer syntax-quote-srcloc))

(define-syntax quote-source-file
  (source-transformer syntax-quote-source))

(define-syntax quote-line-number
  (source-transformer syntax-quote-line))

(define-syntax quote-column-number
  (source-transformer syntax-quote-column))

(define-syntax quote-character-position
  (source-transformer syntax-quote-position))

(define-syntax quote-character-span
  (source-transformer syntax-quote-span))

(define-syntax quote-srcloc-string
  (source-transformer syntax-quote-string))

(define-syntax quote-srcloc-prefix
  (source-transformer syntax-quote-prefix))

(define (variable-reference->module-source/submod vr)
  (module-source/submod (variable-reference->module-source vr)
                        (variable-reference->resolved-module-path vr)))

(define (module-source/submod src rname)
  (define name (and rname (resolved-module-path-name rname)))
  (if (pair? name)
      (cons src (cdr name))
      src))

;; Passing `(#%variable-reference)` directly to primitives (instead of
;; to `variable-reference->module-source/submod`) lets a module that
;; uses `quote-module-name` have its instance shared across namespaces
(define-syntax-rule (module-source)
  (module-source/submod
   (variable-reference->module-source (#%variable-reference))
   (variable-reference->resolved-module-path (#%variable-reference))))

(define-for-syntax (do-quote-module stx fixup)
  (syntax-case stx ()
    [(_ path ...) 
     (for ([path (in-list (syntax->list #'(path ...)))]
           [i (in-naturals)])
       (unless (or (symbol? (syntax-e path))
                   (equal? (syntax-e path) ".."))
         (raise-syntax-error #f "not a submodule path element" stx path)))
     (with-syntax ([fixup fixup])
       #'(fixup (module-source) (list 'path ...)))]))

(define-syntax (quote-module-name stx)
  (do-quote-module stx #'module-name-fixup))

(define (module-name-fixup src path)
  (do-fixup src path #f))

(define-syntax (quote-module-path stx)
  (do-quote-module stx #'module-path-fixup))
    
(define (module-path-fixup src path)
  (do-fixup src path #t))

(define (do-fixup src path as-modpath?)
  (define (last-pass src)
    (cond
     [(path? src) src]
     [(symbol? src) (if as-modpath?
                        `(quote ,src)
                        src)]
     [(list? src) 
      (define base (last-pass (car src)))
      (define sm (cdr src))
      (if as-modpath?
          `(submod ,base ,@sm)
          (cons base sm))]
     [else 'top-level]))
  (last-pass
    (cond 
      [(null? path) src]
      [(pair? src) (append src path)]
      [else (cons src path)])))

;; ------------------------------------------------------------------------

(define (syntax-source-directory stx)
  (let* ([source (syntax-source stx)])
    (and (path-string? source)
         (let-values ([(base file dir?) (split-path source)])
           (and (path? base)
                (path->complete-path base
                                     (or (current-load-relative-directory)
                                         (current-directory))))))))

(define (syntax-source-file-name stx)
  (let* ([f (syntax-source stx)])
    (and (path-string? f)
         (let-values ([(base file dir?) (split-path f)]) file))))
