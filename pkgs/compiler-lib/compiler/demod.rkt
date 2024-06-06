#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (rename-out
          [module-begin #%module-begin]))
 
(module reader syntax/module-reader
  compiler/demod)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ mod-path
        (~alt (~optional (~seq #:exclude (exclude-mod-path ...))
                         #:defaults ([(exclude-mod-path 1) '()]))
              (~optional (~seq #:max-phase max-phase)
                         #:defaults ([max-phase #'1]))
              (~optional (~seq (~and demod-submod #:demod-submodules))
                         #:defaults ([demod-submod #f]))
              (~optional (~seq #:submodule ([submod-name (~and option (~or #:demod)) ...]
                                            ...))
                         #:defaults ([(submod-name 1) '()]
                                     [(option 2) '()])))
        ...)
     (define (get sym)
       (dynamic-require 'compiler/demodularizer/main sym))
     (define demodularize (get 'demodularize))
     (define syntax-object-preservation-enabled (get 'syntax-object-preservation-enabled))
     (define bundle
      (demodularize (resolved-module-path-name
                     (module-path-index-resolve
                      (module-path-index-join (syntax->datum #'mod-path) #f)))
                    #:keep-syntax? #t
                    #:work-directory (build-path (or (current-load-relative-directory)
                                                     (current-directory))
                                                 "compiled/demod")
                    #:exclude (for/list ([mod-path (syntax->list #'(exclude-mod-path ...))])
                                (resolved-module-path-name
                                 (module-path-index-resolve
                                  (module-path-index-join (syntax->datum mod-path) #f))))
                    #:demod-submodules? (attribute demod-submod)
                    #:submodule-specs (for/hash ([submod-name (in-list (syntax->datum #'(submod-name ...)))]
                                                 [options (in-list (syntax->datum #'((option ...) ...)))])
                                        (values submod-name
                                                (for/hasheq ([option (in-list options)])
                                                  (values (string->symbol (keyword->string option))
                                                          #t))))
                    #:max-phase (syntax-e #'max-phase)
                    #:return-bundle? #t))
     (with-output-to-file "/tmp/dump" #:exists 'truncate (lambda () (write bundle)))
     (datum->syntax #f bundle)]))
