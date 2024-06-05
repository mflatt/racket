#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre))

(provide (rename-out
          [module-begin #%module-begin]))
 
(module reader syntax/module-reader
  compiler/demod)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ mod-path)
     (define (get sym)
       (dynamic-require 'compiler/demodularizer/main sym))
     (define demodularize (get 'demodularize))
     (define syntax-object-preservation-enabled (get 'syntax-object-preservation-enabled))
     (parameterize ([syntax-object-preservation-enabled #t])
       (datum->syntax
        #f
        (demodularize (resolved-module-path-name
                       (module-path-index-resolve
                        (module-path-index-join (syntax->datum #'mod-path) #f)))
                      #:return-bundle? #t)))]))
