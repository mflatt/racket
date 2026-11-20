(module print-value-columns '#%kernel
  (#%require (for-syntax '#%kernel (only '#%linklet primitive-table)))

  ;; The parameter is implemented by the core, so that a module that
  ;; uses it can have its instance shared across namespaces. To support
  ;; bootstrapping (e.g., extracting the expander) with an older core
  ;; that does not have the primitive, fall back to a definition that
  ;; uses a different name internally, so it does not collide with the
  ;; primitive in flattened code that is later run with a newer core.
  (define-syntaxes (define-print-value-columns)
    (lambda (stx)
      (datum->syntax
       stx
       (if (hash-ref (primitive-table '#%kernel) 'print-value-columns #f)
           '(begin
              (#%require (only '#%kernel print-value-columns))
              (#%provide print-value-columns))
           '(begin
              (#%provide (rename print-value-columns/fallback print-value-columns))
              (define-values (print-value-columns/fallback)
                (make-parameter +inf.0
                                (lambda (c)
                                  (if (if (eqv? c +inf.0)
                                          #t
                                          (if (exact-integer? c)
                                              ;; somewhat arbitrary value, enough for "(list"
                                              (> c 5)
                                              #f))
                                      c
                                      (raise-argument-error 'print-value-columns
                                                            "(or/c +inf.0 (and/c exact-integer? (>/c 5)))"
                                                            c)))
                                'print-value-columns)))))))

  (define-print-value-columns))
