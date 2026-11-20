(module require-transform '#%kernel
  (#%require "core-syntax.rkt"
             (only '#%unsafe unsafe-root-continuation-prompt-tag))

  (#%provide current-lift-require-definition-box
             call-with-lift-require-definition-box)

  ;; Implemented with a continuation mark instead of a parameter, so
  ;; that this module has no state, and its instance can be shared
  ;; across module expansions
  (define lift-require-definition-key
    (make-continuation-mark-key 'syntax-local-lift-require-definition))

  (define (current-lift-require-definition-box)
    (continuation-mark-set-first #f lift-require-definition-key #f
                                 (unsafe-root-continuation-prompt-tag)))

  (define (call-with-lift-require-definition-box b thunk)
    (with-continuation-mark lift-require-definition-key b
      (thunk))))
