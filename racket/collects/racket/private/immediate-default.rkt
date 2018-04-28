(module kw '#%kernel
  (#%require "define.rkt"
             "small-scheme.rkt"
             "stxcase-scheme.rkt"
             (for-template '#%kernel))

  (#%provide immediate-default?)

  ;; A default-argument expression counts as an "immediate default"
  ;; if it syntactically (before expansion) matches
  ;;
  ;;    <immediate-default> = #t | #f | <number>    [*]
  ;;                        | '#t | '#f | '<number>
  ;;                        | '<id> | '()
  ;;                        | (void) | null
  ;;
  ;; where the three [*] possibilities match only if the literal's
  ;; syntax transferred to '#%datum is bound to `#%datum` from
  ;; `racket/base`.

  (define (immediate-default? expr)
    (let ([immediate-literal?
           (lambda (v)
             (or (boolean? v)
                 (number? v)))])
      (or (and (immediate-literal? (syntax-e expr))
               (free-identifier=? (quote-syntax #%datum) (datum->syntax expr '#%datum)))
          (syntax-case expr (quote void null)
            [(quote s-exp) (let ([v (syntax-e #'s-exp)])
                             (or (symbol? v)
                                 (null? v)
                                 (immediate-literal? v)))]
            [(void) #t]
            [null #t]
            [_ #f])))))
