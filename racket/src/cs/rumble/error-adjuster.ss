
(define/who current-error-message-adjuster
  (make-parameter (lambda (mode)
                    (case mode
                      [(name)
                       (lambda (name realm)
                         (define who 'default-error-message-adjust-handler/name-mode)
                         (check who symbol? name)
                         (check who symbol? realm)
                         (values name realm))]
                      [(contract)
                       (lambda (str realm)
                         (define who 'default-error-message-adjust-handler/contract-mode)
                         (check who string? str)
                         (check who symbol? realm)
                         (values str realm))]
                      [(message)
                       (lambda (from from-realm msg msg-realm)
                         (define who 'default-error-message-adjust-handler/message)
                         (check who symbol? :or-false from)
                         (check who symbol? from-realm)
                         (check who string? msg)
                         (check who symbol? msg-realm)
                         (values from from-realm msg msg-realm))]
                      [else
                       (let ([who 'default-error-message-adjust-handler])
                         (check who (lambda (x) #f)
                                :contract "(or/c 'name 'contract 'message)"
                                mode))]))
                  (lambda (v)
                    (check who (procedure-arity-includes/c 1) v)
                    v)
                  'current-error-message-adjuster
                  primitive-realm))

(define error-message-adjuster-key '#{error-message fid2zn5ypffnt8i3i6rr29nfn-0})

(define (apply-adjusters vals modes)
  (define (apply-adjuster adjr vals)
    (let loop ([modes modes])
      (cond
        [(null? modes) vals]
        [(|#%app| adjr (caar modes))
         => (lambda (adj)
              (define n-args (cadar modes))
              (define apply-adj (caddar modes))
              (unless (and (procedure? adj)
                           (procedure-arity-includes? adj n-args))
                (raise-result-error* 'current-error-message-adjuster
                                     primitive-realm
                                     (string-append-immutable "(or/c (procedure-arity-includes/c "
                                                              (number->string n-args)
                                                              ") #f)")
                                     adj))
              (apply-adj adj vals))]
        [else (loop (cdr modes))])))
  (cond
    [(eq? none (continuation-mark-set-first #f error-message-adjuster-key none))
     (apply-adjuster (|#%app| current-error-message-adjuster) vals)]
    [else
     (let loop ([procs (continuation-mark-set->list #f
                                                    error-message-adjuster-key
                                                    the-root-continuation-prompt-tag)]
                [vals vals])
       (if (null? procs)
           (apply-adjuster (|#%app| current-error-message-adjuster) vals)
           (loop (cdr procs)
                 (let ([adjr (car procs)])
                   (if (and (procedure? adjr)
                            (procedure-arity-includes? adjr 1))
                       (apply-adjuster adjr vals)
                       vals)))))]))

(define (error-message->adjusted-string name name-realm msg msg-realm)
  (let ([v (apply-adjusters (vector name name-realm msg msg-realm)
                            (list
                             (list
                              'message
                              4
                              (lambda (proc v)
                                (let ([who '|current-error-message-adjuster for message|])
                                  (#%call-with-values
                                   (lambda () (|#%app|
                                               proc
                                               (#%vector-ref v 0)
                                               (#%vector-ref v 1)
                                               (#%vector-ref v 2)
                                               (#%vector-ref v 3)))
                                   (case-lambda
                                    [(name name-realm msg msg-realm)
                                     (unless (or (not name) (symbol? name))
                                       (raise-result-error* who primitive-realm "(or/c symbol? #f)" name))
                                     (unless (symbol? name-realm)
                                       (raise-result-error* who primitive-realm "symbol?" name-realm))
                                     (unless (string? msg)
                                       (raise-result-error* who primitive-realm "string?" msg))
                                     (unless (symbol? msg-realm)
                                       (raise-result-error* who primitive-realm "symbol?" msg-realm))
                                     (vector name name-realm msg msg-realm)]
                                    [args
                                     (apply raise-result-arity-error* who primitive-realm 4 #f args)])))))
                             (list
                              'name
                              2
                              (lambda (proc v)
                                (let ([who '|current-error-message-adjuster for name|])
                                  (#%call-with-values
                                   (lambda () (|#%app|
                                               proc
                                               (#%vector-ref v 0)
                                               (#%vector-ref v 1)))
                                   (case-lambda
                                    [(name name-realm)
                                     (unless (or (not name) (symbol? name))
                                       (raise-result-error* who primitive-realm "(or/c symbol? #f)" name))
                                     (unless (symbol? name-realm)
                                       (raise-result-error* who primitive-realm "symbol?" name-realm))
                                     (vector name name-realm (#%vector-ref v 2) (#%vector-ref v 3))]
                                    [args
                                     (apply raise-result-arity-error* who primitive-realm 2 #f args)])))))))])
    (let ([name (#%vector-ref v 0)]
          [msg (#%vector-ref v 2)])
      (if name
          (string-append-immutable (symbol->immutable-string name)
                                   ": "
                                   msg)
          msg))))

(define (error-contract->adjusted-string contract realm)
  (car
   (apply-adjusters (cons contract realm)
                    (list
                     (list
                      'contract
                      2
                      (lambda (proc ctc+realm)
                        (let ([who '|current-error-message-adjuster for contract|])
                          (#%call-with-values
                           (lambda () (|#%app| proc (car ctc+realm) (cdr ctc+realm)))
                           (case-lambda
                            [(ctc realm)
                             (unless (string? ctc)
                               (raise-result-error* who primitive-realm "string?" ctc))
                             (unless (symbol? realm)
                               (raise-result-error* who primitive-realm "symbol?" realm))
                             (cons ctc realm)]
                            [args
                             (apply raise-result-arity-error* who primitive-realm 2 #f args)])))))))))
