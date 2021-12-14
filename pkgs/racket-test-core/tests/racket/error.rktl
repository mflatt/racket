(load-relative "../racket/loadtest.rktl")

(Section 'error)

(test #t parameter? error-primitive-name->symbol-handler)
(test #t parameter? error-primitive-contract->string-handler)
(test #t parameter? error-primitive-message->string-handler)
(test #t parameter? error-struct-operation-names-handler)

(define-syntax-rule (test-error-match rx e)
  (test #t
        regexp-match?
        rx
        (with-handlers ([exn:fail? exn-message])
          e
          "no error")))

(parameterize ([error-primitive-name->symbol-handler
                (lambda (n)
                  (case n
                    [(cons) 'kons]
                    [(vector-ref) 'vector/ref]
                    [(bytes-ref) 'bytes/ref]
                    [(thread-wait) 'wait-thread]
                    [(regexp-match) 'rx-match]
                    [(read-char) 'read/char]
                    [else n]))])
  (test-error-match #rx"^kons" (cons 1))
  (test-error-match #rx"^vector/ref" (vector-ref 1 2))
  (test-error-match #rx"^vector/ref" (vector-ref '#(1) 2))
  (test-error-match #rx"^bytes/ref" (bytes-ref 1 2))
  (test-error-match #rx"^bytes/ref" (bytes-ref #"1" 2))
  (test-error-match #rx"^bytes/ref" (bytes-ref 1))
  (test-error-match #rx"^wait-thread" (thread-wait "not a thread"))
  (test-error-match #rx"^rx-match" (regexp-match 10))
  (test-error-match #rx"^read/char" (read-char (open-output-bytes)))
  (test-error-match #rx"^read/char" (let ([p (open-input-bytes #"")])
                                      (close-input-port p)
                                      (read-char p)))

  (test-error-match #rx"^cons" (raise-argument-error 'cons "string?" 17))
  (test-error-match #rx"^cons" (let ([cons (lambda (x) x)])
                                 (cons 1 2))))

(parameterize ([error-primitive-name->symbol-handler
                (lambda (n) "oops!")])
  (test-error-match #rx"^[.][.][.]:" (cons 1)))

(parameterize ([error-primitive-message->string-handler
                (lambda (who str)
                  (format "~a>> ~a" who str))])
  (test-error-match #rx"^cons>> arity mismatch" (cons 1))
  (test-error-match #rx"^f>> arity mismatch" (let ([f (lambda (x y) x)])
                                               (f 1)))
  (test-error-match #rx"^vector-ref>> index is out of range" (vector-ref '#(1 2 3) 10))
  (test-error-match #rx"^vector[*]-ref>> index is out of range" (vector*-ref '#(1 2 3) 10))
  (test-error-match #rx"^f: arity mismatch" (error 'f "arity mismatch")))

(parameterize ([error-primitive-message->string-handler
                (lambda (who str) 'oops!)])
  (test-error-match #rx"^[.][.][.]" (cons 1)))

(parameterize ([error-primitive-contract->string-handler
                (lambda (ctc)
                  (case ctc
                    [("number?") "number/c"]
                    [else ctc]))])
  (test-error-match #rx"expected: number/c" (+ 'a 'b))

  (test-error-match #rx"expected: number[?]" (raise-argument-error 'plus "number?" 'a)))

(parameterize ([error-primitive-contract->string-handler
                (lambda (ctc)
                  'oops!)])
  (test-error-match #rx"expected: [.][.][.]" (+ 'a 'b)))

(parameterize ([error-struct-operation-names-handler
                (lambda (name field mode)
                  (values (string->symbol (format "~a/~a/~a" name field mode))
                          (format "is_~a" name)))])
  (struct posn (x y) #:mutable)
  (test-error-match #rx"^posn/x/ref" (posn-x 1))
  (test-error-match #rx"expected: is_posn" (posn-x 1))
  (test-error-match #rx"^posn/x/set!" (set-posn-x! 1 2))

  (struct pt (x y) #:mutable #:authentic)
  (test-error-match #rx"^pt/y/ref" (pt-y 1))
  (test-error-match #rx"expected: is_pt" (pt-y 1))
  (test-error-match #rx"^pt/y/set!" (set-pt-y! 1 2))

  (test-error-match #rx"^date/second" (date-second 10)))

(parameterize ([error-struct-operation-names-handler
                (lambda (name field mode)
                  'oops!)])
  (test-error-match #rx"^[.][.][.]: .* expected: [.][.][.]" (date-second 0)))

(report-errs)
