
(define-syntax (include-generated stx)
  (syntax-case stx ()
    [(inc file)
     (let* ([dir "../bootstrapped/"]
            [file (#%datum->syntax #'inc (string-append dir (#%syntax->datum #'file)))])
       (#%datum->syntax #'inc `(include ,file)))]))
