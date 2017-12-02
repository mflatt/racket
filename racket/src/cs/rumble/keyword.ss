
(define-record-type keyword
  (fields symbol)
  (nongenerative #{keyword dhghafpy3v03qbye1a9lwf-0}))

(define/who (string->keyword s)
  (check who string? s)
  (let ([sym (string->symbol s)])
    (or (getprop sym 'keyword #f)
        (let ([kw (make-keyword sym)])
          (putprop sym 'keyword kw)
          kw))))

(define/who (keyword->string kw)
  (check who keyword? kw)
  (symbol->string (keyword-symbol kw)))

(define/who keyword<?
  (case-lambda
   [(a b)
    (check who keyword? a)
    (check who keyword? b)
    (symbol<? (keyword-symbol a)
              (keyword-symbol b))]
   [(a b . cs)
    (check who keyword? a)
    (check who keyword? b)
    (let loop ([prev b] [cs cs] [lt? (keyword<? a b)])
      (cond
       [(null? cs) lt?]
       [else
        (let ([c (car cs)])
          (check who keyword? c)
          (loop c (cdr cs) (and lt? (keyword<? prev c))))]))]))
