(module path-list '#%kernel
  (#%require "core-syntax.rkt"
             (rename "core-syntax.rkt" -define define)
             (rename "core-syntax.rkt" -define-syntax define-syntax)
             (only '#%unsafe unsafe-assume-shareable))

  (#%provide path-list-string->path-list)

  ;; The regexp is created lazily, and it's ok to share the box
  ;; across instances
  (-define rx:path-list-box (unsafe-assume-shareable (box #f)))

  (-define (get-rx:path-list)
    (or (unbox rx:path-list-box)
        (let ([rx (byte-regexp (string->bytes/utf-8
                                (let ((sep (if (eq? (system-type) 'windows)
                                               ";"
                                               ":")))
                                  (format "([^~a]*)~a(.*)" sep sep))))])
          (set-box! rx:path-list-box rx)
          rx)))

  (-define (cons-path default s l) 
    (let ([s (if (eq? (system-type) 'windows)
                 (regexp-replace* #rx#"\"" s #"")
                 s)])
      (if (bytes=? s #"")
          (append default l)
          (cons (bytes->path s)
                l))))

  (-define (path-list-string->path-list s default)
    (unless (or (bytes? s)
                (string? s))
      (raise-argument-error 'path-list-string->path-list "(or/c bytes? string?)" s))
    (when (regexp-match? #rx#"\0" s)
      (let ([label (if (bytes? s) "byte string" "string")])
        (raise-arguments-error 'path-list-string->path-list
                               (format "given ~a contains a nul character" label)
                               label s)))
    (unless (and (list? default)
                 (andmap (lambda (p) (or (eq? p 'same) path?)) default))
      (raise-argument-error 'path-list-string->path-list "(listof path?)" default))
    (let loop ([s (if (string? s)
                      (string->bytes/utf-8 s)
                      s)])
      (let ([m (regexp-match (get-rx:path-list) s)])
        (if m
            (cons-path default (cadr m) (loop (caddr m)))
            (cons-path default s null))))))
