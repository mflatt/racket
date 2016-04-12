#lang racket/base
(require setup/dirs
         racket/file
         compiler/embed
         launcher)

(provide installer
         addon-installer)

(define (installer path coll user?)
  (void))

(define (addon-installer path coll user?)
  (define dir (find-addon-console-bin-dir))
  (make-directory* dir)
  (define variants (available-racket-variants))
  (for ([v (in-list variants)])
    (parameterize ([current-launcher-variant v])
      (create-embedding-executable
       (racket-program-launcher-path "Racket" #:user? #t #:addon? #t)
       #:variant v
       #:cmdline (list "-X" (path->string (find-collects-dir))
                       "-G" (path->string (find-config-dir))
                       "-A" (path->string (find-system-path 'addon-dir)))
       #:launcher? #t
       #:aux `((relative? . #f))))))
