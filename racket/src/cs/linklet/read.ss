
(define (read-compiled-linklet in)
  (performance-region
   'read-bundle
   (read-linklet-bundle-or-directory in
                                     vm-bytes
                                     hash->linklet-directory
                                     read-linklet-bundle-hash
                                     read-hash->linklet-bundle)))

(define read-linklet-bundle-or-directory
  (lambda (in
           vm-bytes
           hash->linklet-directory
           read-linklet-bundle-hash
           hash->linklet-bundle)
    (#%error 'read-linklet-bundle-or-directory "not ready")))
(define (set-generic-linklet-reader! proc)
  (set! read-linklet-bundle-or-directory proc))

(define (read-linklet-bundle-hash bstr)
  (fasl-read (open-bytevector-input-port bstr)))

(define (read-hash->linklet-bundle ht)
  (hash->linklet-bundle
   (adjust-linklet-bundle-laziness ht)))

(define read-on-demand-source
  (make-parameter #f
                  (lambda (v)
                    (unless (or (eq? v #t) (eq? v #f) (and (path? v)
                                                           (complete-path? v)))
                      (raise-argument-error 'read-on-demand-source
                                            "(or/c #f #t (and/c path? complete-path?))"
                                            v))
                    v)))

(define (adjust-linklet-bundle-laziness ht)
  (let loop ([i (hash-iterate-first ht)])
    (cond
     [(not i) (hasheq)]
     [else
      (let-values ([(key val) (hash-iterate-key+value ht i)])
        (hash-set (loop (hash-iterate-next ht i))
                  key
                  (if (linklet? val)
                      (adjust-linklet-laziness val)
                      val)))])))

(define (adjust-linklet-laziness linklet)
  (set-linklet-code linklet
                    (linklet-code linklet)
                    (if (|#%app| read-on-demand-source)
                        'faslable
                        'faslable-strict)))

