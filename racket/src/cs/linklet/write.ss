(define expander:write-linklet-bundle
  (lambda (b
           vm-bytes
           linklet-bundle->bytes
           port)
    (#%error 'expander:write-linklet-bundle "not ready")))
(define expander:write-linklet-directory
  (lambda (ld
           linklet-directory->hash
           vm-bytes
           linklet-bundle->bytes
           port)
    (#%error 'expander:write-linklet-directory "not ready")))
(define (set-generic-linklet-writers! b-proc ld-proc)
  (set! expander:write-linklet-bundle b-proc)
  (set! expander:write-linklet-directory ld-proc))

(define (write-linklet-bundle b port mode)
  (expander:write-linklet-bundle b
                                 vm-bytes
                                 linklet-bundle->bytes
                                 port))

(define (write-linklet-directory ld port mode)
  (expander:write-linklet-directory ld
                                    linklet-directory->hash
                                    vm-bytes
                                    linklet-bundle->bytes
                                    port))

(define (linklet-bundle->bytes b)
  (let-values ([(o get) (open-bytevector-output-port)])
    (fasl-write* (linklet-bundle->hash b) o)
    (get)))
