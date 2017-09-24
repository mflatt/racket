#lang racket/base
(require "internal-error.rkt"
         (only-in '#%linklet primitive-table))

(provide make-engine
         engine-block
         engine-return
         current-process-milliseconds
         set-ctl-c-handler!
         root-continuation-prompt-tag
         break-enabled-key
         set-break-enabled-transition-hook!
         ;; Just `exn:break`, but the host may need
         ;; to distinguish breaks raised by the thread
         ;; implementation:
         exn:break/non-engine
         exn:break:hang-up/non-engine
         exn:break:terminate/non-engine)

(define-values (make-engine engine-block engine-return
                            current-process-milliseconds
                            set-ctl-c-handler!
                            root-continuation-prompt-tag
                            break-enabled-key
                            set-break-enabled-transition-hook!
                            exn:break/non-engine
                            exn:break:hang-up/non-engine
                            exn:break:terminate/non-engine)
  (let ([ht (primitive-table '#%engine)])
    (unless ht
      (internal-error "engines not provided by host"))
    (values
     (hash-ref ht 'make-engine)
     (hash-ref ht 'engine-block)
     (hash-ref ht 'engine-return)
     (hash-ref ht 'current-process-milliseconds)
     (hash-ref ht 'set-ctl-c-handler!)
     (hash-ref ht 'root-continuation-prompt-tag)
     (hash-ref ht 'break-enabled-key)
     (hash-ref ht 'set-break-enabled-transition-hook!)
     (hash-ref ht 'exn:break/non-engine)
     (hash-ref ht 'exn:break:hang-up/non-engine)
     (hash-ref ht 'exn:break:terminate/non-engine))))