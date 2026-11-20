#lang racket/base

;; Logging for module-instance sharing, so that a programmer can find
;; out why a module's phase level is not shared across namespaces.
;; Messages are at the 'debug level with the 'module-sharing topic,
;; e.g., use `PLTSTDERR="debug@module-sharing"`.

(provide log-sharing-enabled?
         log-sharing)

(define (log-sharing-enabled?)
  (log-level? (current-logger) 'debug 'module-sharing))

;; Assumes that `log-sharing-enabled?` produced true
(define (log-sharing data fmt . args)
  (log-message (current-logger)
               'debug
               'module-sharing
               (apply format fmt args)
               data))
