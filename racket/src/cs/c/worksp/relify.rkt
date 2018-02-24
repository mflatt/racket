#lang racket/base
(require racket/cmdline)

(define-values (rel-path path)
  (command-line
   #:args
   (rel-path path)
   (values rel-path path)))

(displayln
 (if (and (relative-path? path)
	  (file-exists? path))
     (build-path rel-path path)
     path))

