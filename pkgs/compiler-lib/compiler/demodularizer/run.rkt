#lang racket/base

(provide (struct-out run))

(struct run (path/submod phase linkl meta-linkl
                         uses         ; list of (cons path/submod phase-level)
                         import-uses  ; like `uses`, but a `path/submod` may be redirected to a supermodule
                         stx-vec stx-mpi
                         portal-stxes))
