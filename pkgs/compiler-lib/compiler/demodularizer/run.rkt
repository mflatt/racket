#lang racket/base

(provide (struct-out run))

(struct run (path/submod phase linkl uses stx-vec self-mpi))
