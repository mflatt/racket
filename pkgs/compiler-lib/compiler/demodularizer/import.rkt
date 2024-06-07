#lang racket/base

(provide (struct-out import))

(struct import (name phase shape int-name [pos #:mutable]))
