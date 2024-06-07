#lang racket/base

(provide (struct-out import))

(struct import (name path/submod+phase src-ext-name))
