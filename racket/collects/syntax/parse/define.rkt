#lang racket/base
(require (except-in syntax/parse/pre/define
                    static)
         (for-syntax syntax/parse))
(provide (all-from-out syntax/parse/pre/define)
         (for-syntax (all-from-out syntax/parse)))
