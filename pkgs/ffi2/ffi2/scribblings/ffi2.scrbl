#lang scribble/manual

@title{FFI2: Racket Foreign Interface}

@defmodule[ffi2]{The @racketmodname[ffi2] library is an alternative to
@racket[ffi/unsafe] for interacting with foreign libraries that use a
C-based API.}

@defform[(define-ffi2-type name parent_type
           option
           ...)
         #:grammar ([option (code:line #:tag tag)
                            (code:line #:predicate predicate-expr)
                            (code:line #:racket->c racket->c-expr)
                            (code:line #:c->racket c->racket-expr)
                            (code:line #:release release-expr)]
                    [tag identifier
                         #f])]{

Ok.
  
}
