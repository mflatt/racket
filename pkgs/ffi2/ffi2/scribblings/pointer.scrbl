#lang scribble/manual
@(require (for-label ffi2)
          "common.rkt")

@title[#:tag "pointer"]{Foreign Pointer Operators}

A @deftech{pointer} object encapulates a memory address and a
potentially empty sequence of symbolic tags. A pointer object may
refer to an address that under the control of Racket's memory manager
and garbage collection, or it may refer to an address is managed
externally.

The base type for pointers is @racket[void_t*], and new pointer types
can be created via @racket[define-ffi-type] with either an existing
pointer type, a @racket[struct] type, a @racket[union] type, or an a
@racket[array] type.

A pointer's tags are intended to enable sanity checking that the right
kind of pointer is provided to an operation. If an operation expects a
pointer with a certain sequence of tags, it accepts a pointer with
additional tags added to the end. A @racket[void_t*] representation
has no tags, which means that a C conversion to Racket via
@racket[void_t*] is not accepted by any context that expects some tag,
whereas a pointer with any tags is accepted as a @racket[void_t*]
representation to translate to C. Whether a pointer object represents
an address managed by Racket's garbage collector is independent of its
tags.

@defform[(ffi2-malloc maybe-mode
                      maybe-type
                      n-expr maybe-bytes
                      maybe-as-type)
         #:grammar ([maybe-mode #:gcable
                                #:gcable-traced
                                #:gcable-immobile
                                #:gcable-traced-immobile
                                #:manual
                                ϵ]
                    [maybe-type type
                                ϵ]
                    [maybe-bytes #:bytes
                                 ϵ]
                    [maybe-as-type (code:line #:as as-type)
                                   ϵ])]{

Allocates memory.

The amount of allocated memory depends on @racket[n-expr]. If no type
is provided as @racket[maybe-type], or if @racket[maybe-bytes] is
@racket[#:bytes], then the allocated memory spans as many bytes as the
value of @racket[n-expr]. Otherwise, the allocated memory's size is
the result of @racket[(* n-expr (ffi2-sizeof maybe-type))].

If @racket[maybe-as] is specified, then the result is cast to
@racket[as-type] in the sense of @racket[ffi2-cast]. Otherwise, the
result is a @tech{pointer} object encapsulating the address of
allocated memory. If @racket[maybe-type] is a @racket[struct],
@racket[union], or size @racket[array] type (and @racket[maybe-as] is
not specified), the result is a pointer object using the
representation of @racket[maybe-type]. Otherwise, the result is a
pointer object using the representation of @racket[void_t*].

By default, allocation uses @racket[#:gcable] mode, but a
@racket[maybe-mode] specificaiton can pick any of the supported modes:

@itemlist[

 @item{@racket[#:gcable]: Allocates in Racket's garbage-collected
       space. The allocated memory becomes eligible for garbage
       collection when it is not referenced by any reachable pointer
       object or @tech{traced} allocated memory. Even before
       collection, the meory manager may relocate the object, but
       garbage collection or relocation cannot happen with a
       foreign-procedure call is active.}

 @item{@racket[#:gcable-immobile]: Like @racket[#:gcable], but the
       allocated memory will not be relocated by a different address
       by the memory manager as long as it is not collected.}

 @item{@racket[#:gcable-traced]: Like @racket[#:gcable], but the
       allocated memory is @deftech{traced}, meaning that it can
       itself contain references to other allocated memory. The
       references are updated by the memory manager if it moved the
       referenced objects.}

 @item{@racket[#:gcable-traced-immobile]: Like
       @racket[#:gcable-traced], but the allocated memory will not be
       relocated by a different address by the memory manager as long
       as it is not collected.}

 @item{@racket[#:gcable]: Allocates outside of Racket's
       garbage-collected space. The allocated memory is never
       relocated by the garbage collection, and it must be freed
       explicitly with @racket[ffi2-free].}

]

}

@defform[(ffi2-cast expr option ...)
         #:grammar ([option (code:line #:from from-type)
                            (code:line #:to to-type)
                            (code:line #:offset maybe-type n-expr)]
                    [maybe-type type
                                ϵ])]{

Converts the Racket representation produced by @racket[expr] from one
foreign type's representation to another. If @racket[from-type] or
@racket[to-type] is not specified, each defaults to @racket[void_t*].

}
