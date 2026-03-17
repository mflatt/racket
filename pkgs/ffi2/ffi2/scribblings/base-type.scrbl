#lang scribble/manual
@(require (for-label ffi2)
          "common.rkt")

@(define-syntax-rule (deftypeform id)
   @defidform[#:kind "ffi2 type" id])

@title[#:tag "base-ffi2-type"]{Base Foreign Types}

@deftogether[(
@deftypeform[void_t*]
@deftypeform[void_t*/gcable]
)]{

A generic pointer. On the C side, both are represented as an address
with the same representation as @tt{void*}. On the Racket side, both
are represented as @deftech{pointer} objects}

When an address is converted from C to Racket, then @racket[void_t*]
produces a pointer object that references memory (assumed to be) not
managed by Racket's garbage collector. The @racket[void_t*/gcable]
type implies that a pointer converted from C should be treated as
(potentially) managed by Racket/s garbage collector. In both cases,
conversion from Racket to C allows any pointer object.

A pointer object has an optional sequence of symbolic tags, which are
intended to enable some amount of checking that the right kind of
pointer is provided to an operation. If an operation expects a pointer
with a certain sequence of tags, it accepts a pointer with additional
tags added to the end. A @racket[void_t*] representation has no
tags, which means that a C conversion to Racket via @racket[void_t*]
is not accepted by any context that expects some tag, whereas a
pointer with any tags is accepted as a @racket[void_t*] representation
to translate to C. Whether a pointer object represents an address
managed by Racket's garbage collector is independent of its tags.

}

@deftogether[(
@deftypeform[int8_t]
@deftypeform[uint8_t]
@deftypeform[int16_t]
@deftypeform[uint16_t]
@deftypeform[int32_t]
@deftypeform[uint32_t]
@deftypeform[int64_t]
@deftypeform[uint64_t]
)]{

Signed and unsigned integer @tech{scalar} types of specific bit widths
on the C side. All are represented as exact integers on the Racket
side, constrained to a range that fits in the unsigned or two's
complement bit representation.

}

@deftogether[(
@deftypeform[int_t]
@deftypeform[uint_t]
@deftypeform[long_t]
@deftypeform[ulong_t]
@deftypeform[intptr_t]
@deftypeform[uintptr_t]
@deftypeform[size_t]
@deftypeform[ssize_t]
)]{

Signed and unsigned integer @tech{scalar} types of platform-specific
bit widths. For consistently, a @litchar{_t} is added to the end of C
type names like @tt{int} to form a type name like @racket[int_t].

All are represented as exact integers on the Racket side, constrained
to a range that fits in the platform-specific C representation.

}

@deftogether[(
@deftypeform[float_t]
@deftypeform[double_t]
)]{

IEEE floating-point number @tech{scalar} types. On the C side, a
@racket[float_t] is 8 bytes, and a @racket[double_t] is 16 bytes. On
the Racket side, both are represented as @tech[#:doc
ref-doc]{flonums}.

}

@deftogether[(
@deftypeform[wchar_t]
@deftypeform[intwchar_t]
)]{

On the C side, both @racket[wchar_t] and @racket[intwchar_t] occupy
the same number of bytes. On the Racket side, a @racket[wchar_t] is
represented as a character, while a @racket[intwchar_t] is a
@tech{scalar} type that is represented as an exact integer that fits
into the platform-specific C representation.

The range of @racket[wchar_t] on the C side may include integers that
do not correspond to a Racket character, and it may omit values that
do correspond to a Racket character. The Racket representation of a
@racket[wchar_t] is constrained to characters that fit in the C
representation, and values from C that are are not representable as
Racket characters are converted to the Unicode replacement character,
@racket[#\uFFFD].

}

@deftogether[(
@deftypeform[bool_t]
@deftypeform[intbool_t]
)]{

Boolean @tech{scalar} types. On the C side, @racket[bool_t]
corresponds to the C @tt{bool} type from @tt{<stdbool.h>}, while
@racket[intbool_t] corresponds to @tt{int} (which is often used for a
boolean representation in C-based libraries). On the Racket side, both
are represented by boolean values when received from C, and and Racket
value is allowed when converting to C (where @racket[#f] is treated as
false and all other values are treated as true).

}

@deftogether[(
@deftypeform[void_t]
)]{

A type with no representation on the C side and a @racket[(void)]
representation on the Racket side. The @racket[void_t] type can only
be used for the result of a foreign procedure for foreign callback.

}

@deftogether[(
@deftypeform[string_t]
@deftypeform[bytes_t]
@deftypeform[bytes_ptr_t]
@deftypeform[path_t]
)]{

Types that are represented on the C side like @racket[void_t*], but
that are represented in Scheme by conversion to and from strings, byte
strings, and paths. The @racket[string_t] type converts a Racket
string to a null-terminated byte string and passes the address of the
start of the byte string. The @racket[bytes_t] type similarly copies a
racket byte string to add a null terminator, while
@racket[bytes_ptr_t] passes the start of a Racket byte string as-is,
without adding a terminator (and where mutation of pointer content on
the C side is reflected as changes to the byte string content). The
@racket[path_t] is like @racket[string_t], but for paths in the sense
of @racket[path-for-some-system?].

When converting from C to Racket, the pointer received from C is
treated as a reference to a null-terminated C string, and a fresh
Racket byte string is created to hold the content up to the null
terminator. The @racket[string_t] or @racket[path_t] types then
convert that byte string to a string or path, respectively.

}


@deftogether[(
@deftypeform[racket_t]
)]{

A type that is represented on the C side like @racket[void_t*], but on
the Racket side by an arbitrary value. this type will make sense only
when interacting with a foreign procedure that is specifically aware
of the Racket runtime system and cooperating with it.

}
