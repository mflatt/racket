
(include "rumble/hamt.ss")

;; or
;; (include "rumble/patricia.ss")
;; which is a much simpler and prettier implementation that runs
;; about as fast, but uses more memory

;; or
;; (include "rumble/hamt-vector.ss")
;; which was the starting point for "hamt.ss" and uses plain vectors
;; instead of stencil vectors; it runs about as fast, too, but also
;; uses more memory
