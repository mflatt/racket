
;; We have several implementations of immutable hash tables. Pick one...

(include "rumble/hamt-stencil.ss")
;;
;; The HAMT implementaiton using stencil vectors tends to use the last
;; memory, often by a lot. It's a little slow than the others, though.

;; (include "rumble/patricia.ss")
;;
;; The Patricia-trie implementation is the prettiest and fastest. It
;; uses the most memory, though --- typically much more than the
;; vector-stencil HAMT.

;; (include "rumble/hamt-vector.ss")
;;
;; This HAMT implementaiton uses plain vectors instead of stencil
;; vectors. Its speed and memory use are intermediate, but its speed
;; is closer to the stencil-vector HAMT implementation, and memory use
;; is closer to the Patrica trie implementation.
