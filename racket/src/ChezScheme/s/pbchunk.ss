;; pbchunk conversion uses the fasl parser from "strip.ss"; it mutates
;; code in the parsed structure to generate references to C chunks
;; that implement a shadow version of a chunk of bytecode instructions,
;; and then the printer of "strip.ss" is used to write the updated
;; fasl content

(constant-case architecture
 [else #;(pb)

(let ()

(include "strip-types.ss")
  
(define-record-type chunk-info
  (fields (mutable counter)
          seen
          code-op)
  (nongenerative))

(define-record-type label
  (fields to min-from max-from all-from)
  (nongenerative))

(define (fasl-chunk! v code-op start-index seen-table)
  (let ([ci (make-chunk-info start-index
                             seen-table
                             code-op)])
    (chunk! v ci)
    (chunk-info-counter ci)))

(define (chunk! v ci)
  (unless (eq-hashtable-ref (chunk-info-seen ci) v #f)
    (eq-hashtable-set! (chunk-info-seen ci) v #t)
    (do-chunk! v ci)))

(define (chunk-vector! vec ci)
  (vector-for-each (lambda (e) (chunk! e ci)) vec))

(define (do-chunk! v ci)
  (fasl-case* v
    [(pair vec)
     (chunk-vector! vec ci)]
    [(tuple ty vec)
     (constant-case* ty
       [(fasl-type-box fasl-type-immutable-box)
        (chunk! (vector-ref vec 0) ci)]
       [(fasl-type-weak-pair)
        ($oops 'chunk "weak pair not supported")]
       [(fasl-type-ephemeron)
        ($oops 'chunk "ephemeron pair not supported")]
       [else (void)])]
    [(vector ty vec)
     (constant-case* ty
       [(fasl-type-vector fasl-type-immutable-vector)
        (chunk-vector! vec ci)]
       [else (void)])]
    [(stencil-vector mask vec)
     (chunk-vector! vec ci)]
    [(record maybe-uid size nflds rtd pad-ty* fld*)
     (for-each (lambda (fld)
                 (field-case fld [ptr (elem) (chunk! elem ci)] [else (void)]))
               fld*)]
    [(closure offset c)
     (chunk! c ci)]
    [(code flags free name arity-mask info pinfo* bytes m vreloc)
     (chunk-code! name bytes vreloc ci)
     (chunk-vector! vreloc ci)]
    [(reloc type-etc code-offset item-offset elem)
     (chunk! elem ci)]
    [(symbol-hashtable mutable? minlen subtype veclen vpfasl)
     (vector-for-each (lambda (p)
                        (chunk! (car p) ci)
                        (chunk! (cdr p) ci))
                      vpfasl)]
    [(indirect g i) (chunk! (vector-ref g i) ci)]
    [else
     ;; nothing else contains references that can reach code
     (void)]))

(define min-chunk-len 3)
(define instr-bytes 4)
(define reloc-instrs 4)

(define (instr-op instr) (bitwise-and instr #xFF))

(define (instr-d-dest instr) (bitwise-and (bitwise-arithmetic-shift-right instr 8) #xF))

(define (instr-dr-dest instr) (instr-d-dest instr))
(define (instr-dr-reg instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-di-dest instr) (instr-d-dest instr))
(define (instr-di-imm instr) (bitwise-arithmetic-shift-right instr 16))
(define (instr-di-imm/unsigned instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xFFFFFF))

(define (instr-adr-dest instr) (instr-di-dest instr))
(define (instr-adr-imm instr) (bitwise-arithmetic-shift-right instr 12))

(define (instr-drr-dest instr) (instr-d-dest instr))
(define (instr-drr-reg1 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-drr-reg2 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-dri-dest instr) (instr-d-dest instr))
(define (instr-dri-reg1 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-dri-imm instr) (bitwise-arithmetic-shift-right instr 16))

(define (instr-i-imm instr) (bitwise-arithmetic-shift-right instr 8))

(define (make-chunk-instr index) (bitwise-ior (constant pb-chunk)
                                              (bitwise-arithmetic-shift-left index 8)))

(define-syntax (instruction-case stx)
  (syntax-case stx ()
    [(_ instr emit [op . shape] ...)
     #'(constant-case*
        (instr-op instr)
        [(op) (emit op . shape)]
        ...
        [else ($oops 'chunk "unrecognized instruction ~s" instr)])]))

(define-syntax (instruction-cases stx)
  (syntax-case stx ()
    [(_ instr emit)
     #'(instruction-case
        instr emit
        [pb-mov16-pb-zero-bits-pb-shift0 di/u]
        [pb-mov16-pb-zero-bits-pb-shift1 di/u]
        [pb-mov16-pb-zero-bits-pb-shift2 di/u]
        [pb-mov16-pb-zero-bits-pb-shift3 di/u]
        [pb-mov16-pb-keep-bits-pb-shift0 di/u]
        [pb-mov16-pb-keep-bits-pb-shift1 di/u]
        [pb-mov16-pb-keep-bits-pb-shift2 di/u]
        [pb-mov16-pb-keep-bits-pb-shift3 di/u]
        [pb-mov-pb-i->i dr]
        [pb-mov-pb-d->d dr]
        [pb-mov-pb-i->d dr]
        [pb-mov-pb-d->i dr]
        [pb-mov-pb-s->d dr]
        [pb-mov-pb-d->s dr]
        [pb-mov-pb-d->s->d dr]
        [pb-mov-pb-i-bits->d-bits dr]
        [pb-mov-pb-d-bits->i-bits dr]
        [pb-mov-pb-i-i-bits->d-bits dr]
        [pb-mov-pb-d-lo-bits->i-bits dr]
        [pb-mov-pb-d-hi-bits->i-bits dr]
        [pb-bin-op-pb-no-signal-pb-add-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-add-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-sub-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-sub-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-mul-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-mul-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-div-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-div-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-and-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-and-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-ior-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-ior-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-xor-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-xor-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-asr-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-asr-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-add-pb-register drr]
        [pb-bin-op-pb-signal-pb-add-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-sub-pb-register drr]
        [pb-bin-op-pb-signal-pb-sub-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-mul-pb-register drr]
        [pb-bin-op-pb-signal-pb-mul-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-subz-pb-register drr]
        [pb-bin-op-pb-signal-pb-subz-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-subp-pb-register drr]
        [pb-bin-op-pb-signal-pb-subp-pb-immediate dri]
        [pb-cmp-op-pb-eq-pb-register dr]
        [pb-cmp-op-pb-eq-pb-immediate di]
        [pb-cmp-op-pb-lt-pb-register dr]
        [pb-cmp-op-pb-lt-pb-immediate di]
        [pb-cmp-op-pb-gt-pb-register dr]
        [pb-cmp-op-pb-gt-pb-immediate di]
        [pb-cmp-op-pb-le-pb-register dr]
        [pb-cmp-op-pb-le-pb-immediate di]
        [pb-cmp-op-pb-ge-pb-register dr]
        [pb-cmp-op-pb-ge-pb-immediate di]
        [pb-cmp-op-pb-ab-pb-register dr]
        [pb-cmp-op-pb-ab-pb-immediate di]
        [pb-cmp-op-pb-bl-pb-register dr]
        [pb-cmp-op-pb-bl-pb-immediate di]
        [pb-cmp-op-pb-cs-pb-register dr]
        [pb-cmp-op-pb-cs-pb-immediate di]
        [pb-cmp-op-pb-cc-pb-register dr]
        [pb-cmp-op-pb-cc-pb-immediate di]
        [pb-fp-bin-op-pb-add-pb-register drr]
        [pb-fp-bin-op-pb-sub-pb-register drr]
        [pb-fp-bin-op-pb-mul-pb-register drr]
        [pb-fp-bin-op-pb-div-pb-register drr]
        [pb-un-op-pb-not-pb-register dr]
        [pb-un-op-pb-not-pb-immediate di]
        [pb-fp-un-op-pb-sqrt-pb-register dr]
        [pb-fp-cmp-op-pb-eq-pb-register dr]
        [pb-fp-cmp-op-pb-lt-pb-register dr]
        [pb-fp-cmp-op-pb-le-pb-register dr]
        [pb-rev-op-pb-int16-pb-register dr]
        [pb-rev-op-pb-uint16-pb-register dr]
        [pb-rev-op-pb-int32-pb-register dr]
        [pb-rev-op-pb-uint32-pb-register dr]
        [pb-rev-op-pb-int64-pb-register dr]
        [pb-ld-op-pb-int8-pb-register drr]
        [pb-ld-op-pb-int8-pb-immediate dri]
        [pb-ld-op-pb-uint8-pb-register drr]
        [pb-ld-op-pb-uint8-pb-immediate dri]
        [pb-ld-op-pb-int16-pb-register drr]
        [pb-ld-op-pb-int16-pb-immediate dri]
        [pb-ld-op-pb-uint16-pb-register drr]
        [pb-ld-op-pb-uint16-pb-immediate dri]
        [pb-ld-op-pb-int32-pb-register drr]
        [pb-ld-op-pb-int32-pb-immediate dri]
        [pb-ld-op-pb-uint32-pb-register drr]
        [pb-ld-op-pb-uint32-pb-immediate dri]
        [pb-ld-op-pb-int64-pb-register drr]
        [pb-ld-op-pb-int64-pb-immediate dri]
        [pb-ld-op-pb-double-pb-register drr]
        [pb-ld-op-pb-double-pb-immediate dri]
        [pb-ld-op-pb-single-pb-register drr]
        [pb-ld-op-pb-single-pb-immediate dri]
        [pb-st-op-pb-int8-pb-register drr]
        [pb-st-op-pb-int8-pb-immediate dri]
        [pb-st-op-pb-int16-pb-register drr]
        [pb-st-op-pb-int16-pb-immediate dri]
        [pb-st-op-pb-int32-pb-register drr]
        [pb-st-op-pb-int32-pb-immediate dri]
        [pb-st-op-pb-int64-pb-register drr]
        [pb-st-op-pb-int64-pb-immediate dri]
        [pb-st-op-pb-double-pb-register drr]
        [pb-st-op-pb-double-pb-immediate dri]
        [pb-st-op-pb-single-pb-register drr]
        [pb-st-op-pb-single-pb-immediate dri]
        [pb-b-op-pb-fals-pb-register r/b "if (!flag) "]
        [pb-b-op-pb-fals-pb-immediate i/b "if (!flag) "]
        [pb-b-op-pb-true-pb-register r/b  "if (flag) "]
        [pb-b-op-pb-true-pb-immediate i/b "if (flag) "]
        [pb-b-op-pb-always-pb-register r/b ""]
        [pb-b-op-pb-always-pb-immediate i/b ""]
        [pb-b*-op-pb-register dr/b]
        [pb-b*-op-pb-immediate di/b]
        [pb-return n/x]
        [pb-adr adr]
        [pb-interp r/x]
        [pb-call dri/x]
        [pb-inc-pb-register dr]
        [pb-inc-pb-immediate di]
        [pb-lock r]
        [pb-cas drr]
        [pb-fence-pb-fence-store-store n]
        [pb-fence-pb-fence-acquire n]
        [pb-fence-pb-fence-release n]
        [pb-call-arena-in n]
        [pb-fp-call-arena-in n]
        [pb-call-arena-out n]
        [pb-fp-call-arena-out n]
        [pb-stack-call dr])]))

(define (advance-relocs relocs i)
  (let loop ([relocs relocs])
    (cond
      [(null? relocs) '()]
      [(fx> (car relocs) i) relocs]
      [else (loop (cdr relocs))])))

(define (sort-and-combine-labels labels)
  (let ([labels (sort (lambda (a b) (< (label-to a) (label-to b))) labels)])
    (let remove-dups ([labels labels])
      (cond
        [(null? labels) '()]
        [(null? (cdr labels)) labels]
        [else
         (let ([a (car labels)]
               [b (cadr labels)])
           (if (fx= (label-to a) (label-to b))
               (remove-dups (cons (make-label (label-to a)
                                              (fxmin (label-min-from a)
                                                     (label-min-from b))
                                              (fxmax (label-max-from a)
                                                     (label-max-from b))
                                              (append (label-all-from a)
                                                      (label-all-from b)))
                                  (cddr labels)))
               (cons a (remove-dups (cdr labels)))))]))))

(define (chunk-code! name bv vreloc ci)
  (let ([len (bytevector-length bv)]
        [o (chunk-info-code-op ci)]
        [relocs (let loop ([off 0] [rels (vector->list vreloc)])
                  (cond
                    [(null? rels) '()]
                    [else
                     (fasl-case* (car rels)
                       [(reloc type-etc code-offset item-offset elem)
                        (let ([off (+ off code-offset)])
                          (cons (fx- off (constant code-data-disp))
                                (loop off (cdr rels))))]
                       [else '()])]))])
    (fprintf o "\n/* code ~a */\n" (extract-name name))
    (let-values ([(headers labels) (gather-targets bv len)])
      (let loop ([i 0] [relocs relocs] [headers headers] [labels labels] [index (chunk-info-counter ci)])
        (cond
          [(fx= i len)
           (chunk-info-counter-set! ci index)]
          [else
           (let-values ([(start-i end-i) (select-instruction-range bv i len relocs headers labels)])
             (when (fx= i end-i)
               ($oops 'chunk-code "failed to make progress at ~a out of ~a" i len))
             (let ([start-i (if (fx< (fx- end-i start-i)
                                     (fx* min-chunk-len instr-bytes))
                                ;; the chunk would be too small to save us any time, so don't bother;
                                ;; a threshold greater than 1 also avoids code that wouldn't even
                                ;; use `tc` or `code`:
                                end-i
                                start-i)])
               (let-values ([(index relocs headers labels) (emit-chunk! o bv i relocs headers labels start-i end-i index)])
                 (unless (fx= start-i end-i)
                   (bytevector-u32-set! bv start-i (make-chunk-instr (fx- index 1)) (endianness little)))
                 (loop end-i relocs headers labels index))))])))))

(define (gather-targets bv len)
  (let loop ([i 0] [headers '()] [labels '()])
    (cond
      [(fx= i len) (values '() (sort-and-combine-labels labels))]
      [(and (pair? headers)
            (fx= i (caar headers)))
       (let ([size (cdar headers)])
         (let ([i (+ i size)])
           (let-values ([(rest-headers labels) (loop i (cdr headers) labels)])
             (values (cons (car headers) rest-headers)
                     labels))))]
      [else
       (let ([instr (bytevector-s32-ref bv i (endianness little))]
             [uinstr (bytevector-u32-ref bv i (endianness little))])
         (define (next)
           (loop (fx+ i instr-bytes) headers labels))

         (define (next/add-label new-label)
           (loop (fx+ i instr-bytes) headers (cons new-label labels)))

         (define (next/adr delta)
           (cond
             [(> delta 0)
              (let* ([after (fx+ i instr-bytes delta)]
                     [size (if (fx= 1 (fxand 1 (bytevector-u8-ref bv (fx- after 8))))
                               (constant size-rp-compact-header)
                               (constant size-rp-header))]
                     [start (fx- after size)]
                     [header (cons start size)])
                (loop (fx+ i instr-bytes)
                      ;; insert keeping headers sorted
                      (let loop ([headers headers])
                        (cond
                          [(null? headers) (list header)]
                          [(fx<= start (car headers)) (cons header headers)]
                          [else (cons (car headers) (loop (cdr headers)))]))
                      labels))]
             [else (next)]))

         (define-syntax (dispatch stx)
           (syntax-case stx (i/b adr)
             [(_ op i/b test)
              #'(let* ([delta (instr-i-imm instr)]
                       [target-label (fx+ i instr-bytes delta)])
                  (next/add-label (make-label target-label i i (list i))))]
             [(_ op adr)
              #'(let ([delta (fx* instr-bytes (instr-adr-imm instr))])
                  (next/adr delta))]
             [else #'(next)]))

         (instruction-cases instr dispatch))])))

(define (select-instruction-range bv i len relocs headers labels)
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels] [start-i #f])
    (cond
      [(fx= i len) (values (or start-i i) i)]
      [(and (pair? headers)
            (fx= i (caar headers)))
       (cond
         [start-i
          ;; we want to start  new chunk after the header, so end this one
          (values start-i i)]
         [else
          (let* ([size (cdar headers)]
                 [i (+ i size)])
            (loop i
                  (advance-relocs relocs i)
                  (cdr headers)
                  labels
                  start-i))])]
      [(and (pair? labels)
            (fx= i (label-to (car labels))))
       ;; we want to stop at this label if it's a target outside the range
       ;; that we're trying to build
       (cond
         [(< (label-min-from (car labels)) (or start-i i))
          ;; target from jump before this chunk
          (if start-i
              (values start-i i)
              (loop i relocs headers (cdr labels) #f))]
         [(< (label-max-from (car labels)) i)
          ;; always a forward jump within this chunk
          (loop i relocs headers (cdr labels) start-i)]
         [else
          ;; some backward jump exists, but tenatively assume that
          ;; it's within the chunk, then check; THIS MAKES OVERALL
          ;; CHUNKING NOT LINEAR-TIME, but it's probably ok in
          ;; practice
          (let-values ([(maybe-start-i end-i) (loop i relocs headers (cdr labels) start-i)])
            (cond
              [(fx>= maybe-start-i i)
               ;; chunk here or starts later, anyway
               (values maybe-start-i end-i)]
              [(fx< (label-max-from (car labels)) end-i)
               ;; backward jumps stay within chunk
               (values maybe-start-i end-i)]
              [else
               ;; not within chunk
               (values start-i i)]))])]
      [(and (pair? relocs)
            (fx= i (car relocs)))
       ;; can't start a chunk at a relocation, since the relocation
       ;; bytecode can't be rewritten (so don't set `start-i`), but 
       ;; can continue through a relocation load
       (loop (fx+ i (fx* reloc-instrs instr-bytes)) (cdr relocs) headers labels start-i)]
      [else
       ;; if the instruction always has to trampoline back, then the instruction
       ;; after can start a chunk to resume
       (let ([instr (bytevector-s32-ref bv i (endianness little))])
         (define (keep)
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i)))
         (define (stop-before)
           (if start-i
               (values start-i i)
               (loop (fx+ i instr-bytes) relocs headers labels #f)))
         (define (stop-after)
           (values (or start-i i) (fx+ i instr-bytes)))
         (define-syntax (dispatch stx)
           (syntax-case stx (dri/x r/x n/x r/b dr/b di/b)
             [(_ op dri/x) #'(stop-before)]
             [(_ op r/x) #'(stop-before)]
             [(_ op n/x) #'(stop-before)]
             [(_ r/b "") #'(stop-after)]
             [(_ op dr/b) #'(stop-after)]
             [(_ op di/b) #'(stop-after)]
             [_ #'(keep)]))
         (instruction-cases instr dispatch))])))

;; just show decoded instructions from `i` until `start-i`, then
;; generate a chunk function from `start-i` to `end-i`
(define (emit-chunk! o bv i relocs headers labels start-i end-i index)
  #;
  (fprintf o "/* 0x~x: 0x~x - 0x~x~a */\n" i start-i end-i
           (if (pair? labels)
               (format "; next label 0x~x" (label-to (car labels)))
               ""))
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels] [started? #f])
    (let ([old-started? started?]
          [started? (or started?
                        (and (fx= i start-i)
                             (not (fx= start-i end-i))))])
      (define (maybe-emit-label)
        (when (and started?
                   (fx< i end-i))
          (let ([a (car labels)])
            (when (ormap (lambda (from) (< start-i from end-i))
                         (label-all-from a))
              (fprintf o "label_~x:\n" i)))))
      (when (and started? (not old-started?))
        (fprintf o "static uptr chunk_~a(ptr tc, uptr ip) { /* at code+0x~x~a */\n"
                 index
                 i
                 (apply string-append
                        (let loop ([from (if (and (pair? labels)
                                                  (fx= i (label-to (car labels))))
                                             (label-all-from (car labels))
                                             '())])
                          (cond
                            [(null? from) '()]
                            [(fx< start-i (car from) end-i) (loop (cdr from))]
                            [else (cons (format ", from 0x~x" (car from))
                                        (loop (cdr from)))])))))
      (cond
        [(and (pair? headers)
              (fx= i (caar headers)))
         (cond
           [(fx>= i start-i)
            (unless (fx= i end-i) ($oops 'emit-chunk "should have ended at header ~a/~a" i end-i))
            (when started?
              (fprintf o "}\n"))
            (values (if started? (fx+ index 1) index)
                    (advance-relocs relocs i)
                    headers
                    labels)]
           [else
            (let ([size (cdar headers)])
              (fprintf o "/* data: ~a bytes */\n" size)
              (let ([i (fx+ i size)])
                (loop i
                      (advance-relocs relocs i)
                      (cdr headers)
                      labels
                      started?)))])]
        [(fx= i end-i)
         (when (and (pair? labels)
                    (fx= i (label-to (car labels))))
           (maybe-emit-label))
         (when started?
           (fprintf o "  return ip+code_rel(0x~x, 0x~x);\n}\n" start-i i))
         (values (if started? (fx+ 1 index) index) relocs headers labels)]
        [(and (pair? labels)
              (fx= i (label-to (car labels))))
         (maybe-emit-label)
         (loop i relocs headers (cdr labels) started?)]
        [else
         (let ([instr (bytevector-s32-ref bv i (endianness little))]
               [uinstr (bytevector-u32-ref bv i (endianness little))])
           (define (next)
             (loop (fx+ i instr-bytes) relocs headers labels started?))

           (define (done)
             (next))

           (define (pre)
             (if (>= i start-i) "  " "/* "))
           (define (post)
             (if (>= i start-i) " " " */ "))

           (define (emit-do _op)
             (fprintf o "~ado_~a(0x~x);~a" (pre) _op uinstr (post)))

           (define (emit-return)
             (fprintf o "~areturn ip+code_rel(0x~x, 0x~x);~a" (pre) start-i i (post)))

           (define-syntax (emit stx)
             (with-syntax ([_op (syntax-case stx ()
                                  [(_ op . _)
                                   (datum->syntax #'op
                                                  (list->string
                                                   (fold-right (lambda (x rest) 
                                                                 (case x
                                                                   [(#\-) (cons #\_ rest)]
                                                                   [(#\>) rest]
                                                                   [(#\*) (cons #\s rest)]
                                                                   [else (cons x rest)]))
                                                               '()
                                                               (string->list (symbol->string (syntax->datum #'op))))))])])
               (syntax-case stx (di di/u dr drr dri dri/x r r/x i r/b i/b dr/b di/b n n/x adr)
                 [(_ op di)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* r~a <- 0x~x */\n"
                               (instr-di-dest instr)
                               (instr-di-imm instr))
                      (next))]
                 [(_ op di/u)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* r~a <- 0x~x */\n"
                               (instr-di-dest instr)
                               (instr-di-imm/unsigned instr))
                      (next))]
                 [(_ op dr)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o " /* r~a <- r~a */\n"
                               (instr-dr-dest instr)
                               (instr-dr-reg instr))
                      (next))]
                 [(_ op drr)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* r~a <- r~a, r~a */\n"
                               (instr-drr-dest instr)
                               (instr-drr-reg1 instr)
                               (instr-drr-reg2 instr))
                      (next))]
                 [(_ op dri)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* r~a <- r~a, 0x~x */\n"
                               (instr-dri-dest instr)
                               (instr-dri-reg1 instr)
                               (instr-dri-imm instr))
                      (next))]
                 [(_ op dri/x)
                  #'(begin
                      (emit-return)
                      (fprintf o "/* ~a: r~a <- r~a, 0x~x */\n"
                               '_op
                               (instr-dri-dest instr)
                               (instr-dri-reg1 instr)
                               (instr-dri-imm instr))
                      (done))]
                 [(_ op r)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* ~a */\n"
                               (instr-dr-reg instr))
                      (next))]
                 [(_ op r/x)
                  #'(begin
                      (emit-return)
                      (fprintf o "/* ~a: ~a */\n"
                               '_op
                               (instr-dr-reg instr))
                      (done))]
                 [(_ op i)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "/* 0x~x */\n"
                               (instr-i-imm instr))
                      (next))]
                 [(_ op r/b test)
                  #'(begin
                      (fprintf o "~a~areturn regs[~a];~a/* ~a */\n"
                               (pre)
                               test
                               (instr-dr-reg instr)
                               (post)
                               '_op)
                      (if (equal? test "")
                          (done)
                          (next)))]
                 [(_ op i/b test)
                  #'(let* ([delta (instr-i-imm instr)]
                           [target-label (fx+ i instr-bytes delta)])
                      (cond
                        [(and (fx>= target-label start-i)
                              (fx< target-label end-i))
                         (fprintf o "~a~agoto label_~x;~a/* ~a: 0x~x */\n"
                                  (pre)
                                  test
                                  target-label
                                  (post)
                                  '_op
                                  delta)
                         (next)]
                        [else
                         (fprintf o "~a~areturn ip+code_rel(0x~x, 0x~x);~a/* ~a: 0x~x */\n"
                                  (pre)
                                  test
                                  start-i
                                  target-label
                                  (post)
                                  '_op
                                  delta)
                         (if (equal? test "")
                             (done)
                             (next))]))]
                 [(_ op dr/b)
                  #'(begin
                      (fprintf o "~areturn ~a_addr(0x~x);~a/* r~a + r~a */\n"
                               (pre)
                               '_op
                               uinstr
                               (post)
                               (instr-dr-dest instr)
                               (instr-dr-reg instr))
                      (done))]
                 [(_ op di/b)
                  #'(let* ([delta (instr-i-imm instr)]
                           [target-label (fx+ i instr-bytes delta)])
                      (fprintf o "~areturn ~a_addr(0x~x);~a/* r~a + 0x~x */\n"
                               (pre)
                               '_op
                               uinstr
                               (post)
                               (instr-di-dest instr)
                               (instr-di-imm instr))
                      (done))]
                 [(_ op n)
                  #'(begin
                      (emit-do '_op)
                      (fprintf o "\n")
                      (next))]
                 [(_ op n/x)
                  #'(begin
                      (emit-return)
                      (fprintf o "/* ~a */\n" '_op)
                      (done))]
                 [(_ op adr)
                  #'(let ([delta (fx+ i instr-bytes (fx* instr-bytes (instr-adr-imm instr)))])
                      (fprintf o "~aload_code_relative(~a, ip+code_rel(0x~x, ~a));~a\n"
                               (pre)
                               (instr-adr-dest instr)
                               start-i
                               (if (fx< delta 0)
                                   (format "-0x~x" (fx- delta))
                                   (format "0x~x" delta))
                               (post))
                      (next))])))

           (cond
             [(and (pair? relocs)
                   (= i (car relocs)))
              (let ([dest (instr-di-dest instr)])
                (fprintf o "~aload_from_relocation(~a, ip+code_rel(0x~x, 0x~x));~a\n"
                         (pre)
                         dest
                         start-i
                         i
                         (post)))
              (loop (fx+ i (fx* reloc-instrs instr-bytes)) (cdr relocs) headers labels started?)]
             [else
              (instruction-cases instr emit)]))]))))

(define (extract-name name)
  (fasl-case* name
    [(string ty string) string]
    [(indirect g i) (extract-name (vector-ref g i))]
    [else "???"]))

(set! $fasl-chunk! fasl-chunk!)

)]
 #;
 [else
  (set-who! $fasl-chunk!
    (lambda args
      ($oops 'pbchunk-convert-file "not supported for this machine type")))])
