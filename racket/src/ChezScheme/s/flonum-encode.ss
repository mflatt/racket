(define flonum-encode-immediate
  (lambda (d)
    (constant-case immediate-flonums
      [(#t)
       (let ([di (flbit-field d 0 64)])
         (cond
           [(member (logand di (constant immediate-flonum-drop-mask))
                    (list 0 (constant immediate-flonum-drop-mask)))
            (logor
             (logand (logor (bitwise-arithmetic-shift-left di (- (constant flonum-bits) (constant immediate-flonum-offset)))
                            (bitwise-arithmetic-shift-right di (- (constant immediate-flonum-lo-bits)
                                                                  (constant immediate-flonum-mask-bits))))
                     (- (sub1 (expt 2 (constant flonum-bits)))
                        (constant mask-immediate-flonum)))
             (constant type-immediate-flonum))]
           [else #f]))]
      [else #f])))
