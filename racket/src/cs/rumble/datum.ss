(define-thread-local datums (make-weak-hashtable equal-hash-code equal?))

(define intern-regexp? #f)
(define (set-intern-regexp?! p) (set! intern-regexp? p))

(define (datum-intern-literal v)
  (when (current-future) (block-future))
  (cond
   [(or (number? v)
        (string? v)
        (char? v)
        (bytes? v)
        (intern-regexp? v))
    (with-interrupts-disabled*
     ;; getting hash code with interrupts disabled means that `v`
     ;; cannot be mutated meanwhile:
     (let* ([c (hashtable-cell datums v #f)]
            [v0 (car c)])
       (if (eq? v0 v)
           (let ([v (cond
                      [(string? v) (string->immutable-string v)]
                      [(bytes? v) (bytes->immutable-bytes v)]
                      [else v])])
             (unless (eq? v v0)
               (set-car! c v))
             v)
           v0)))]
   [else v]))
