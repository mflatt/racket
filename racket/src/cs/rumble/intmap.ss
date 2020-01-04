
(include "rumble/hamt.ss")
;; or
;; (include "rumble/patricia.ss")

#|
(define-values (h:intmap?
                h:empty-hash
                h:empty-hasheqv
                h:empty-hasheq
                h:make-intmap-shell
                h:intmap-shell-sync!
                h:intmap-equal?
                h:intmap-eqv?
                h:intmap-eq?
                h:intmap-count
                h:intmap-empty?
                h:intmap-ref
                h:intmap-ref-key
                h:intmap-has-key?
                h:intmap-set
                h:intmap-remove
                h:intmap-iterate-first
                h:intmap-iterate-next
                h:intmap-iterate-pair
                h:intmap-iterate-key
                h:intmap-iterate-value
                h:intmap-iterate-key+value
                h:unsafe-intmap-iterate-first
                h:unsafe-intmap-iterate-next
                h:unsafe-intmap-iterate-pair
                h:unsafe-intmap-iterate-key
                h:unsafe-intmap-iterate-value
                h:unsafe-intmap-iterate-key+value
                h:intmap-for-each
                h:intmap-map
                h:intmap=?
                h:intmap-hash-code
                h:intmap-keys-subset?)
  (let ()
    (include "rumble/hamt.ss")
    (values intmap?
            empty-hash
            empty-hasheqv
            empty-hasheq
            make-intmap-shell
            intmap-shell-sync!
            intmap-equal?
            intmap-eqv?
            intmap-eq?
            intmap-count
            intmap-empty?
            intmap-ref
            intmap-ref-key
            intmap-has-key?
            intmap-set
            intmap-remove
            intmap-iterate-first
            intmap-iterate-next
            intmap-iterate-pair
            intmap-iterate-key
            intmap-iterate-value
            intmap-iterate-key+value
            unsafe-intmap-iterate-first
            unsafe-intmap-iterate-next
            unsafe-intmap-iterate-pair
            unsafe-intmap-iterate-key
            unsafe-intmap-iterate-value
            unsafe-intmap-iterate-key+value
            intmap-for-each
            intmap-map
            intmap=?
            intmap-hash-code
            intmap-keys-subset?)))

(define-values (p:intmap?
                p:empty-hash
                p:empty-hasheqv
                p:empty-hasheq
                p:make-intmap-shell
                p:intmap-shell-sync!
                p:intmap-equal?
                p:intmap-eqv?
                p:intmap-eq?
                p:intmap-count
                p:intmap-empty?
                p:intmap-ref
                p:intmap-ref-key
                p:intmap-has-key?
                p:intmap-set
                p:intmap-remove
                p:intmap-iterate-first
                p:intmap-iterate-next
                p:intmap-iterate-pair
                p:intmap-iterate-key
                p:intmap-iterate-value
                p:intmap-iterate-key+value
                p:unsafe-intmap-iterate-first
                p:unsafe-intmap-iterate-next
                p:unsafe-intmap-iterate-pair
                p:unsafe-intmap-iterate-key
                p:unsafe-intmap-iterate-value
                p:unsafe-intmap-iterate-key+value
                p:intmap-for-each
                p:intmap-map
                p:intmap=?
                p:intmap-hash-code
                p:intmap-keys-subset?)
  (let ()
    (include "rumble/patricia.ss")
    (values intmap?
            empty-hash
            empty-hasheqv
            empty-hasheq
            make-intmap-shell
            intmap-shell-sync!
            intmap-equal?
            intmap-eqv?
            intmap-eq?
            intmap-count
            intmap-empty?
            intmap-ref
            intmap-ref-key
            intmap-has-key?
            intmap-set
            intmap-remove
            intmap-iterate-first
            intmap-iterate-next
            intmap-iterate-pair
            intmap-iterate-key
            intmap-iterate-value
            intmap-iterate-key+value
            unsafe-intmap-iterate-first
            unsafe-intmap-iterate-next
            unsafe-intmap-iterate-pair
            unsafe-intmap-iterate-key
            unsafe-intmap-iterate-value
            unsafe-intmap-iterate-key+value
            intmap-for-each
            intmap-map
            intmap=?
            intmap-hash-code
            intmap-keys-subset?)))

(define-record-type intmap
  (fields h p))

(define empty-hash (make-intmap h:empty-hash p:empty-hash))
(define empty-hasheqv (make-intmap h:empty-hasheqv p:empty-hasheqv))
(define empty-hasheq (make-intmap h:empty-hasheq p:empty-hasheq))

(define (make-intmap-shell et)
  (make-intmap (h:make-intmap-shell et)
               (p:make-intmap-shell et)))

(define (intmap-shell-sync! dst src)
  (h:intmap-shell-sync! (intmap-h dst) (intmap-h src))
  (p:intmap-shell-sync! (intmap-p dst) (intmap-p src)))

(define-syntax define-same
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id t arg ...))
       (with-syntax ([h:id (datum->syntax #'id (#%string->symbol
                                                (string-append "h:" (#%symbol->string (syntax->datum #'id)))))]
                     [p:id (datum->syntax #'id (#%string->symbol
                                                (string-append "p:" (#%symbol->string (syntax->datum #'id)))))])
         #`(define (id t arg ...)
             (let ([h (h:id (intmap-h t) arg ...)]
                   [p (p:id (intmap-p t) arg ...)])
               (unless (eqv? h p)
                 (error 'id "different"))
               h)))])))

(define-syntax define-one
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id t arg ...))
       (with-syntax ([p:id (datum->syntax #'id (#%string->symbol
                                                (string-append "p:" (#%symbol->string (syntax->datum #'id)))))])
         #`(define (id t arg ...)
             (p:id (intmap-p t) arg ...)))])))

(define-syntax define-other
  (lambda (stx)
    (syntax-case stx ()
      [(_ (id t arg ...))
       (with-syntax ([h:id (datum->syntax #'id (#%string->symbol
                                                (string-append "h:" (#%symbol->string (syntax->datum #'id)))))])
         #`(define (id t arg ...)
             (h:id (intmap-h t) arg ...)))])))

(define-same (intmap-equal? t))
(define-same (intmap-eqv? t))
(define-same (intmap-eq? t))

(define-same (intmap-count t))

(define-same (intmap-empty? t))

(define-same (intmap-ref t key default))

(define-same (intmap-ref-key t key default))

(define-same (intmap-has-key? t key))

(define (intmap-set t key val)
  (make-intmap (h:intmap-set (intmap-h t) key val)
               (p:intmap-set (intmap-p t) key val)))

(define (intmap-remove t key)
  (make-intmap (h:intmap-remove (intmap-h t) key)
               (p:intmap-remove (intmap-p t) key)))

(define-one (intmap-iterate-first t))
(define-one (intmap-iterate-next t pos))
(define-one (intmap-iterate-pair t pos fail))
(define-one (intmap-iterate-key t pos fail))
(define-one (intmap-iterate-value t pos fail))
(define-one (intmap-iterate-key+value t pos fail))

(define-one (unsafe-intmap-iterate-first t))
(define-one (unsafe-intmap-iterate-next t pos))
(define-one (unsafe-intmap-iterate-pair t pos))
(define-one (unsafe-intmap-iterate-key t pos))
(define-one (unsafe-intmap-iterate-value t pos))
(define-one (unsafe-intmap-iterate-key+value t pos))

(define-other (intmap-for-each t proc))
(define-other (intmap-map t proc))

(define (intmap=? a b eql?)
  (let ([h (h:intmap=? (intmap-h a) (intmap-h b) eql?)]
        [p (p:intmap=? (intmap-p a) (intmap-p b) eql?)])
    (unless (eqv? h p)
      (error 'intmap=? "different"))
    h))

(define-other (intmap-hash-code t hash))

(define (intmap-keys-subset? a b)
  (let ([h (h:intmap-keys-subset? (intmap-h a) (intmap-h b))]
        [p (p:intmap-keys-subset? (intmap-p a) (intmap-p b))])
    (unless (eqv? h p) (error 'intmap-keys-subset? "different"))
    h))
|#
